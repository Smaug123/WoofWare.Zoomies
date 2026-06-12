# Completing the Incremental Migration

**Status: implemented.** This document was the design for finishing the migration started on
the `incremental` branch, and is retained as the as-built rationale; every section below has
landed (see the commit history of this branch for the stage-by-stage implementation, which
followed `IMPLEMENTATION_PLAN.md`). Sections are written in the design's original tense, so
"current state" descriptions refer to the pre-migration code. It superseded
`event-loop-design.md`, which was written against an earlier revision of `App.fs`.

## Where we are

The branch moved the framework's types to the right shape (view as `'state Node -> Vdom Node`, time as a `Clock`, focus/bounds as `Var`s), but the engine is still an immediate-mode poll: `App.run` stabilizes every `frameDelayMs` milliseconds with a fabricated 1ns clock advance, and correctness is maintained by several change-tracking mechanisms running *alongside* the graph rather than through it:

1. Incremental's own stabilization/cutoff;
2. `VdomContext._IsDirty`, set in nine places;
3. `Object.referenceEquals` on the observed vdom;
4. `'state` equality comparisons around each batch;
5. focus before/after comparisons around render;
6. the activation dictionary plus a generation `Var` plus polled pruning.

The goal of this design: the graph becomes the single source of truth for "did anything change?", and the loop becomes event-driven so that an idle application does no work. Mechanisms 2 and 6 are deleted; 3-5 are retained only where they express something the graph genuinely cannot see.

## Verified upstream facts

These were checked against the local WoofWare.Incremental and WoofWare.TimingWheel repos and change the plan relative to `event-loop-design.md`:

- **`IClock.NextAlarmFiresAt : Clock -> TimeNs voption` already exists** in WoofWare.Incremental 0.1.19, which is published on NuGet. We depend on 0.1.18. The "expose the alarm time upstream" workstream is a dependency bump, not cross-repo development.
- **`AdvanceClock` is a silent no-op when the target time is not strictly greater than the clock's current time** (`State.fs`, `advanceClock`: `if to_ > Clock.now clock then ...`). The 1ns fabrication in `IncrementalState.advanceClockAndStabilize` is not needed for safety and never was.
- **TimingWheel advance cost is O(number of levels), not O(elapsed time)** (`PriorityQueue.increaseMinAllowedKey` walks at most the 3 levels of the default config). Creating the clock at the Unix epoch and jumping ~56 years on the first frame is harmless. No change needed here.
- **Alarms can only be scheduled up to ~52 days ahead** with the default config (alarm precision 2^20 ns ≈ 1.05ms, level bits `[14; 13; 5]`). Fine for UI purposes; noted as a constraint.
- **`Var.Set` during stabilization is legal** (queued, applied afterwards). `Var.Value` returns the value as of the last application; `Var.LatestValue` includes the queued value. All framework sets happen outside stabilization, where `Set` applies immediately.
- **`Expert1Node.makeStale` has no cross-thread guarantee** and carries a debug-mode assertion about the calling context. Another reason the Expert-node-based `StateMachine` should go.
- **No scheduler/async component exists upstream.** Whatever drives the loop, we build it.

## §1 The event-driven loop

### Wake sources

The loop must wake when, and only when, one of these occurs:

| Source | Mechanism today | Mechanism after |
|---|---|---|
| Keystroke | poll `Console.KeyAvailable` each frame | dedicated input thread does blocking `ReadKey`, enqueues, signals |
| Posted app event (`IWorldBridge.PostEvent`) | found on next poll | enqueue + signal |
| Subscription event (`SubscribeEvent`) | found on next poll | enqueue + signal |
| Terminal resize (SIGWINCH) | generation counter found on next poll | counter + signal |
| Clock alarm (spinners, activation expiry) | implicit: we stabilize every frame anyway | `Task.Delay` until `NextAlarmFiresAt` |
| WorldFreezer internal deadline (lone-Esc re-emit, paste-mode timeout) | implicit, same | `Task.Delay` until `WorldFreezer.NextDeadline` |
| Cancellation | checked each frame | `CancellationToken` in the wait |

### The loop

```
setup; stabilize; render
while not cancelled:
    deadline := min(NextAlarmFiresAt(clock), worldFreezer.NextDeadline())   // both optional
    waitForWork(wakeSignal, deadline, cancellationToken)                    // blocks
    advance clock to getUtcNow(); stabilize
    drain WorldFreezer changes; fold into state; stabilize
    render if observer changed or full redraw requested
    post-layout fixpoint (unchanged)
```

The key invariant (inherited from `event-loop-design.md` and still correct): **the deadline is computed after the final stabilization of the iteration, before sleeping.** Alarms are only created during node construction, node construction only happens during setup or inside `Bind` during stabilization, therefore querying after stabilization sees every alarm.

`waitForWork` races a `TaskCompletionSource` (the wake signal, recreated after each consumption), an optional `Task.Delay` to the deadline, and cancellation. Multiple signals while awake collapse into one; a signal arriving between the deadline computation and the wait must not be lost (set-then-check ordering, covered by tests).

### Who owns the wake signal

`WorldFreezer` owns it. Every path that enqueues into its `ConcurrentQueue` (input thread, `PostEvent`, subscription handlers, `NotifyTerminalResize`) signals after enqueueing. The loop obtains the waitable from the freezer. This keeps "something entered the world" and "wake the loop" impossible to separate, which is the misuse the old design doc worried about.

### Blocking input

`WorldFreezer.listen'` changes signature: instead of `keyAvailable : unit -> bool` and `readKey : unit -> ConsoleKeyInfo` polled by the loop, it takes a blocking `readKey : CancellationToken -> ConsoleKeyInfo voption` and runs it on a dedicated background thread (`IsBackground = true`) that enqueues and signals. The production implementation wraps `Console.ReadKey true`; the test implementation wraps a `BlockingCollection`.

Shutdown caveat, stated as a guarantee: `Console.ReadKey` cannot be interrupted portably. On disposal the input thread may remain blocked in `ReadKey` until process exit; it is a background thread, so it cannot keep the process alive, and it must not touch any disposed state when/if it returns. Tests use the cancellable mock and shut down cleanly.

### Where the scheduler lives

In WoofWare.Zoomies, as an internal module. The old doc recommended a separate `WoofWare.Incremental.Scheduler` package; by the boundary-cost framework that is speculative generality today — there is exactly one consumer. The module is written so that lifting it upstream later is mechanical (it depends only on `Incremental`, `Clock`, and BCL types, not on Zoomies types). Revisit when a second consumer exists.

### API changes

- `App.run` loses `frameDelayMs`.
- `WorldFreezer.listen'` takes the blocking read function; `listen` wires up the console implementation.
- `pumpOnce` remains `internal` and synchronous: it is the deterministic unit the tests drive. The event loop is exactly `waitForWork` + `pumpOnce`. The waiter is injectable so `App.run` tests do not depend on real time.

## §2 Clock honesty

`IncrementalState.advanceClockAndStabilize` stops fabricating time. New behaviour: call `AdvanceClock` unconditionally (it no-ops unless time strictly advanced — verified above) and stabilize unconditionally. Delete `LastAdvancedTimeNs`. Graph time now equals the last observed wall-clock time, full stop; a wake that observes an unchanged clock (sub-tick) stabilizes without dirtying the time subgraph.

## §3 Alarm-scheduling time primitives

`IncrTime` nodes currently derive from `WatchNow` only, so no alarms exist and an event-driven loop would never wake for them. Each primitive gains an `AtIntervals` dependency:

```fsharp
// value comes from WatchNow; the wake-up comes from AtIntervals
incr.Map2 (fun () (timeNs : TimeNs) -> computeFrame timeNs) (incr.Clock.AtIntervals clock interval) (incr.Clock.WatchNow clock)
```

Properties this must preserve: the *value* is a pure function of the current clock time (unchanged from today, so existing snapshot tests hold), and while the node is observed, `NextAlarmFiresAt` is at most one interval away. When the node becomes unobserved, its alarm must drop out of the wheel (verify during implementation; this is the expected `AtIntervals` behaviour and is what makes invisible spinners free).

Invalid-input handling (non-positive fps/interval) keeps the current graceful-default behaviour and must not schedule alarms.

## §4 Activations into the clock

Replace the side-table (`_LastActivationTimes : Dictionary<NodeKey, DateTime>` + `_ActivationGenerationVar` + `pruneExpiredActivations` polling) with graph-native state:

```fsharp
ActivationsVar : Var<Map<NodeKey, TimeNs>>   // last activation time per key

wasRecentlyActivated key =
    Var.Watch ActivationsVar
    |> incr.Map (Map.tryFind key)            // cutoff isolates this key's changes
    |> incr.Bind (function
        | None -> incr.Return false
        | Some t -> incr.Clock.At clock (t + TIMEOUT) |> incr.Map (fun ba -> ba = Before))
```

- `recordActivation` = pure map insert (plus opportunistic pruning of entries already older than the timeout, to bound the map) and `Var.Set`, from the loop thread, outside stabilization.
- Expiry needs no pruning poll: the `At` alarm flips the node to `false` exactly at the deadline, the vdom recomputes, and — crucially — the alarm is what wakes the event-driven loop to repaint the un-highlighted button. Under the old design this would have silently broken at §1; this ordering is why §4 must land before §1's loop is switched on.
- `pruneExpiredActivations`, the generation var, and all `_IsDirty` writes in this area are deleted. `IVdomContext.WasRecentlyActivated`'s signature is unchanged.

The clock rounds creation time to the microsecond and alarms fire at ~1ms precision; the 500ms activation window is unaffected.

## §5 State: delete `StateMachine`

Production code only ever uses `SetState`; `Inject` and the `ConcurrentQueue` are dead outside their own unit tests; `makeStale` has no cross-thread story anyway. The Expert node is a `Var` in costume. So: delete the `StateMachine` type and module entirely. `App.fs` holds `stateVar : Var<'state>`; the view receives `Var.Watch stateVar`; the loop folds `config.Transition` over the batch exactly as it does today and `Var.Set`s once per batch.

**Deliberate deferral**: the Bonsai-correct alternative — event routing *inside* the graph, where the action interpreter is handed the current model (Bonsai's `apply_action`) — is real and is where the framework should eventually go. It is not this migration. The blocker is that `ActivationResolver` and Tab handling consult intermediate state mid-batch; restructuring that is the Bonsai-layer design's job. Until then, an honest `Var` beats a dishonest state machine: one write path, no queue with unspecified interleaving semantics.

## §6 Dirty-flag elimination

Every `_IsDirty <- true` site, and its fate:

| Site | Fate |
|---|---|
| `setTerminalBounds` | delete — bounds is a `Var`; views depending on `boundsNode` recompute; observer comparison gates the render |
| `setFocusedKey` | delete — same, via `focusedKeyNode` |
| `recordActivation` / `clearActivation` / `pruneExpiredActivations` | deleted by §4 |
| `PostLayoutEvent` | delete — the post-layout fixpoint loop drains the event list directly; it never needed the flag |
| resize corruption (`markDirty` in `pumpOnce`) | survives as a new, narrow `NeedsFullRedraw : bool` — this is a *renderer* fact ("my knowledge of the terminal contents is invalid"), not a graph fact, and full-screen invalidation is its only legitimate use |

End state: render iff `not (referenceEquals previousVdom currentVdom) || NeedsFullRedraw`. The reference comparison is sound because cutoff retains the old boxed value when the recomputed vdom is structurally equal.

Note for reviewers: a view that does **not** depend on `focusedKeyNode` will no longer re-render on focus change. That is correct — if focus isn't reflected in the vdom, the pixels didn't change — but it is a behavioural difference from the flag-based gate, and a test pins it down.

## §7 Render/focus feedback: machine-enforce convergence

Render mutates `FocusedKeyVar` (initial-focus assignment, stale-focus clearing). `renderWithFocusStabilization` re-stabilizes and re-renders once, assuming the second render cannot change focus again. Make the assumption an invariant: after the second render, if focus changed again, fail loudly (framework bug; loud failure is policy per AGENTS.md). No restructuring — moving focus assignment out of render into the action path is Bonsai-layer work.

## §8 Bounds as a node

`IVdomContext.TerminalBounds : Rectangle` is the only imperative scalar read on the context (`FocusedKey` is already a `Node`); read inside a custom incremental view's closure it is a silent staleness trap. Add `TerminalBoundsNode : Rectangle Node` to `IVdomContext`. The scalar stays — pure views (plain `'state -> Vdom` functions lifted by `pureView`) cannot consume a node, and `pureView` force-depends on bounds so the scalar read is consistent there — but its doc comment must state the contract: "only meaningful inside views lifted by `pureView`/`pureViewIncr`; inside hand-built incremental nodes, depend on `TerminalBoundsNode` instead."

## Non-goals (explicitly out of scope)

- **The Bonsai composition layer** (per-component state machines, `assoc` over collections, in-graph action routing). This migration makes the substrate honest; Bonsai is the next design.
- **`pureView` granularity.** Every Tab press rebuilds a `pureView` app's entire vdom because `pureView` depends on focus wholesale. Acceptable: it is the documented semantics of the convenience wrapper, and the Bonsai layer is the real fix.
- **Vdom cutoff cost.** The vdom node's polyEqual cutoff structurally compares trees on recompute. Measure before optimizing; if profiling shows it matters, that is a separate change with a benchmark.
- **`WorldFreezer` internals** beyond the deadline/signal/blocking-read surface.

## Risks and open questions

- `AtIntervals` unobserve behaviour (alarm leaves the wheel) is asserted by Jane Street semantics but must be verified by test in §3's stage.
- The abandoned-input-thread shutdown caveat (§1) is uncomfortable but standard for .NET console apps; revisit if a portable interruptible read appears.
- Stage tests that previously relied on free-running pumping (`frameDelayMs = 0` + poll) need the injectable waiter; this is the largest test-churn item and is budgeted in the plan.
