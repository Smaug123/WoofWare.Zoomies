# Implementation plan: completing the Incremental migration

Implement this plan with each stage on its own branch, stacked as necessary on previous branches, so that a reviewer can review each branch in isolation.

The design document is `docs/architecture/incremental-migration-completion.md` (referenced below as DESIGN). Each stage states its dependencies, the design sections it implements, and its correctness oracle. Stages 2-6 and 9-10 are mutually independent once Stage 1 lands; Stage 7 is the convergence point.

General rules for every stage: `dotnet build` clean, full `dotnet test` green, `dotnet fantomas .` applied, `./analyzers/run.sh` warning-free.

---

## Stage 1: Dependency bump to WoofWare.Incremental 0.1.19

**Dependencies**: None

**Implements**: DESIGN "Verified upstream facts" (makes `NextAlarmFiresAt` available)

**Work**: Bump `WoofWare.Zoomies.fsproj` PackageReference and `nix/deps.json`. No code changes.

**Correctness oracle**:
- `dotnet build` and `nix build` both succeed; full test suite green.
- A trivial new test calls `incr.Clock.NextAlarmFiresAt clock` and gets `ValueNone` on a fresh clock — proving the API is visible to this codebase.

---

## Stage 2: Stop fabricating time

**Dependencies**: Stage 1 (stacking convenience only)

**Implements**: DESIGN §2

**Work**: In `IncrementalState.advanceClockAndStabilize`, delete the 1ns-creep logic and the `LastAdvancedTimeNs` field; call `AdvanceClock` unconditionally (upstream no-ops when time has not strictly advanced) and `Stabilize` unconditionally.

**Correctness oracle**:
- Property (FsCheck): for an arbitrary monotone-with-repeats sequence of `DateTime`s, after each `advanceClockAndStabilize` the observed `WatchNow` value equals the maximum time seen so far (no creep), and an observer of a `Var` set between equal-time advances still sees the new value after the stabilize (stabilization is not skipped).
- Property: advancing to a strictly earlier time leaves the observed clock unchanged.
- Full existing suite green — in particular `TestIncrTime` and the spinner/activation tests, which would catch any test that silently depended on time creep.

---

## Stage 3: Delete `StateMachine`; state becomes a `Var`

**Dependencies**: Stage 1 (stacking convenience only)

**Implements**: DESIGN §5

**Work**: Delete `StateMachine.fs`. `App.run` creates `stateVar : Var<'state>`; `config.View` receives `incr.Var.Watch stateVar`; `processChangesWithConfig` and the post-layout loop read via `Var.Value`/set via `Var.Set` exactly where they called `CurrentState`/`SetState`. Delete `TestStateMachine.fs`; fix `TestAppRun`'s one `Inject` usage to go through the world bridge (`PostEvent`) like a real app would.

**Correctness oracle**:
- Full existing suite green (the app-level tests exercise every behaviour the StateMachine carried in production).
- `grep -r Expert1Node WoofWare.Zoomies/` returns nothing.
- New regression test: events posted via `IWorldBridge.PostEvent` from a background task are folded into state in posting order (this is the behaviour the deleted `Inject` tests nominally covered, asserted through the supported API).

---

## Stage 4: Activations into the clock

**Dependencies**: Stage 1 (needs `NextAlarmFiresAt` for its oracle); Stage 2 recommended first (honest time makes the tests simpler)

**Implements**: DESIGN §4

**Work**: Replace `_LastActivationTimes` dictionary + `_ActivationGenerationVar` + `pruneExpiredActivations` with `ActivationsVar : Var<Map<NodeKey, TimeNs>>` and the `Map.tryFind`-projected `Bind` to `Clock.At` described in DESIGN §4. `recordActivation`/`clearActivation` become map updates with opportunistic pruning. Remove the `pruneExpiredActivations` call from `pumpOnce`. `IVdomContext.WasRecentlyActivated` signature unchanged. The `_IsDirty` writes in this area go away (the flag itself survives until Stage 8).

**Correctness oracle**:
- Property (FsCheck, model-based): for an arbitrary interleaving of operations drawn from {advance time by Δ, activate key k, clear key k, query key k}, the observed value of `WasRecentlyActivated k` equals the reference model "last activation of k was strictly less than 500ms ago". Use `MockTime`; this is the correctness oracle for the whole mechanism.
- New test: after `recordActivation` at time t and stabilization, `NextAlarmFiresAt` is `ValueSome` and ≤ t + 500ms + one alarm-precision tick — proving expiry is visible to the future scheduler.
- New test: once the activation window passes, querying again schedules no further alarms (no alarm leak).
- Existing button/checkbox activation snapshot tests green unchanged.

---

## Stage 5: Alarm-scheduling `IncrTime`

**Dependencies**: Stage 1 (needs `NextAlarmFiresAt` for its oracle)

**Implements**: DESIGN §3

**Work**: `spinnerFrameNode*` and `periodicTickNode*` gain an `AtIntervals` dependency (`Map2` shape from DESIGN §3). Value computation unchanged. Non-positive fps/interval keeps graceful defaults and schedules nothing.

**Correctness oracle**:
- Property: for arbitrary valid fps/intervals and arbitrary time advances, node values equal the existing pure reference computation (`(ns / intervalNs) % frameCount` etc.) — i.e. values are unchanged by this stage.
- New test: while a spinner node is observed, after stabilization `NextAlarmFiresAt` is at most one frame interval in the future.
- New test: after disposing the observer and stabilizing, the spinner's alarms are gone (`NextAlarmFiresAt` returns `ValueNone` on an otherwise-quiet clock). This verifies the DESIGN "Risks" item about unobserve behaviour.
- New test: fps ≤ 0 / interval ≤ 0 schedules no alarms.
- Existing spinner/progress-bar snapshot tests green unchanged.

---

## Stage 6a: `WorldFreezer.NextDeadline`

**Dependencies**: None

**Implements**: DESIGN §1 (wake source: freezer internal deadlines)

**Work**: Expose `NextDeadline : unit -> int64 voption` (stopwatch-timestamp units) on `WorldFreezer`, returning the earliest of: pending lone-Esc re-emit deadline, paste-mode timeout deadline; `ValueNone` when neither is pending.

**Correctness oracle**:
- Unit tests with `StopwatchMock`: consume an Esc with no follow-up → `NextDeadline` = consumption timestamp + the Esc timeout; entering paste mode → paste deadline; both pending → the min; neither → `ValueNone`; deadline clears once `Changes()` re-emits.

---

## Stage 6b: Wake signal and blocking input thread

**Dependencies**: None (reviewable independently of 6a)

**Implements**: DESIGN §1 (wake-signal ownership, blocking input)

**Work**: `WorldFreezer` gains a wake signal (recreated-`TaskCompletionSource` pattern per DESIGN §1) exposed as `WaitForChange : CancellationToken -> Task`; every enqueue path (`PostEvent`, subscription handlers, `NotifyTerminalResize`, keystroke arrival) signals it. `listen'` changes signature to take a blocking `readKey : CancellationToken -> ConsoleKeyInfo voption` and spawns the dedicated background input thread; `listen` wraps `Console.ReadKey true`. `MockWorld` gains a `BlockingCollection`-backed implementation. `RefreshExternal` becomes a no-op shim or is deleted (callers updated).

**Correctness oracle**:
- Unit tests: `WaitForChange` completes promptly when (i) a key is posted to the mock input source, (ii) `PostEvent` is called from another thread, (iii) `NotifyTerminalResize` fires; it does *not* complete on a quiet freezer within a generous negative-test window.
- Race test: signal raised after the wait task is created but before it is awaited is not lost; N concurrent `PostEvent`s produce at least one completion and no deadlock.
- Disposal test (mock input): `DisposeAsync` completes with an in-flight blocking read pending; cancelling the token unblocks the mock read cleanly.
- Existing input-parsing tests (ANSI, paste, mouse) green with the new harness.

---

## Stage 7: The event-driven loop

**Dependencies**: Stages 2, 3, 4, 5, 6a, 6b

**Implements**: DESIGN §1 (the loop, scheduler placement, API changes)

**Work**: New internal module (e.g. `EventLoop.fs`): `waitForWork (wake : Task) (deadline : TimeNs voption) (now : ...) (ct : CancellationToken)` racing wake/delay/cancellation. `App.run`: drop `frameDelayMs`; replace `pumpOnce + Thread.Sleep` with `waitForWork` (deadline = min of `NextAlarmFiresAt` and `NextDeadline`, computed after the iteration's final stabilization) followed by `pumpOnce`. The waiter is injectable (internal parameter) so tests substitute a deterministic trigger; `pumpOnce` stays internal and synchronous. Update `Program.fs` and all `App.run` call sites in tests.

**Correctness oracle**:
- Full existing suite green under the new harness (the bulk of this stage's effort; tests drive the injected waiter rather than relying on free-running pumping).
- New test (idle does no work): app with a static view and no alarms; after initial render, repeatedly signalling nothing for a bounded mock-time window produces zero additional `Flush` ops and zero stabilizations (counted via a probe node).
- New test (alarm wake): app whose view contains a spinner; with the real waiter and mock-driven deadlines, the loop wakes at most one alarm-precision tick after the frame deadline and renders the next frame.
- New test (input wake): posting a key wakes a sleeping loop and the resulting render reflects the keystroke, with no intervening timer expiry.
- New test (Esc deadline): a lone Esc with no follow-up key is delivered to the app after the Esc timeout without any other wake source firing.
- Manual smoke (record in PR description, per DESIGN testing intent): sample app idle CPU ≈ 0%; spinner animates smoothly; keystroke latency feels immediate.

---

## Stage 8: Dirty-flag elimination

**Dependencies**: Stages 4, 7

**Implements**: DESIGN §6

**Work**: Delete `_IsDirty`/`markDirty`/`markClean`/`isDirty`. Add `NeedsFullRedraw` set only by the resize-corruption path in `pumpOnce`, cleared on the subsequent full render. Render gate becomes `not (referenceEquals previousVdom currentVdom) || NeedsFullRedraw`. The post-layout fixpoint loop keys off the drained event list directly. Delete the `setTerminalBounds`/`setFocusedKey` dirty writes.

**Correctness oracle**:
- Full suite green.
- New test (no spurious renders): focus change with a view that ignores focus → no render (pins the deliberate behavioural difference called out in DESIGN §6); focus change with a `pureView` → renders.
- New test: resize triggers exactly one full-screen redraw (clear + repaint) and subsequent frames diff normally.
- New test: post-layout events still converge (existing `TestPostLayoutEvents` suite is the oracle here; it must pass unmodified or with changes explicable purely by the gate change).
- `grep -rn "IsDirty\|markDirty\|markClean" WoofWare.Zoomies/` returns only the `NeedsFullRedraw` machinery.

---

## Stage 9: Machine-enforce render/focus convergence

**Dependencies**: None (orthogonal; lands any time)

**Implements**: DESIGN §7

**Work**: In `renderWithFocusStabilization`, after the second render, check focus again; if it changed a second time, fail loudly (exception — framework bug).

**Correctness oracle**:
- Full suite green with the assertion live (the suite's focus tests — `TestFocusCycle`, initial-focus tests — are the evidence the invariant actually holds).
- The assertion failure message names the before/after keys, so a future violation is diagnosable.

---

## Stage 10: `TerminalBoundsNode`

**Dependencies**: None (orthogonal; lands any time)

**Implements**: DESIGN §8

**Work**: Add `TerminalBoundsNode : Rectangle Node` to `IVdomContext`; document the staleness contract on the scalar `TerminalBounds`. Audit components and framework code for scalar bounds reads inside incremental closures and migrate any found.

**Correctness oracle**:
- New test: a hand-built incremental view depending on `TerminalBoundsNode` re-renders with correct content after a resize, without any `pureView` involvement.
- Full suite green.

---

## Stage 11: Documentation truth-up

**Dependencies**: All previous stages

**Implements**: DESIGN as-built record

**Work**: Delete `docs/architecture/event-loop-design.md` (superseded; its still-true content is absorbed into the design doc). Update `docs/explanation/render-loop.md`, `docs/explanation/cutoff.md`, `docs/how_to/emergency-rerender.md`, `docs/how_to/debug-layout.md`, and `AGENTS.md` for: no `frameDelayMs`, event-driven loop, `Var`-backed state, alarm-based activations/animations, the new `WorldFreezer` surface. Mark `incremental-migration-completion.md` as completed/as-built.

**Correctness oracle**:
- `grep -rn "frameDelayMs\|StateMachine\|pruneExpiredActivations\|RefreshExternal" docs/ AGENTS.md README.md` returns nothing stale.
- Each doc's code samples compile (paste into a scratch test where practical).

---

## Sequencing summary

```
1 ──► 2 ──┬───────────────► 7 ──► 8 ──► 11
   ├► 3 ──┤                              ▲
   ├► 4 ──┤                              │
   ├► 5 ──┤            9, 10 ────────────┘ (any time)
6a ───────┤
6b ───────┘
```

Stages 2-5 and 6a/6b can be developed in parallel after Stage 1; Stage 7 is where the loop flips over and is the highest-risk stage (largest test churn); Stage 8 is deliberately *after* 7 so the dirty flag keeps the old loop safe until the new wake sources are proven.
