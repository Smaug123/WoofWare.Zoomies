# Button Component Design (v2)

## Overview

This document describes the design for adding a Button component to WoofWare.Zoomies. Unlike Checkbox (which has persistent boolean state), Button has temporally transient "pressed" state that decays after approximately 500ms.

The design supports two usage modes:

1. **Low-level**: User manually tracks press state and handles keystrokes in `ProcessWorld`
2. **High-level**: Framework intercepts keystrokes on focused elements, transforms them to typed events via user-provided resolver, and manages visual feedback timing automatically

The high-level mode mirrors how Tab handling works: when framework focus handling is enabled, certain keystrokes are intercepted and transformed before the user sees them.

## Design Goals

- **Type safety**: No boxing or runtime casts
- **Separation of concerns**: VDOM describes structure/visuals; activation resolver handles behavior
- **Composability**: Button builds on existing focus primitives
- **Extensibility**: Same mechanism supports TextBox and future interactive components
- **Single event loop**: All events flow through the existing `WorldStateChange` stream

## Key Insight: Activation as Keystroke Transformation

Rather than storing events in the VDOM, we introduce an **ActivationResolver**: a user-provided function that transforms keystrokes on focused elements into application events.

| Component | Keystroke | Transformed Event |
|-----------|-----------|-------------------|
| Button | Space/Enter | `SubmitClicked` |
| TextBox | `'a'` | `TextInput("field1", 'a')` |
| TextBox | Backspace | `TextBackspace("field1")` |
| Checkbox | Space | `CheckboxToggled("opt1")` |

This cleanly separates concerns:

| Concern | Responsibility |
|---------|----------------|
| VDOM | What to display (purely visual) |
| Focus tracking | Which node receives input |
| Activation resolver | Keystroke → event transformation |
| ProcessWorld | Handle events, update state |

## Refactoring Plan

The implementation proceeds in four phases:

1. Add `ActivationResolver` type and integrate into `App.run`
2. Add activation tracking infrastructure to `VdomContext`
3. Modify event processing loop to use the resolver
4. Add `Vdom.button` primitive and `Components.Button` helper

The VDOM types require minimal changes—no new type parameters, no boxing, no visitor pattern.

---

## Phase 1: ActivationResolver Type

### Definition

```fsharp
/// Transforms keystrokes on focused elements into application events.
/// Return Some to intercept the keystroke and emit the event.
/// Return None to pass the keystroke through to ProcessWorld unchanged.
///
/// The state passed into the `ActivationResolver` is the source of truth.
/// You should not close over external state that may change during app
/// execution, for example.
type ActivationResolver<'appEvent, 'state> =
    delegate of focusedKey:NodeKey * keystroke:ConsoleKeyInfo * state:'state -> 'appEvent option
```

Using a delegate rather than a function type for better interop and clearer signature.

### Integration with App.run

```fsharp
module App =
    let run<'appEvent, 'state when 'state : equality>
        (getEnv : string -> string option)
        (initialState : 'state)
        (haveFrameworkHandleFocus : 'state -> bool)
        (processWorld : WorldProcessor<'appEvent, 'state>)
        (vdom : VdomContext -> 'state -> ComposableVdom<DesiredBounds>)
        (resolveActivation : ActivationResolver<'appEvent, 'state>)
        : Task
```

### Combinator for Composing Resolvers

Since applications will have multiple interactive elements, we provide a combinator:

```fsharp
module ActivationResolver =
    /// Combine multiple resolvers. First one to return Some wins.
    let combine (resolvers : ActivationResolver<'e, 's> list) : ActivationResolver<'e, 's> =
        ActivationResolver(fun key keystroke state ->
            // TODO: this is pretty inefficient; consider this more carefully,
            // perhaps with a map of "keystroke" to "component that accepts the keystroke".
            // This isn't a blocking architectural problem.
            resolvers
            |> List.tryPick (fun r -> r.Invoke(key, keystroke, state)))

    /// Create a resolver for a button (activates on Space/Enter)
    let button (key : NodeKey) (event : 'e) : ActivationResolver<'e, 's> =
        ActivationResolver(fun k keystroke _ ->
            if k = key && isActivationKey keystroke then Some event
            else None)

    /// Create a resolver for a text input field
    let textInput
        (key : NodeKey)
        (onChar : char -> 'e)
        (onBackspace : 'e)
        : ActivationResolver<'e, 's> =
        ActivationResolver(fun k keystroke _ ->
            if k <> key then None
            elif keystroke.Key = ConsoleKey.Backspace then Some onBackspace
            elif keystroke.KeyChar <> '\000' then Some (onChar keystroke.KeyChar)
            else None)

let private isActivationKey (k : ConsoleKeyInfo) =
    (k.Key = ConsoleKey.Spacebar || k.Key = ConsoleKey.Enter)
    && k.Modifiers = ConsoleModifiers.None
```

### Usage Example

```fsharp
type AppEvent =
    | SubmitClicked
    | CancelClicked
    | UsernameChar of char
    | UsernameBackspace

let submitKey = NodeKey.make "submit"
let cancelKey = NodeKey.make "cancel"
let usernameKey = NodeKey.make "username"

let resolver =
    ActivationResolver.combine [
        ActivationResolver.button submitKey SubmitClicked
        ActivationResolver.button cancelKey CancelClicked
        ActivationResolver.textInput usernameKey UsernameChar UsernameBackspace
    ]
```

---

## Phase 2: Activation Tracking Infrastructure

**Note:** The "Internal Event Type" subsection below describes infrastructure that was **not implemented**. The actual implementation uses synchronous pruning instead. See "Implementation Divergence" section at the end.

### VdomContext Additions

The framework tracks when activations occur for visual feedback:

```fsharp
type VdomContext =
    private
        {
            mutable _FocusedKey : NodeKey option
            mutable _TerminalBounds : Rectangle
            mutable IsDirty : bool
            _LastActivationTimes : Dictionary<NodeKey, DateTime>
            GetUtcNow : unit -> DateTime
        }

[<RequireQualifiedAccess>]
module VdomContext =
    // ... existing members ...

    /// Returns true if the node with the given key was activated within the
    /// visual feedback window (approximately 500ms).
    let wasRecentlyActivated (key : NodeKey) (ctx : VdomContext) : bool =
        match ctx._LastActivationTimes.TryGetValue key with
        | true, time -> (ctx.GetUtcNow () - time).TotalMilliseconds < 500.0
        | false, _ -> false

    /// Record that a node was just activated. Internal use only.
    let internal recordActivation (key : NodeKey) (ctx : VdomContext) : unit =
        ctx._LastActivationTimes.[key] <- ctx.GetUtcNow ()
        ctx.IsDirty <- true

    /// Clear activation state for a key. Internal use only.
    let internal clearActivation (key : NodeKey) (ctx : VdomContext) : unit =
        if ctx._LastActivationTimes.Remove key then
            ctx.IsDirty <- true
```

### Internal Event Type

For the visual feedback timeout:

```fsharp
type internal FrameworkEvent =
    | ActivationVisualTimeout of NodeKey

type WorldFreezer<'appEvent> =
    private
        {
            // ... existing fields ...
            _InternalEvents : ConcurrentQueue<FrameworkEvent>
        }

    member internal this.PostInternalEvent(evt : FrameworkEvent) =
        this._InternalEvents.Enqueue evt
```

---

## Phase 3: Event Processing Changes

**Note:** The timeout mechanism described below was the original design but was **not implemented**. See "Implementation Divergence" section at the end for the simpler approach actually used.

### Modified Event Processing

When framework focus handling is enabled, we now consult the resolver for all keystrokes (not just Tab):

```fsharp
let processChanges
    (changes : WorldStateChange<'appEvent>[])
    (state : 'state)
    (haveFrameworkHandleFocus : 'state -> bool)
    (renderState : RenderState)
    (processWorld : WorldProcessor<'appEvent, 'state>)
    (resolveActivation : ActivationResolver<'appEvent, 'state>)
    (vdom : VdomContext -> 'state -> ComposableVdom<DesiredBounds>)
    : 'state
    =
    // ... existing batch processing structure ...

    if haveFrameworkHandleFocus currentState then
        // Check each keystroke against the resolver
        while nextToProcess < changes.Length do
            match changes.[nextToProcess] with
            | WorldStateChange.Keystroke k when isTabKey k ->
                // Existing Tab handling (unchanged)
                ...

            | WorldStateChange.Keystroke k ->
                match VdomContext.focusedKey renderState.VdomContext with
                | Some focusedKey ->
                    match resolveActivation.Invoke(focusedKey, k, currentState) with
                    | Some appEvent ->
                        // Activation! Process batch up to here, then inject event
                        processBatchUpTo nextToProcess

                        // Record activation time for visual feedback
                        VdomContext.recordActivation focusedKey renderState.VdomContext

                        // Schedule visual timeout
                        task {
                            do! Task.Delay(500)
                            listener.PostInternalEvent(FrameworkEvent.ActivationVisualTimeout focusedKey)
                        } |> ignore

                        // Inject the resolved event
                        injectedEventHolder.[0] <- WorldStateChange.ApplicationEvent appEvent
                        let span = ReadOnlySpan(injectedEventHolder)
                        let result = processWorld.ProcessWorld(span, renderState.VdomContext, currentState)
                        currentState <- result.NewState

                        // Re-render for visual feedback
                        Render.oneStep renderState currentState (vdom renderState.VdomContext)
                        VdomContext.markClean renderState.VdomContext

                        startOfBatch <- nextToProcess + 1
                        nextToProcess <- nextToProcess + 1

                    | None ->
                        // Resolver didn't handle it, include in batch
                        nextToProcess <- nextToProcess + 1

                | None ->
                    // Nothing focused, include in batch
                    nextToProcess <- nextToProcess + 1

            | _ ->
                nextToProcess <- nextToProcess + 1
```

### Handling Internal Events

At the start of each pump cycle:

```fsharp
let pumpOnce ... =
    // Handle internal events first
    let mutable internalEvt = Unchecked.defaultof<_>
    while listener._InternalEvents.TryDequeue(&internalEvt) do
        match internalEvt with
        | FrameworkEvent.ActivationVisualTimeout key ->
            VdomContext.clearActivation key renderState.VdomContext

    // ... rest of pump logic ...
```

---

## Phase 4: Button Primitive and Helper

### Low-Level Primitive

The VDOM primitive is purely visual:

```fsharp
type internal UnkeyedVdomNode<'bounds> =
    // ... existing cases ...
    | Button of label : string * isFocused : bool * isPressed : bool

[<Sealed; AbstractClass>]
type Vdom =
    // ... existing members ...

    /// Creates a button with the given visual state.
    /// This is purely visual—use ActivationResolver to handle activation.
    static member button (isFocused : bool) (isPressed : bool) (label : string) : UnkeyedVdom<DesiredBounds> =
        UnkeyedVdom(UnkeyedVdomNode.Button(label, isFocused, isPressed))
```

### Rendering

The button renders as:

| State | Rendering |
|-------|-----------|
| Normal | `[ Submit ]` |
| Focused | `[[ Submit ]]` or `[ Submit ]` with highlight |
| Pressed | Inverted colors or distinct styling |
| Focused + Pressed | Both effects combined |

### High-Level Helper

```fsharp
module Components.Button =
    /// Creates a button with automatic focus and activation visual state.
    /// Note: You must also register an ActivationResolver for the key.
    let make (ctx : VdomContext) (key : NodeKey) (label : string) : ComposableVdom<DesiredBounds> =
        let isFocused = VdomContext.focusedKey ctx = Some key
        let isPressed = VdomContext.wasRecentlyActivated key ctx

        Vdom.button isFocused isPressed label
        |> Vdom.withKey key
        |> Vdom.withFocusTracking
```

---

## Complete Usage Example

```fsharp
module MyApp =
    type AppEvent =
        | SubmitClicked
        | CancelClicked
        | CounterIncremented

    type State =
        {
            Count : int
            Submitted : bool
        }

    let submitKey = NodeKey.make "submit"
    let cancelKey = NodeKey.make "cancel"
    let incrementKey = NodeKey.make "increment"

    let resolver =
        ActivationResolver.combine [
            ActivationResolver.button submitKey SubmitClicked
            ActivationResolver.button cancelKey CancelClicked
            ActivationResolver.button incrementKey CounterIncremented
        ]

    let view (ctx : VdomContext) (state : State) =
        let submitButton = Components.Button.make ctx submitKey "Submit"
        let cancelButton = Components.Button.make ctx cancelKey "Cancel"
        let incrementButton = Components.Button.make ctx incrementKey $"+1 (count: %d{state.Count})"

        Vdom.panelSplitProportion(
            SplitDirection.Horizontal,
            0.5,
            Vdom.panelSplitProportion(SplitDirection.Vertical, 0.5, submitButton, cancelButton),
            incrementButton)

    let processWorld =
        { new WorldProcessor<AppEvent, State> with
            member _.ProcessWorld(events, ctx, state) =
                let mutable newState = state

                for evt in events do
                    match evt with
                    | WorldStateChange.ApplicationEvent SubmitClicked ->
                        newState <- { newState with Submitted = true }
                    | WorldStateChange.ApplicationEvent CounterIncremented ->
                        newState <- { newState with Count = newState.Count + 1 }
                    | WorldStateChange.ApplicationEvent CancelClicked ->
                        // Handle cancel
                        ()
                    | _ -> ()

                ProcessWorldResult.make newState
        }

    let run () =
        App.run
            Environment.GetEnvironmentVariable
            { Count = 0; Submitted = false }
            (fun _ -> true)
            processWorld
            view
            resolver
```

---

## TextBox Example

The same mechanism naturally extends to text input:

```fsharp
module TextBoxExample =
    type AppEvent =
        | UsernameChar of char
        | UsernameBackspace
        | PasswordChar of char
        | PasswordBackspace
        | SubmitLogin

    type State =
        {
            Username : string
            Password : string
        }

    let usernameKey = NodeKey.make "username"
    let passwordKey = NodeKey.make "password"
    let submitKey = NodeKey.make "submit"

    let resolver =
        ActivationResolver.combine [
            ActivationResolver.textInput usernameKey UsernameChar UsernameBackspace
            ActivationResolver.textInput passwordKey PasswordChar PasswordBackspace
            ActivationResolver.button submitKey SubmitLogin
        ]

    let view (ctx : VdomContext) (state : State) =
        let username = Components.TextBox.make ctx usernameKey state.Username "Username"
        let password = Components.TextBox.make ctx passwordKey (String.replicate state.Password.Length "*") "Password"
        let submit = Components.Button.make ctx submitKey "Login"

        Vdom.panelSplitAuto(
            SplitDirection.Horizontal,
            Vdom.panelSplitAuto(SplitDirection.Horizontal, username, password),
            submit)

    let processWorld =
        { new WorldProcessor<AppEvent, State> with
            member _.ProcessWorld(events, ctx, state) =
                let mutable newState = state

                for evt in events do
                    match evt with
                    | WorldStateChange.ApplicationEvent (UsernameChar c) ->
                        newState <- { newState with Username = newState.Username + string c }
                    | WorldStateChange.ApplicationEvent UsernameBackspace ->
                        if newState.Username.Length > 0 then
                            newState <- { newState with Username = newState.Username.[..^1] }
                    | WorldStateChange.ApplicationEvent (PasswordChar c) ->
                        newState <- { newState with Password = newState.Password + string c }
                    | WorldStateChange.ApplicationEvent PasswordBackspace ->
                        if newState.Password.Length > 0 then
                            newState <- { newState with Password = newState.Password.[..^1] }
                    | WorldStateChange.ApplicationEvent SubmitLogin ->
                        // Handle login
                        ()
                    | _ -> ()

                ProcessWorldResult.make newState
        }
```

---

## Edge Cases and Invariants

### Resolver returns None for all keystrokes

The keystroke passes through to `ProcessWorld` unchanged. This is the "low-level" mode.

### Multiple resolvers match the same key

`ActivationResolver.combine` uses first-match-wins. Document this clearly.

### Focus changes mid-batch

Each keystroke is evaluated against the *current* focused key at the time of processing. Batch splitting ensures the VDOM is re-rendered before processing continues.

### Activation on non-existent key

If the focused key doesn't exist in the VDOM (e.g., node was removed), the resolver may still return `Some`. The event is emitted normally. `wasRecentlyActivated` will return `true` for that key even though there's nothing to render.

### State-dependent activation

The resolver receives the current state, enabling patterns like:
```fsharp
ActivationResolver(fun key keystroke state ->
    if key = submitKey && state.FormIsValid && isActivationKey keystroke then
        Some SubmitClicked
    else
        None)
```

### Visual feedback for TextBox

Text boxes typically don't need the 500ms "pressed" visual. The feedback is the updated text. `wasRecentlyActivated` can be ignored, or used for cursor blink timing.

---

## VDOM Changes Summary

The VDOM types require **minimal changes**:

1. Add `Button` case to `UnkeyedVdomNode`
2. Add `Vdom.button` static method

**No changes to:**
- Type parameters (no `'event` anywhere)
- Class hierarchy (if we even need one—the existing DU structure may suffice)
- `FocusableVdom` (doesn't need to exist as a separate type)

The complexity of type erasure, visitor patterns, and boxing is entirely avoided.

---

## Future Considerations

### Mouse click activation

Mouse clicks arrive as `WorldStateChange.MouseEvent`. The framework can check if click coordinates fall within a focusable node's bounds, focus that node, and then invoke the resolver with a synthetic "activation" keystroke (or extend the resolver signature to handle mouse events directly).

### Configurable visual feedback timeout

Could be a parameter to `App.run` or per-key configuration. Low priority.

### Keyboard shortcuts

The resolver already supports this naturally:

```fsharp
ActivationResolver(fun key keystroke state ->
    // Ctrl+S triggers save regardless of focus
    if keystroke.Key = ConsoleKey.S && keystroke.Modifiers = ConsoleModifiers.Control then
        Some SaveDocument
    // Space/Enter on focused save button
    elif key = saveKey && isActivationKey keystroke then
        Some SaveDocument
    else
        None)
```

Note: Global shortcuts should be checked first, before focus-specific handling.

---

## Summary

| Phase | Changes |
|-------|---------|
| 1 | Add `ActivationResolver` type and `ActivationResolver` combinators |
| 2 | Add `wasRecentlyActivated` to `VdomContext`, internal timeout events |
| 3 | Modify event processing to consult resolver, inject transformed events |
| 4 | Add `Vdom.button` primitive and `Components.Button.make` helper |

**Key benefits of this approach:**

- **No type erasure**: No boxing, no visitor pattern, no runtime casts
- **Clean separation**: VDOM is purely visual; resolver handles behavior
- **Extensible**: Same mechanism supports Button, TextBox, Checkbox, and future components
- **Explicit**: The resolver makes keystroke handling visible and testable
- **Simple VDOM**: No additional type parameters or class hierarchy changes needed

The tradeoff is non-locality: activation is declared separately from VDOM construction. But this is arguably more honest—rendering and behavior are different concerns, and keys already need to be coordinated for focus checking anyway.

---

## Implementation Divergence

### Activation Visual Feedback Timeout Mechanism

The design doc (Phase 2 and Phase 3 above) originally proposed an **asynchronous timeout mechanism** for clearing activation visual feedback:

**Original Design:**
- When a button is activated, schedule a `Task.Delay(500)` that posts `FrameworkEvent.ActivationVisualTimeout` to an internal events queue
- At the start of each render cycle, drain internal events and clear activation state for timed-out keys
- This required infrastructure: `FrameworkEvent` type, `_InternalEvents` queue, `PostInternalEvent` and `DrainInternalEvents` methods

**Actual Implementation:**
- When a button is activated, call `VdomContext.recordActivation` which stores the activation timestamp
- At the start of each render cycle, call `VdomContext.pruneExpiredActivations` which synchronously removes any activations older than 500ms
- No async tasks, no internal events queue, no framework event types needed

**Rationale for Divergence:**

The synchronous pruning approach is **simpler and equally correct**:

1. **Simpler implementation**: No async task scheduling, no concurrent queue management, no internal event plumbing
2. **Same user-visible behavior**: Visual feedback clears after ~500ms in both designs
3. **Acceptable timing imprecision**: If the UI is idle (no renders), activation state persists slightly longer than 500ms—but this doesn't matter because there's nothing rendering the visual feedback anyway
4. **Easier to reason about**: State cleanup happens at a single well-defined point in the render cycle rather than via async callbacks

**Code removed during cleanup (2025-01-25):**
- `FrameworkEvent` type and `ActivationVisualTimeout` case
- `WorldFreezer._InternalEvents` field
- `WorldFreezer.PostInternalEvent` and `WorldFreezer.DrainInternalEvents` methods
- Event draining loop in `App.pumpOnce`
- Unused `listener` parameter in `App.processChanges`

The implementation maintains all the benefits described in this design doc while using a simpler mechanism that better fits the immediate-mode rendering model of the framework.
