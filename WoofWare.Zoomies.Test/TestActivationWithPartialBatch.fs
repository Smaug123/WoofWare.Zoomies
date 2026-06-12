namespace WoofWare.Zoomies.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

/// Tests for event ordering when activation keystrokes interact with other keystrokes.
/// These tests verify that activation events are correctly interleaved with surrounding keystrokes.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestActivationWithPartialBatch =

    type AppEvent = | ButtonClicked

    /// Tracks all events in the order they were processed for ordering assertions
    type ProcessedEvent =
        | Keystroke of char
        | AppEvent of AppEvent

    [<NoComparison>]
    type State =
        {
            ProcessedKeystrokes : char list
            ProcessedAppEvents : AppEvent list
            ButtonClickCount : int
            /// All events in the order they were processed (for ordering assertions)
            AllEventsInOrder : ProcessedEvent list
        }

    /// App event that includes both keystrokes and button clicks
    type InputEvent =
        | KeystrokeEvent of char
        | ButtonClickEvent

    let initialState =
        {
            ProcessedKeystrokes = []
            ProcessedAppEvents = []
            ButtonClickCount = 0
            AllEventsInOrder = []
        }

    let transition (state : State) (event : InputEvent) : State =
        match event with
        | KeystrokeEvent c ->
            { state with
                ProcessedKeystrokes = state.ProcessedKeystrokes @ [ c ]
                AllEventsInOrder = state.AllEventsInOrder @ [ Keystroke c ]
            }
        | ButtonClickEvent ->
            { state with
                ProcessedAppEvents = state.ProcessedAppEvents @ [ ButtonClicked ]
                ButtonClickCount = state.ButtonClickCount + 1
                AllEventsInOrder = state.AllEventsInOrder @ [ AppEvent ButtonClicked ]
            }

    [<Test>]
    let ``activation does not lose events`` () =
        task {
            let buttonKey = NodeKey.make "test-button"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> Node =
                let text =
                    Vdom.textContent $"Clicks: {state.ButtonClickCount}, Keys: {state.ProcessedKeystrokes.Length}"

                let buttonNode =
                    Button.make (ctx, buttonKey, "Click Me", isInitiallyFocused = true, isFirstToFocus = true)

                buttonNode
                |> ctx.Incr.Map (fun button -> Vdom.panelSplitAuto (SplitDirection.Horizontal, text, button))

            let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 3)

            use worldFreezer =
                WorldFreezer.listen' UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

            let world = MockWorld.attach worldFreezer

            // ActivationResolver converts space/enter on button to ButtonClickEvent
            let activationResolver =
                ActivationResolver (fun key keystroke _state ->
                    if key = buttonKey then
                        if keystroke.Key = ConsoleKey.Spacebar || keystroke.Key = ConsoleKey.Enter then
                            Some ButtonClickEvent
                        else
                            None
                    else
                        None
                )

            // HandleInput converts keystrokes to KeystrokeEvent
            let handleInput change =
                match change with
                | WorldStateChange.Keystroke k -> Some (KeystrokeEvent k.KeyChar)
                | _ -> None

            let config : AppConfig<State, InputEvent, unit> =
                {
                    Initial = initialState
                    Transition = transition
                    View = App.pureViewIncr vdom
                    HandleInput = handleInput
                    HandlePostLayout = fun _ s -> s
                    FocusHandling = FocusHandling.FrameworkManaged
                    ActivationResolver = activationResolver
                    OnSetup = fun _ -> ()
                }

            use ctx = IncrTestContext.make console config None

            // Initial render - button is focused
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // Send a batch of events:
            // - 'a', 'b', 'c' keystrokes
            // - Space keystroke (activation - triggers ButtonClickEvent)
            // - 'd' keystroke (after activation)
            world.SendKey (ConsoleKeyInfo ('a', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('b', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('c', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            world.SendKey (ConsoleKeyInfo ('d', ConsoleKey.NoName, false, false, false))

            // Process the batch
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            let state = IncrTestContext.currentState ctx

            // Verify that ALL keystrokes were processed (not lost)
            // Expected: ['a', 'b', 'c', 'd']
            state.ProcessedKeystrokes |> shouldEqual [ 'a' ; 'b' ; 'c' ; 'd' ]

            // Verify that the button click was processed
            state.ProcessedAppEvents |> shouldEqual [ ButtonClicked ]
            state.ButtonClickCount |> shouldEqual 1
        }

    [<Test>]
    let ``multiple activations do not lose events`` () =
        task {
            let buttonKey = NodeKey.make "test-button"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> Node =
                Button.make (ctx, buttonKey, "Click Me", isInitiallyFocused = true, isFirstToFocus = true)

            let console, _terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            use worldFreezer =
                WorldFreezer.listen' UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

            let world = MockWorld.attach worldFreezer

            let activationResolver =
                ActivationResolver (fun key keystroke _state ->
                    if key = buttonKey then
                        if keystroke.Key = ConsoleKey.Spacebar || keystroke.Key = ConsoleKey.Enter then
                            Some ButtonClickEvent
                        else
                            None
                    else
                        None
                )

            let handleInput change =
                match change with
                | WorldStateChange.Keystroke k -> Some (KeystrokeEvent k.KeyChar)
                | _ -> None

            let config : AppConfig<State, InputEvent, unit> =
                {
                    Initial = initialState
                    Transition = transition
                    View = App.pureViewIncr vdom
                    HandleInput = handleInput
                    HandlePostLayout = fun _ s -> s
                    FocusHandling = FocusHandling.FrameworkManaged
                    ActivationResolver = activationResolver
                    OnSetup = fun _ -> ()
                }

            use ctx = IncrTestContext.make console config None

            // Initial render
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // Send: a, b, c, Space (activate), d, e, Space (activate), f
            world.SendKey (ConsoleKeyInfo ('a', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('b', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('c', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            world.SendKey (ConsoleKeyInfo ('d', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('e', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            world.SendKey (ConsoleKeyInfo ('f', ConsoleKey.NoName, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            let state = IncrTestContext.currentState ctx

            // All keystrokes should be processed (note: space keys are consumed by activation, not passed through)
            state.ProcessedKeystrokes |> shouldEqual [ 'a' ; 'b' ; 'c' ; 'd' ; 'e' ; 'f' ]

            // Both button clicks should be processed
            state.ButtonClickCount |> shouldEqual 2
        }

    [<Test>]
    let ``Enter-based activation does not lose events`` () =
        task {
            let buttonKey = NodeKey.make "test-button"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> Node =
                let text =
                    Vdom.textContent $"Clicks: {state.ButtonClickCount}, Keys: {state.ProcessedKeystrokes.Length}"

                let buttonNode =
                    Button.make (ctx, buttonKey, "Click Me", isInitiallyFocused = true, isFirstToFocus = true)

                buttonNode
                |> ctx.Incr.Map (fun button -> Vdom.panelSplitAuto (SplitDirection.Horizontal, text, button))

            let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 3)

            use worldFreezer =
                WorldFreezer.listen' UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

            let world = MockWorld.attach worldFreezer

            let activationResolver =
                ActivationResolver (fun key keystroke _state ->
                    if key = buttonKey then
                        if keystroke.Key = ConsoleKey.Spacebar || keystroke.Key = ConsoleKey.Enter then
                            Some ButtonClickEvent
                        else
                            None
                    else
                        None
                )

            let handleInput change =
                match change with
                | WorldStateChange.Keystroke k -> Some (KeystrokeEvent k.KeyChar)
                | _ -> None

            let config : AppConfig<State, InputEvent, unit> =
                {
                    Initial = initialState
                    Transition = transition
                    View = App.pureViewIncr vdom
                    HandleInput = handleInput
                    HandlePostLayout = fun _ s -> s
                    FocusHandling = FocusHandling.FrameworkManaged
                    ActivationResolver = activationResolver
                    OnSetup = fun _ -> ()
                }

            use ctx = IncrTestContext.make console config None

            // Initial render - button is focused
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // Send events using Enter instead of Spacebar for activation
            world.SendKey (ConsoleKeyInfo ('a', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('b', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('c', ConsoleKey.NoName, false, false, false))
            // Enter key for activation (char '\r' is typical for Enter)
            world.SendKey (ConsoleKeyInfo ('\r', ConsoleKey.Enter, false, false, false))
            world.SendKey (ConsoleKeyInfo ('d', ConsoleKey.NoName, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            let state = IncrTestContext.currentState ctx

            // Verify ALL keystrokes were processed
            state.ProcessedKeystrokes |> shouldEqual [ 'a' ; 'b' ; 'c' ; 'd' ]

            // Verify button was activated via Enter
            state.ProcessedAppEvents |> shouldEqual [ ButtonClicked ]
            state.ButtonClickCount |> shouldEqual 1
        }

    [<Test>]
    let ``app event is delivered in correct order relative to surrounding keystrokes`` () =
        task {
            let buttonKey = NodeKey.make "test-button"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> Node =
                Button.make (ctx, buttonKey, "Click Me", isInitiallyFocused = true, isFirstToFocus = true)

            let console, _terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            use worldFreezer =
                WorldFreezer.listen' UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

            let world = MockWorld.attach worldFreezer

            let activationResolver =
                ActivationResolver (fun key keystroke _state ->
                    if key = buttonKey then
                        if keystroke.Key = ConsoleKey.Spacebar || keystroke.Key = ConsoleKey.Enter then
                            Some ButtonClickEvent
                        else
                            None
                    else
                        None
                )

            let handleInput change =
                match change with
                | WorldStateChange.Keystroke k -> Some (KeystrokeEvent k.KeyChar)
                | _ -> None

            let config : AppConfig<State, InputEvent, unit> =
                {
                    Initial = initialState
                    Transition = transition
                    View = App.pureViewIncr vdom
                    HandleInput = handleInput
                    HandlePostLayout = fun _ s -> s
                    FocusHandling = FocusHandling.FrameworkManaged
                    ActivationResolver = activationResolver
                    OnSetup = fun _ -> ()
                }

            use ctx = IncrTestContext.make console config None

            // Initial render
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // Send: 'a', 'b', Space (activate), 'c', 'd'
            // The ButtonClickEvent should appear between 'b' and 'c' in the processing order
            world.SendKey (ConsoleKeyInfo ('a', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('b', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            world.SendKey (ConsoleKeyInfo ('c', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('d', ConsoleKey.NoName, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            let state = IncrTestContext.currentState ctx

            // The app event should be interleaved correctly: 'a', 'b', ButtonClicked, 'c', 'd'
            // The activation keystroke (Space) is consumed and replaced by the app event in its position
            state.AllEventsInOrder
            |> shouldEqual
                [
                    Keystroke 'a'
                    Keystroke 'b'
                    AppEvent ButtonClicked
                    Keystroke 'c'
                    Keystroke 'd'
                ]
        }

    [<Test>]
    let ``multiple activations maintain correct ordering with Enter and Spacebar`` () =
        task {
            let buttonKey = NodeKey.make "test-button"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> Node =
                Button.make (ctx, buttonKey, "Click Me", isInitiallyFocused = true, isFirstToFocus = true)

            let console, _terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            use worldFreezer =
                WorldFreezer.listen' UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

            let world = MockWorld.attach worldFreezer

            let activationResolver =
                ActivationResolver (fun key keystroke _state ->
                    if key = buttonKey then
                        if keystroke.Key = ConsoleKey.Spacebar || keystroke.Key = ConsoleKey.Enter then
                            Some ButtonClickEvent
                        else
                            None
                    else
                        None
                )

            let handleInput change =
                match change with
                | WorldStateChange.Keystroke k -> Some (KeystrokeEvent k.KeyChar)
                | _ -> None

            let config : AppConfig<State, InputEvent, unit> =
                {
                    Initial = initialState
                    Transition = transition
                    View = App.pureViewIncr vdom
                    HandleInput = handleInput
                    HandlePostLayout = fun _ s -> s
                    FocusHandling = FocusHandling.FrameworkManaged
                    ActivationResolver = activationResolver
                    OnSetup = fun _ -> ()
                }

            use ctx = IncrTestContext.make console config None

            // Initial render
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // Send: 'a', Space (activate), 'b', Enter (activate), 'c'
            // This tests both activation keys and verifies ordering is maintained
            world.SendKey (ConsoleKeyInfo ('a', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            world.SendKey (ConsoleKeyInfo ('b', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('\r', ConsoleKey.Enter, false, false, false))
            world.SendKey (ConsoleKeyInfo ('c', ConsoleKey.NoName, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            let state = IncrTestContext.currentState ctx

            // Verify ordering: 'a', ButtonClicked, 'b', ButtonClicked, 'c'
            state.AllEventsInOrder
            |> shouldEqual
                [
                    Keystroke 'a'
                    AppEvent ButtonClicked
                    Keystroke 'b'
                    AppEvent ButtonClicked
                    Keystroke 'c'
                ]

            // Verify counts
            state.ProcessedKeystrokes |> shouldEqual [ 'a' ; 'b' ; 'c' ]
            state.ButtonClickCount |> shouldEqual 2
        }
