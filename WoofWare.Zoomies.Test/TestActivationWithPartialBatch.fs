namespace WoofWare.Zoomies.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

/// Tests for event loss bug when activation keystrokes interact with partial batch consumption
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

    [<Test>]
    let ``activation with partial batch consumption does not lose events`` () =
        task {
            let buttonKey = NodeKey.make "test-button"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
                let text =
                    Vdom.textContent $"Clicks: {state.ButtonClickCount}, Keys: {state.ProcessedKeystrokes.Length}"

                let button =
                    Button.make (ctx, buttonKey, "Click Me", isInitiallyFocused = true, isFirstToFocus = true)

                Vdom.panelSplitAuto (SplitDirection.Horizontal, text, button)

            let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.button buttonKey ButtonClicked

            // Track how many times ProcessWorld is called
            let mutable processWorldCallCount = 0

            let processWorld =
                { new WorldProcessor<AppEvent, unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        processWorldCallCount <- processWorldCallCount + 1
                        let mutable newState = state

                        // On the first call to ProcessWorld in this test, we want to process only
                        // one event and then request a Rerender to simulate partial batch consumption.
                        // This tests whether events get lost when activation handling happens
                        // immediately after a partial batch.
                        let shouldPartiallyConsume = processWorldCallCount = 1

                        let toProcess =
                            if shouldPartiallyConsume then
                                min 1 inputs.Length
                            else
                                inputs.Length

                        // Process events
                        for i = 0 to toProcess - 1 do
                            match inputs.[i] with
                            | WorldStateChange.Keystroke k ->
                                newState <-
                                    { newState with
                                        ProcessedKeystrokes = newState.ProcessedKeystrokes @ [ k.KeyChar ]
                                        AllEventsInOrder = newState.AllEventsInOrder @ [ Keystroke k.KeyChar ]
                                    }
                            | WorldStateChange.ApplicationEvent ButtonClicked ->
                                newState <-
                                    { newState with
                                        ProcessedAppEvents = newState.ProcessedAppEvents @ [ ButtonClicked ]
                                        ButtonClickCount = newState.ButtonClickCount + 1
                                        AllEventsInOrder = newState.AllEventsInOrder @ [ AppEvent ButtonClicked ]
                                    }
                            | _ -> ()

                        if shouldPartiallyConsume && toProcess < inputs.Length then
                            // We only processed one event but there are more in the batch.
                            // Request a rerender to force the framework to split the batch.
                            ProcessWorldResult.make newState
                            |> ProcessWorldResult.withRerender (toProcess - 1)
                        else
                            ProcessWorldResult.make newState

                    member _.ProcessPostLayoutEvents (_events, _ctx, state) = state
                }

            let clock = MockTime.makeFromConsole console
            let renderState, _ = MockTime.makeRenderStateFromTimer console clock None

            let mutable state =
                {
                    ProcessedKeystrokes = []
                    ProcessedAppEvents = []
                    ButtonClickCount = 0
                    AllEventsInOrder = []
                }

            // Initial render - button is focused
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

            // Send a batch of events that will trigger the bug:
            // - 'a' keystroke (will be processed in first batch, then Rerender requested)
            // - 'b' keystroke (BUG: this gets lost)
            // - 'c' keystroke (BUG: this gets lost)
            // - Space keystroke (activation - triggers injection of ButtonClicked event)
            // - 'd' keystroke (after activation)
            world.SendKey (ConsoleKeyInfo ('a', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('b', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('c', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            world.SendKey (ConsoleKeyInfo ('d', ConsoleKey.NoName, false, false, false))

            // Process the batch
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

            // Verify that ALL keystrokes were processed (not lost due to the bug)
            // Expected: ['a', 'b', 'c', 'd']
            // With the bug: ['a', 'd'] - 'b' and 'c' are lost
            state.ProcessedKeystrokes |> shouldEqual [ 'a' ; 'b' ; 'c' ; 'd' ]

            // Verify that the button click was processed
            state.ProcessedAppEvents |> shouldEqual [ ButtonClicked ]
            state.ButtonClickCount |> shouldEqual 1
        }

    [<Test>]
    let ``multiple activations with partial batches do not lose events`` () =
        task {
            let buttonKey = NodeKey.make "test-button"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
                let button =
                    Button.make (ctx, buttonKey, "Click Me", isInitiallyFocused = true, isFirstToFocus = true)

                button

            let console, _terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.button buttonKey ButtonClicked

            // Always process only 1 event at a time to maximize the chance of hitting the bug
            let processWorld =
                { new WorldProcessor<AppEvent, unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        let mutable newState = state

                        // Always process only the first event
                        let toProcess = min 1 inputs.Length

                        for i = 0 to toProcess - 1 do
                            match inputs.[i] with
                            | WorldStateChange.Keystroke k ->
                                newState <-
                                    { newState with
                                        ProcessedKeystrokes = newState.ProcessedKeystrokes @ [ k.KeyChar ]
                                        AllEventsInOrder = newState.AllEventsInOrder @ [ Keystroke k.KeyChar ]
                                    }
                            | WorldStateChange.ApplicationEvent ButtonClicked ->
                                newState <-
                                    { newState with
                                        ProcessedAppEvents = newState.ProcessedAppEvents @ [ ButtonClicked ]
                                        ButtonClickCount = newState.ButtonClickCount + 1
                                        AllEventsInOrder = newState.AllEventsInOrder @ [ AppEvent ButtonClicked ]
                                    }
                            | _ -> ()

                        if toProcess < inputs.Length then
                            ProcessWorldResult.make newState
                            |> ProcessWorldResult.withRerender (toProcess - 1)
                        else
                            ProcessWorldResult.make newState

                    member _.ProcessPostLayoutEvents (_events, _ctx, state) = state
                }

            let clock = MockTime.makeFromConsole console
            let renderState, _ = MockTime.makeRenderStateFromTimer console clock None

            let mutable state =
                {
                    ProcessedKeystrokes = []
                    ProcessedAppEvents = []
                    ButtonClickCount = 0
                    AllEventsInOrder = []
                }

            // Initial render
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

            // Send: a, b, c, Space (activate), d, e, Space (activate), f
            world.SendKey (ConsoleKeyInfo ('a', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('b', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('c', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            world.SendKey (ConsoleKeyInfo ('d', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('e', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            world.SendKey (ConsoleKeyInfo ('f', ConsoleKey.NoName, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

            // All keystrokes should be processed (note: space keys are consumed by activation, not passed through)
            state.ProcessedKeystrokes |> shouldEqual [ 'a' ; 'b' ; 'c' ; 'd' ; 'e' ; 'f' ]

            // Both button clicks should be processed
            state.ButtonClickCount |> shouldEqual 2
        }

    [<Test>]
    let ``Enter-based activation with partial batch consumption does not lose events`` () =
        task {
            let buttonKey = NodeKey.make "test-button"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
                let text =
                    Vdom.textContent $"Clicks: {state.ButtonClickCount}, Keys: {state.ProcessedKeystrokes.Length}"

                let button =
                    Button.make (ctx, buttonKey, "Click Me", isInitiallyFocused = true, isFirstToFocus = true)

                Vdom.panelSplitAuto (SplitDirection.Horizontal, text, button)

            let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.button buttonKey ButtonClicked

            let mutable processWorldCallCount = 0

            let processWorld =
                { new WorldProcessor<AppEvent, unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        processWorldCallCount <- processWorldCallCount + 1
                        let mutable newState = state

                        let shouldPartiallyConsume = processWorldCallCount = 1

                        let toProcess =
                            if shouldPartiallyConsume then
                                min 1 inputs.Length
                            else
                                inputs.Length

                        for i = 0 to toProcess - 1 do
                            match inputs.[i] with
                            | WorldStateChange.Keystroke k ->
                                newState <-
                                    { newState with
                                        ProcessedKeystrokes = newState.ProcessedKeystrokes @ [ k.KeyChar ]
                                        AllEventsInOrder = newState.AllEventsInOrder @ [ Keystroke k.KeyChar ]
                                    }
                            | WorldStateChange.ApplicationEvent ButtonClicked ->
                                newState <-
                                    { newState with
                                        ProcessedAppEvents = newState.ProcessedAppEvents @ [ ButtonClicked ]
                                        ButtonClickCount = newState.ButtonClickCount + 1
                                        AllEventsInOrder = newState.AllEventsInOrder @ [ AppEvent ButtonClicked ]
                                    }
                            | _ -> ()

                        if shouldPartiallyConsume && toProcess < inputs.Length then
                            ProcessWorldResult.make newState
                            |> ProcessWorldResult.withRerender (toProcess - 1)
                        else
                            ProcessWorldResult.make newState

                    member _.ProcessPostLayoutEvents (_events, _ctx, state) = state
                }

            let clock = MockTime.makeFromConsole console
            let renderState, _ = MockTime.makeRenderStateFromTimer console clock None

            let mutable state =
                {
                    ProcessedKeystrokes = []
                    ProcessedAppEvents = []
                    ButtonClickCount = 0
                    AllEventsInOrder = []
                }

            // Initial render - button is focused
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

            // Send events using Enter instead of Spacebar for activation
            world.SendKey (ConsoleKeyInfo ('a', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('b', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('c', ConsoleKey.NoName, false, false, false))
            // Enter key for activation (char '\r' is typical for Enter)
            world.SendKey (ConsoleKeyInfo ('\r', ConsoleKey.Enter, false, false, false))
            world.SendKey (ConsoleKeyInfo ('d', ConsoleKey.NoName, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

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

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
                Button.make (ctx, buttonKey, "Click Me", isInitiallyFocused = true, isFirstToFocus = true)

            let console, _terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.button buttonKey ButtonClicked

            // Process all events at once (no partial batching) to clearly see the order
            let processWorld =
                { new WorldProcessor<AppEvent, unit, State> with
                    member _.ProcessWorld (inputs, _renderState, state) =
                        let mutable newState = state

                        for i = 0 to inputs.Length - 1 do
                            match inputs.[i] with
                            | WorldStateChange.Keystroke k ->
                                newState <-
                                    { newState with
                                        ProcessedKeystrokes = newState.ProcessedKeystrokes @ [ k.KeyChar ]
                                        AllEventsInOrder = newState.AllEventsInOrder @ [ Keystroke k.KeyChar ]
                                    }
                            | WorldStateChange.ApplicationEvent ButtonClicked ->
                                newState <-
                                    { newState with
                                        ProcessedAppEvents = newState.ProcessedAppEvents @ [ ButtonClicked ]
                                        ButtonClickCount = newState.ButtonClickCount + 1
                                        AllEventsInOrder = newState.AllEventsInOrder @ [ AppEvent ButtonClicked ]
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState

                    member _.ProcessPostLayoutEvents (_events, _ctx, state) = state
                }

            let clock = MockTime.makeFromConsole console
            let renderState, _ = MockTime.makeRenderStateFromTimer console clock None

            let mutable state =
                {
                    ProcessedKeystrokes = []
                    ProcessedAppEvents = []
                    ButtonClickCount = 0
                    AllEventsInOrder = []
                }

            // Initial render
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

            // Send: 'a', 'b', Space (activate), 'c', 'd'
            // The ButtonClicked app event should appear between 'b' and 'c' in the processing order
            world.SendKey (ConsoleKeyInfo ('a', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('b', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            world.SendKey (ConsoleKeyInfo ('c', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('d', ConsoleKey.NoName, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

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

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
                Button.make (ctx, buttonKey, "Click Me", isInitiallyFocused = true, isFirstToFocus = true)

            let console, _terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.button buttonKey ButtonClicked

            // Process all events at once
            let processWorld =
                { new WorldProcessor<AppEvent, unit, State> with
                    member _.ProcessWorld (inputs, _renderState, state) =
                        let mutable newState = state

                        for i = 0 to inputs.Length - 1 do
                            match inputs.[i] with
                            | WorldStateChange.Keystroke k ->
                                newState <-
                                    { newState with
                                        ProcessedKeystrokes = newState.ProcessedKeystrokes @ [ k.KeyChar ]
                                        AllEventsInOrder = newState.AllEventsInOrder @ [ Keystroke k.KeyChar ]
                                    }
                            | WorldStateChange.ApplicationEvent ButtonClicked ->
                                newState <-
                                    { newState with
                                        ProcessedAppEvents = newState.ProcessedAppEvents @ [ ButtonClicked ]
                                        ButtonClickCount = newState.ButtonClickCount + 1
                                        AllEventsInOrder = newState.AllEventsInOrder @ [ AppEvent ButtonClicked ]
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState

                    member _.ProcessPostLayoutEvents (_events, _ctx, state) = state
                }

            let clock = MockTime.makeFromConsole console
            let renderState, _ = MockTime.makeRenderStateFromTimer console clock None

            let mutable state =
                {
                    ProcessedKeystrokes = []
                    ProcessedAppEvents = []
                    ButtonClickCount = 0
                    AllEventsInOrder = []
                }

            // Initial render
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

            // Send: 'a', Space (activate), 'b', Enter (activate), 'c'
            // This tests both activation keys and verifies ordering is maintained
            world.SendKey (ConsoleKeyInfo ('a', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            world.SendKey (ConsoleKeyInfo ('b', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('\r', ConsoleKey.Enter, false, false, false))
            world.SendKey (ConsoleKeyInfo ('c', ConsoleKey.NoName, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

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
