namespace WoofWare.Zoomies.Test

open System
open System.Collections.Generic
open System.Threading.Tasks
open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

/// Tests for event loss bug when activation keystrokes interact with partial batch consumption
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestActivationWithPartialBatch =

    let getStaticUtcNow () =
        DateTime (2025, 11, 25, 13, 33, 00, DateTimeKind.Utc)

    type AppEvent = | ButtonClicked

    [<NoComparison>]
    type State =
        {
            ProcessedKeystrokes : char list
            ProcessedAppEvents : AppEvent list
            ButtonClickCount : int
        }

    [<Test>]
    let ``activation with partial batch consumption does not lose events`` () =
        task {
            let buttonKey = NodeKey.make "test-button"

            let vdom (ctx : VdomContext) (state : State) : Vdom<DesiredBounds, Unkeyed> =
                let text =
                    Vdom.textContent false $"Clicks: {state.ButtonClickCount}, Keys: {state.ProcessedKeystrokes.Length}"

                let button =
                    Button.make (ctx, buttonKey, "Click Me", isInitiallyFocused = true, isFirstToFocus = true)

                Vdom.panelSplitAuto (SplitDirection.Horizontal, text, button)

            let console, terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 3)

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
                { new WorldProcessor<AppEvent, State> with
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
                                    }
                            | WorldStateChange.ApplicationEvent ButtonClicked ->
                                newState <-
                                    { newState with
                                        ProcessedAppEvents = newState.ProcessedAppEvents @ [ ButtonClicked ]
                                        ButtonClickCount = newState.ButtonClickCount + 1
                                    }
                            | _ -> ()

                        if shouldPartiallyConsume && toProcess < inputs.Length then
                            // We only processed one event but there are more in the batch.
                            // Request a rerender to force the framework to split the batch.
                            ProcessWorldResult.make newState
                            |> ProcessWorldResult.withRerender (toProcess - 1)
                        else
                            ProcessWorldResult.make newState
                }

            let clock = MockTime.make ()
            let renderState = RenderState.make console clock.GetUtcNow None

            let mutable state =
                {
                    ProcessedKeystrokes = []
                    ProcessedAppEvents = []
                    ButtonClickCount = 0
                }

            // Initial render - button is focused
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

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
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

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

            let vdom (ctx : VdomContext) (state : State) : Vdom<DesiredBounds, Unkeyed> =
                let button =
                    Button.make (ctx, buttonKey, "Click Me", isInitiallyFocused = true, isFirstToFocus = true)

                button

            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

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
                { new WorldProcessor<AppEvent, State> with
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
                                    }
                            | WorldStateChange.ApplicationEvent ButtonClicked ->
                                newState <-
                                    { newState with
                                        ProcessedAppEvents = newState.ProcessedAppEvents @ [ ButtonClicked ]
                                        ButtonClickCount = newState.ButtonClickCount + 1
                                    }
                            | _ -> ()

                        if toProcess < inputs.Length then
                            ProcessWorldResult.make newState
                            |> ProcessWorldResult.withRerender (toProcess - 1)
                        else
                            ProcessWorldResult.make newState
                }

            let clock = MockTime.make ()
            let renderState = RenderState.make console clock.GetUtcNow None

            let mutable state =
                {
                    ProcessedKeystrokes = []
                    ProcessedAppEvents = []
                    ButtonClickCount = 0
                }

            // Initial render
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            // Send: a, b, c, Space (activate), d, e, Space (activate), f
            world.SendKey (ConsoleKeyInfo ('a', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('b', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('c', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            world.SendKey (ConsoleKeyInfo ('d', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('e', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            world.SendKey (ConsoleKeyInfo ('f', ConsoleKey.NoName, false, false, false))

            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            // All keystrokes should be processed (note: space keys are consumed by activation, not passed through)
            state.ProcessedKeystrokes |> shouldEqual [ 'a' ; 'b' ; 'c' ; 'd' ; 'e' ; 'f' ]

            // Both button clicks should be processed
            state.ButtonClickCount |> shouldEqual 2
        }
