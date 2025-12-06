namespace WoofWare.Zoomies.Test

open System
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestButton =
    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    type AppEvent = | FlipText

    type State =
        {
            ShowFirstText : bool
        }

    [<Test>]
    let ``button flips between two text displays when activated`` () =
        task {
            let flipKey = NodeKey.make "flip-button"

            let vdom (ctx : VdomContext) (state : State) : Vdom<DesiredBounds> =
                let text =
                    if state.ShowFirstText then
                        "Hello, World!"
                    else
                        "Goodbye, World!"

                let textVdom = Vdom.textContent text
                let button = Button.make (ctx, flipKey, "Flip Text")

                Vdom.panelSplitAuto (SplitDirection.Horizontal, textVdom, button)

            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.button flipKey FlipText

            let processWorld =
                { new WorldProcessor<AppEvent, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        let mutable newState = state

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent FlipText ->
                                newState <-
                                    { newState with
                                        ShowFirstText = not newState.ShowFirstText
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState
                }

            let clock = MockTime.make ()

            let renderState = RenderState.make console clock.GetUtcNow None

            let mutable state =
                {
                    ShowFirstText = true
                }

            // Initial render - button unfocused
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            expect {
                snapshot
                    @"
Hello, World!                           |
            [  Flip Text  ]             |
                                        |
"

                return ConsoleHarness.toString terminal
            }

            // Press tab to focus the button
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            expect {
                snapshot
                    @"
Hello, World!                           |
            [[ Flip Text ]]             |
                                        |
"

                return ConsoleHarness.toString terminal
            }

            // Press space to activate the button
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            expect {
                snapshot
                    @"
Goodbye, World!                         |
            [* Flip Text *]             |
                                        |
"

                return ConsoleHarness.toString terminal
            }

            // Press space again to flip back
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            expect {
                snapshot
                    @"
Hello, World!                           |
            [* Flip Text *]             |
                                        |
"

                return ConsoleHarness.toString terminal
            }
        }

    type MultiButtonEvent =
        | Button1Clicked
        | Button2Clicked
        | Button3Clicked

    type MultiButtonState =
        {
            LastClicked : string
        }

    [<Test>]
    let ``focus moves between buttons and correct button activates`` () =
        task {
            let button1Key = NodeKey.make "button1"
            let button2Key = NodeKey.make "button2"
            let button3Key = NodeKey.make "button3"

            let vdom (ctx : VdomContext) (state : MultiButtonState) : Vdom<DesiredBounds> =
                let statusText = Vdom.textContent $"Last clicked: {state.LastClicked}"

                let button1 =
                    Button.make (ctx, button1Key, "Button 1", isFirstToFocus = true, isInitiallyFocused = true)

                let button2 = Button.make (ctx, button2Key, "Button 2")
                let button3 = Button.make (ctx, button3Key, "Button 3")

                let buttons =
                    Vdom.panelSplitAuto (SplitDirection.Vertical, button1, button2)
                    |> fun b1b2 -> Vdom.panelSplitAuto (SplitDirection.Vertical, b1b2, button3)

                Vdom.panelSplitAuto (SplitDirection.Horizontal, statusText, buttons)

            let console, terminal = ConsoleHarness.make' (fun () -> 50) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver =
                ActivationResolver.combine
                    [
                        ActivationResolver.button button1Key Button1Clicked
                        ActivationResolver.button button2Key Button2Clicked
                        ActivationResolver.button button3Key Button3Clicked
                    ]

            let processWorld =
                { new WorldProcessor<MultiButtonEvent, MultiButtonState> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        let mutable newState = state

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent Button1Clicked ->
                                newState <-
                                    {
                                        LastClicked = "Button 1"
                                    }
                            | WorldStateChange.ApplicationEvent Button2Clicked ->
                                newState <-
                                    {
                                        LastClicked = "Button 2"
                                    }
                            | WorldStateChange.ApplicationEvent Button3Clicked ->
                                newState <-
                                    {
                                        LastClicked = "Button 3"
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState
                }

            let clock = MockTime.make ()

            let renderState = RenderState.make console clock.GetUtcNow None

            let mutable state =
                {
                    LastClicked = "None"
                }

            // Initial render - Button 1 focused (isFirstToFocus)
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            expect {
                snapshot
                    @"
Last clicked: None                                |
 [[ Button 1 ]]  [  Button 2  ]   [  Button 3  ]  |
                                                  |
"

                return ConsoleHarness.toString terminal
            }

            // Activate Button 1 with space
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            expect {
                snapshot
                    @"
Last clicked: Button 1                            |
 [* Button 1 *]  [  Button 2  ]   [  Button 3  ]  |
                                                  |
"

                return ConsoleHarness.toString terminal
            }

            // Tab to Button 2
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            expect {
                snapshot
                    @"
Last clicked: Button 1                            |
  * Button 1 *   [[ Button 2 ]]   [  Button 3  ]  |
                                                  |
"

                return ConsoleHarness.toString terminal
            }

            // Activate Button 2 with Enter
            world.SendKey (ConsoleKeyInfo ('\r', ConsoleKey.Enter, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            expect {
                snapshot
                    @"
Last clicked: Button 2                            |
  * Button 1 *   [* Button 2 *]   [  Button 3  ]  |
                                                  |
"

                return ConsoleHarness.toString terminal
            }

            // Tab to Button 3
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            expect {
                snapshot
                    @"
Last clicked: Button 2                            |
  * Button 1 *    * Button 2 *    [[ Button 3 ]]  |
                                                  |
"

                return ConsoleHarness.toString terminal
            }

            // Activate Button 3 with space
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            expect {
                snapshot
                    @"
Last clicked: Button 3                            |
  * Button 1 *    * Button 2 *    [* Button 3 *]  |
                                                  |
"

                return ConsoleHarness.toString terminal
            }

            // Tab back to Button 1 (cycles)
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            expect {
                snapshot
                    @"
Last clicked: Button 3                            |
 [* Button 1 *]   * Button 2 *     * Button 3 *   |
                                                  |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``As time passes, the button press stops again`` () =
        task {
            let flipKey = NodeKey.make "flip-button"

            let vdom (ctx : VdomContext) (state : bool) : Vdom<DesiredBounds> =
                let text = if state then "Hello, World!" else "Goodbye, World!"

                let textVdom = Vdom.textContent text

                let button =
                    Button.make (ctx, flipKey, "Flip Text", isInitiallyFocused = true, isFirstToFocus = true)

                Vdom.panelSplitAuto (SplitDirection.Horizontal, textVdom, button)

            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.button flipKey FlipText

            let processWorld =
                { new WorldProcessor<AppEvent, bool> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        let mutable newState = state

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent FlipText -> newState <- not newState
                            | _ -> ()

                        ProcessWorldResult.make newState
                }

            let clock = MockTime.make ()

            let renderState = RenderState.make console clock.GetUtcNow None

            let mutable state = true

            // Initial render - button focused
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            expect {
                snapshot
                    @"
Hello, World!                           |
            [[ Flip Text ]]             |
                                        |
"

                return ConsoleHarness.toString terminal
            }

            // Press space to activate the button
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            expect {
                snapshot
                    @"
Goodbye, World!                         |
            [* Flip Text *]             |
                                        |
"

                return ConsoleHarness.toString terminal
            }

            // See how the button press evolves over time. Wait til just before the timer elapses:

            clock.Advance (TimeSpan.FromMilliseconds (VdomContext.RECENT_ACTIVATION_TIMEOUT_MS - 0.01))
            |> ignore<DateTime>

            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            expect {
                snapshot
                    @"
Goodbye, World!                         |
            [* Flip Text *]             |
                                        |
"

                return ConsoleHarness.toString terminal
            }

            clock.Advance (TimeSpan.FromMilliseconds 0.02) |> ignore<DateTime>
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            expect {
                snapshot
                    @"
Goodbye, World!                         |
            [[ Flip Text ]]             |
                                        |
"

                return ConsoleHarness.toString terminal
            }

        }
