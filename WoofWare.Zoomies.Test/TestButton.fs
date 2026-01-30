namespace WoofWare.Zoomies.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Incremental
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

[<RequireQualifiedAccess>]
module private Object =
    let referenceEquals<'a when 'a : not struct> (x : 'a) (y : 'a) =
        // Type-safe wrapper for ReferenceEquals
        Object.ReferenceEquals (x, y)

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestButton =
    let getUtcNow () = MockTime.defaultStartTime

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

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
                let text =
                    if state.ShowFirstText then
                        "Hello, World!"
                    else
                        "Goodbye, World!"

                let textVdom = Vdom.textContent text
                let buttonNode = Button.make (ctx, flipKey, "Flip Text")
                let buttonObserver = ctx.Incr.Observe buttonNode
                ctx.Incr.Stabilize ()
                let button = Observer.value buttonObserver

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
                { new WorldProcessor<AppEvent, unit, State> with
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

                    member _.ProcessPostLayoutEvents (_events, _ctx, state) = state
                }

            let clock = MockTime.makeFromConsole console

            let renderState, _ = MockTime.makeRenderStateFromTimer console clock None

            let mutable state =
                {
                    ShowFirstText = true
                }

            // Initial render - button unfocused
            state <-
                App.pumpOnce
                    getUtcNow
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

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

            state <-
                App.pumpOnce
                    getUtcNow
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

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

            state <-
                App.pumpOnce
                    getUtcNow
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

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

            state <-
                App.pumpOnce
                    getUtcNow
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

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

            let vdom (ctx : IVdomContext<_>) (state : MultiButtonState) : Vdom<DesiredBounds> =
                let statusText = Vdom.textContent $"Last clicked: {state.LastClicked}"

                let button1Node =
                    Button.make (ctx, button1Key, "Button 1", isFirstToFocus = true, isInitiallyFocused = true)

                let button2Node = Button.make (ctx, button2Key, "Button 2")
                let button3Node = Button.make (ctx, button3Key, "Button 3")

                let button1Observer = ctx.Incr.Observe button1Node
                let button2Observer = ctx.Incr.Observe button2Node
                let button3Observer = ctx.Incr.Observe button3Node
                ctx.Incr.Stabilize ()
                let button1 = Observer.value button1Observer
                let button2 = Observer.value button2Observer
                let button3 = Observer.value button3Observer

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
                { new WorldProcessor<MultiButtonEvent, unit, MultiButtonState> with
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

                    member _.ProcessPostLayoutEvents (_events, _ctx, state) = state
                }

            let clock = MockTime.makeFromConsole console

            let renderState, _ = MockTime.makeRenderStateFromTimer console clock None

            let mutable state =
                {
                    LastClicked = "None"
                }

            // Initial render - Button 1 focused (isFirstToFocus)
            state <-
                App.pumpOnce
                    getUtcNow
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

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

            state <-
                App.pumpOnce
                    getUtcNow
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

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

            state <-
                App.pumpOnce
                    getUtcNow
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

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

            state <-
                App.pumpOnce
                    getUtcNow
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

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

            state <-
                App.pumpOnce
                    getUtcNow
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

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

            state <-
                App.pumpOnce
                    getUtcNow
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

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

            state <-
                App.pumpOnce
                    getUtcNow
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

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

            let vdom (ctx : IVdomContext<_>) (state : bool) : Vdom<DesiredBounds> =
                let text = if state then "Hello, World!" else "Goodbye, World!"

                let textVdom = Vdom.textContent text

                let buttonNode =
                    Button.make (ctx, flipKey, "Flip Text", isInitiallyFocused = true, isFirstToFocus = true)

                let buttonObserver = ctx.Incr.Observe buttonNode
                ctx.Incr.Stabilize ()
                let button = Observer.value buttonObserver

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
                { new WorldProcessor<AppEvent, unit, bool> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        let mutable newState = state

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent FlipText -> newState <- not newState
                            | _ -> ()

                        ProcessWorldResult.make newState

                    member _.ProcessPostLayoutEvents (_events, _ctx, state) = state
                }

            let clock = MockTime.makeFromConsole console

            let renderState, advance = MockTime.makeRenderStateFromTimer console clock None

            // Use clock.CurrentTime for getUtcNow so time advances properly
            let getUtcNow = clock.CurrentTime

            let mutable state = true

            // Initial render - button focused
            state <-
                App.pumpOnce
                    getUtcNow
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

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

            state <-
                App.pumpOnce
                    getUtcNow
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

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

            advance (TimeSpan.FromMilliseconds (VdomContextConstants.RECENT_ACTIVATION_TIMEOUT_MS - 0.01))
            |> ignore<DateTime>

            state <-
                App.pumpOnce
                    getUtcNow
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

            expect {
                snapshot
                    @"
Goodbye, World!                         |
            [* Flip Text *]             |
                                        |
"

                return ConsoleHarness.toString terminal
            }

            advance (TimeSpan.FromMilliseconds 0.02) |> ignore<DateTime>

            state <-
                App.pumpOnce
                    getUtcNow
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

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

    [<Test>]
    let ``Button.make Node updates when focus changes incrementally`` () =
        task {
            // This test verifies that Button.make properly depends on FocusedKeyNode
            // so the returned Node updates when focus changes without needing to re-call Button.make

            let bounds =
                {
                    TopLeftX = 0
                    TopLeftY = 0
                    Width = 80
                    Height = 24
                }

            let buttonKey = NodeKey.make "test-button"
            let otherKey = NodeKey.make "other"

            // Start with no focus
            let incrState = IncrementalState.make () bounds None
            let incr = incrState.Incr
            let ctx = VdomContext.make<unit, unit> incrState

            // Create a button node once
            let buttonNode = Button.make (ctx :> IVdomContext, buttonKey, "Test Button")

            // Observe the node
            let observer = incr.Observe buttonNode
            incr.Stabilize ()

            // Initial state: button is not focused
            let vdom1 = Observer.value observer

            // Verify button shows unfocused style (single brackets with spaces)
            let text1 = Vdom.debugDump vdom1
            text1.Contains "[  Test Button  ]" |> shouldEqual true

            // Change focus to the button
            IncrementalState.setFocusedKey (Some buttonKey) incrState
            incr.Stabilize ()

            // Now the node should have updated to show focused style
            let vdom2 = Observer.value observer

            // Verify button shows focused style (double brackets)
            let text2 = Vdom.debugDump vdom2
            text2.Contains "[[ Test Button ]]" |> shouldEqual true

            // Verify the vdom reference actually changed (not just same object)
            Object.referenceEquals vdom1 vdom2 |> shouldEqual false

            // Change focus to a different key
            IncrementalState.setFocusedKey (Some otherKey) incrState
            incr.Stabilize ()

            // Button should be unfocused again
            let vdom3 = Observer.value observer
            let text3 = Vdom.debugDump vdom3
            text3.Contains "[  Test Button  ]" |> shouldEqual true

            // Clear focus entirely
            IncrementalState.setFocusedKey None incrState
            incr.Stabilize ()

            // Button should still be unfocused
            let vdom4 = Observer.value observer
            let text4 = Vdom.debugDump vdom4
            text4.Contains "[  Test Button  ]" |> shouldEqual true
        }
