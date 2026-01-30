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

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> Node =
                let text =
                    if state.ShowFirstText then
                        "Hello, World!"
                    else
                        "Goodbye, World!"

                let textVdom = Vdom.textContent text
                let buttonNode = Button.make (ctx, flipKey, "Flip Text")

                // Use Incr.Map to compose the button incrementally
                ctx.Incr.Map
                    (fun (button : Vdom<DesiredBounds>) ->
                        Vdom.panelSplitAuto (SplitDirection.Horizontal, textVdom, button)
                    )
                    buttonNode

            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let resolver = ActivationResolver.button flipKey FlipText

            let transition (state : State) (event : AppEvent) : State =
                match event with
                | FlipText ->
                    { state with
                        ShowFirstText = not state.ShowFirstText
                    }

            let config =
                AppConfig.simple
                    {
                        ShowFirstText = true
                    }
                    transition
                    (App.pureViewIncr vdom)
                    resolver

            use ctx = IncrTestContext.make console config None

            // Initial render - button unfocused
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            let vdom (ctx : IVdomContext<_>) (state : MultiButtonState) : Vdom<DesiredBounds> Node =
                let statusText = Vdom.textContent $"Last clicked: {state.LastClicked}"

                let button1Node =
                    Button.make (ctx, button1Key, "Button 1", isFirstToFocus = true, isInitiallyFocused = true)

                let button2Node = Button.make (ctx, button2Key, "Button 2")
                let button3Node = Button.make (ctx, button3Key, "Button 3")

                // Compose buttons incrementally using nested Map2 (no Map3 available)
                let button12Node =
                    ctx.Incr.Map2
                        (fun (b1 : Vdom<DesiredBounds>) (b2 : Vdom<DesiredBounds>) ->
                            Vdom.panelSplitAuto (SplitDirection.Vertical, b1, b2)
                        )
                        button1Node
                        button2Node

                ctx.Incr.Map2
                    (fun (b12 : Vdom<DesiredBounds>) (b3 : Vdom<DesiredBounds>) ->
                        let buttons = Vdom.panelSplitAuto (SplitDirection.Vertical, b12, b3)
                        Vdom.panelSplitAuto (SplitDirection.Horizontal, statusText, buttons)
                    )
                    button12Node
                    button3Node

            let console, terminal = ConsoleHarness.make' (fun () -> 50) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let resolver =
                ActivationResolver.combine
                    [
                        ActivationResolver.button button1Key Button1Clicked
                        ActivationResolver.button button2Key Button2Clicked
                        ActivationResolver.button button3Key Button3Clicked
                    ]

            let transition (state : MultiButtonState) (event : MultiButtonEvent) : MultiButtonState =
                match event with
                | Button1Clicked ->
                    {
                        LastClicked = "Button 1"
                    }
                | Button2Clicked ->
                    {
                        LastClicked = "Button 2"
                    }
                | Button3Clicked ->
                    {
                        LastClicked = "Button 3"
                    }

            let config =
                AppConfig.simple
                    {
                        LastClicked = "None"
                    }
                    transition
                    (App.pureViewIncr vdom)
                    resolver

            use ctx = IncrTestContext.make console config None

            // Initial render - Button 1 focused (isFirstToFocus)
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            let vdom (ctx : IVdomContext<_>) (state : bool) : Vdom<DesiredBounds> Node =
                let text = if state then "Hello, World!" else "Goodbye, World!"

                let textVdom = Vdom.textContent text

                let buttonNode =
                    Button.make (ctx, flipKey, "Flip Text", isInitiallyFocused = true, isFirstToFocus = true)

                // Compose incrementally
                ctx.Incr.Map
                    (fun (button : Vdom<DesiredBounds>) ->
                        Vdom.panelSplitAuto (SplitDirection.Horizontal, textVdom, button)
                    )
                    buttonNode

            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let resolver = ActivationResolver.button flipKey FlipText

            let transition (state : bool) (event : AppEvent) : bool =
                match event with
                | FlipText -> not state

            let config = AppConfig.simple true transition (App.pureViewIncr vdom) resolver

            use ctx = IncrTestContext.make console config None

            // Initial render - button focused
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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
            IncrTestContext.advanceTime
                (TimeSpan.FromMilliseconds (VdomContextConstants.RECENT_ACTIVATION_TIMEOUT_MS - 0.01))
                ctx

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
Goodbye, World!                         |
            [* Flip Text *]             |
                                        |
"

                return ConsoleHarness.toString terminal
            }

            IncrTestContext.advanceTime (TimeSpan.FromMilliseconds 0.02) ctx

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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
            let incrState = IncrementalState.make bounds None
            let incr = incrState.Incr
            let ctx = VdomContext.make<unit> incrState

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
