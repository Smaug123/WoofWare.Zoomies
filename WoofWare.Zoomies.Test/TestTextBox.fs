namespace WoofWare.Zoomies.Test

open System
open NUnit.Framework
open FsUnitTyped
open WoofWare.Expect
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTextBox =
    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    type AppEvent =
        | TextEdit of TextBoxAction
        | ButtonClicked

    type State =
        {
            Content : string
            Cursor : int
        }

    [<Test>]
    let ``tab moves focus from textbox to button when framework focus enabled`` () =
        task {
            let textBoxKey = NodeKey.make "textbox"
            let buttonKey = NodeKey.make "button"

            let vdom (ctx : VdomContext) (state : State) : Vdom<DesiredBounds> =
                let textbox =
                    TextBox.make (
                        ctx,
                        textBoxKey,
                        state.Content,
                        state.Cursor,
                        isFirstToFocus = true,
                        isInitiallyFocused = true
                    )

                let button = Button.make (ctx, buttonKey, "Submit")

                Vdom.panelSplitAuto (SplitDirection.Horizontal, textbox, button)

            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

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
                        ActivationResolver.textBox textBoxKey TextEdit
                        ActivationResolver.button buttonKey ButtonClicked
                    ]

            let processWorld =
                { new WorldProcessor<AppEvent, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        let mutable newState = state

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent (TextEdit action) ->
                                let content, cursor = TextBoxHelpers.applyAction state.Content state.Cursor action

                                newState <-
                                    {
                                        Content = content
                                        Cursor = cursor
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState
                }

            let clock = MockTime.make ()

            let renderState = RenderState.make console clock.GetUtcNow None

            let mutable state =
                {
                    Content = ""
                    Cursor = 0
                }

            // Initial render - textbox focused
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            expect {
                snapshot
                    @"
|                                       |
                                        |
              [  Submit  ]              |
"

                return ConsoleHarness.toString terminal
            }

            // Press Tab to move focus to button
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            expect {
                snapshot
                    @"
                                        |
                                        |
              [[ Submit ]]              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``all keystrokes including tab pass to ProcessWorld when framework focus disabled`` () =
        task {
            let textBoxKey = NodeKey.make "textbox"

            let vdom (ctx : VdomContext) (state : State) : Vdom<DesiredBounds> =
                TextBox.make (ctx, textBoxKey, state.Content, state.Cursor, isInitiallyFocused = true)

            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let resolver = ActivationResolver.none

            let receivedKeystrokes = ResizeArray<ConsoleKeyInfo> ()

            let processWorld =
                { new WorldProcessor<AppEvent, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        let mutable newState = state

                        for input in inputs do
                            match input with
                            | WorldStateChange.Keystroke k -> receivedKeystrokes.Add k
                            | _ -> ()

                        ProcessWorldResult.make newState
                }

            let clock = MockTime.make ()

            let renderState = RenderState.make console clock.GetUtcNow None

            let mutable state =
                {
                    Content = ""
                    Cursor = 0
                }

            // Initial render
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            // Press Tab
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            // Press 'a'
            world.SendKey (ConsoleKeyInfo ('a', ConsoleKey.A, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            // Press Backspace
            world.SendKey (ConsoleKeyInfo ('\b', ConsoleKey.Backspace, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            // Verify all three keystrokes reached ProcessWorld
            receivedKeystrokes.Count |> shouldEqual 3
            receivedKeystrokes.[0].Key |> shouldEqual ConsoleKey.Tab
            receivedKeystrokes.[1].Key |> shouldEqual ConsoleKey.A
            receivedKeystrokes.[2].Key |> shouldEqual ConsoleKey.Backspace
        }

    [<Test>]
    let ``typing uppercase and punctuation updates text correctly`` () =
        task {
            let textBoxKey = NodeKey.make "textbox"

            let vdom (ctx : VdomContext) (state : State) : Vdom<DesiredBounds> =
                TextBox.make (ctx, textBoxKey, state.Content, state.Cursor, isInitiallyFocused = true)

            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.textBox textBoxKey TextEdit

            let processWorld =
                { new WorldProcessor<AppEvent, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        let mutable newState = state

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent (TextEdit action) ->
                                let content, cursor = TextBoxHelpers.applyAction state.Content state.Cursor action

                                newState <-
                                    {
                                        Content = content
                                        Cursor = cursor
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState
                }

            let clock = MockTime.make ()

            let renderState = RenderState.make console clock.GetUtcNow None

            let mutable state =
                {
                    Content = ""
                    Cursor = 0
                }

            // Initial render
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            // Type 'H' (Shift+h)
            world.SendKey (ConsoleKeyInfo ('H', ConsoleKey.H, true, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            state.Content |> shouldEqual "H"
            state.Cursor |> shouldEqual 1

            // Type 'e'
            world.SendKey (ConsoleKeyInfo ('e', ConsoleKey.E, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            state.Content |> shouldEqual "He"
            state.Cursor |> shouldEqual 2

            // Type 'l'
            world.SendKey (ConsoleKeyInfo ('l', ConsoleKey.L, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            state.Content |> shouldEqual "Hel"
            state.Cursor |> shouldEqual 3

            // Type 'l'
            world.SendKey (ConsoleKeyInfo ('l', ConsoleKey.L, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            state.Content |> shouldEqual "Hell"
            state.Cursor |> shouldEqual 4

            // Type 'o'
            world.SendKey (ConsoleKeyInfo ('o', ConsoleKey.O, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            state.Content |> shouldEqual "Hello"
            state.Cursor |> shouldEqual 5

            // Type '!' (Shift+1)
            world.SendKey (ConsoleKeyInfo ('!', ConsoleKey.D1, true, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            state.Content |> shouldEqual "Hello!"
            state.Cursor |> shouldEqual 6

            expect {
                snapshot
                    @"
Hello!|                                 |
                                        |
                                        |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``backspace and delete edit text correctly`` () =
        task {
            let textBoxKey = NodeKey.make "textbox"

            let vdom (ctx : VdomContext) (state : State) : Vdom<DesiredBounds> =
                TextBox.make (ctx, textBoxKey, state.Content, state.Cursor, isInitiallyFocused = true)

            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.textBox textBoxKey TextEdit

            let processWorld =
                { new WorldProcessor<AppEvent, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        let mutable newState = state

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent (TextEdit action) ->
                                let content, cursor = TextBoxHelpers.applyAction state.Content state.Cursor action

                                newState <-
                                    {
                                        Content = content
                                        Cursor = cursor
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState
                }

            let clock = MockTime.make ()

            let renderState = RenderState.make console clock.GetUtcNow None

            let mutable state =
                {
                    Content = "Hello"
                    Cursor = 5
                }

            // Initial render
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            // Press Backspace
            world.SendKey (ConsoleKeyInfo ('\b', ConsoleKey.Backspace, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            state.Content |> shouldEqual "Hell"
            state.Cursor |> shouldEqual 4

            // Move cursor to 1 (between 'H' and 'e')
            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.Home, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.RightArrow, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            state.Cursor |> shouldEqual 1

            // Press Delete (should delete 'e')
            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.Delete, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            state.Content |> shouldEqual "Hll"
            state.Cursor |> shouldEqual 1

            // Press Backspace (should delete 'H')
            world.SendKey (ConsoleKeyInfo ('\b', ConsoleKey.Backspace, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            state.Content |> shouldEqual "ll"
            state.Cursor |> shouldEqual 0

            // Press Backspace again (should do nothing, at start)
            world.SendKey (ConsoleKeyInfo ('\b', ConsoleKey.Backspace, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            state.Content |> shouldEqual "ll"
            state.Cursor |> shouldEqual 0
        }

    [<Test>]
    let ``arrow keys and home end move cursor without changing content`` () =
        task {
            let textBoxKey = NodeKey.make "textbox"

            let vdom (ctx : VdomContext) (state : State) : Vdom<DesiredBounds> =
                TextBox.make (ctx, textBoxKey, state.Content, state.Cursor, isInitiallyFocused = true)

            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.textBox textBoxKey TextEdit

            let processWorld =
                { new WorldProcessor<AppEvent, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        let mutable newState = state

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent (TextEdit action) ->
                                let content, cursor = TextBoxHelpers.applyAction state.Content state.Cursor action

                                newState <-
                                    {
                                        Content = content
                                        Cursor = cursor
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState
                }

            let clock = MockTime.make ()

            let renderState = RenderState.make console clock.GetUtcNow None

            let mutable state =
                {
                    Content = "Hello"
                    Cursor = 5
                }

            // Initial render
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            // Press Left
            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.LeftArrow, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            state.Content |> shouldEqual "Hello"
            state.Cursor |> shouldEqual 4

            // Press Left
            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.LeftArrow, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            state.Content |> shouldEqual "Hello"
            state.Cursor |> shouldEqual 3

            // Press Home
            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.Home, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            state.Content |> shouldEqual "Hello"
            state.Cursor |> shouldEqual 0

            // Press Right
            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.RightArrow, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            state.Content |> shouldEqual "Hello"
            state.Cursor |> shouldEqual 1

            // Press End
            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.End, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            state.Content |> shouldEqual "Hello"
            state.Cursor |> shouldEqual 5

            // Press Right (should do nothing, at end)
            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.RightArrow, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            state.Content |> shouldEqual "Hello"
            state.Cursor |> shouldEqual 5
        }

    [<Test>]
    let ``cursor rendering maintains constant width across focus states`` () =
        task {
            let textBoxKey = NodeKey.make "textbox"

            let vdom (ctx : VdomContext) (state : State) : Vdom<DesiredBounds> =
                TextBox.make (ctx, textBoxKey, state.Content, state.Cursor)

            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.textBox textBoxKey TextEdit

            let processWorld =
                { new WorldProcessor<AppEvent, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                }

            let clock = MockTime.make ()

            let renderState = RenderState.make console clock.GetUtcNow None

            let mutable state =
                {
                    Content = "Hello"
                    Cursor = 2
                }

            // Render unfocused
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            let unfocusedOutput = ConsoleHarness.toString terminal

            expect {
                snapshot
                    @"
Hello                                   |
                                        |
                                        |
"

                return unfocusedOutput
            }

            // Focus by pressing tab (since nothing else is focusable, it will focus the textbox)
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            let focusedOutput = ConsoleHarness.toString terminal

            expect {
                snapshot
                    @"
He|llo                                  |
                                        |
                                        |
"

                return focusedOutput
            }

        // Both outputs should have the same measured width
        // Unfocused: "Hello " (6 chars: 5 content + 1 trailing space)
        // Focused: "He|llo" (6 chars: 5 content + 1 cursor)
        // This ensures zero layout jitter
        }

    [<Test>]
    let ``focused textbox uses inverted style for clear visual feedback`` () =
        task {
            let textBox1Key = NodeKey.make "textbox1"
            let textBox2Key = NodeKey.make "textbox2"

            let vdom (ctx : VdomContext) (state : State) : Vdom<DesiredBounds> =
                let textbox1 =
                    TextBox.make (ctx, textBox1Key, "Focused", 3, isInitiallyFocused = true, isFirstToFocus = true)

                let textbox2 = TextBox.make (ctx, textBox2Key, "Unfocused", 0)

                Vdom.panelSplitAuto (SplitDirection.Horizontal, textbox1, textbox2)

            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

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
                        ActivationResolver.textBox textBox1Key TextEdit
                        ActivationResolver.textBox textBox2Key TextEdit
                    ]

            let processWorld =
                { new WorldProcessor<AppEvent, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                }

            let clock = MockTime.make ()

            let renderState = RenderState.make console clock.GetUtcNow None

            let mutable state =
                {
                    Content = ""
                    Cursor = 0
                }

            // Initial render - textbox1 focused
            state <- App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom resolver

            expect {
                snapshot
                    @"
Foc|used                                |
Unfocused                               |
                                        |
"

                return ConsoleHarness.toString terminal
            }
        }
