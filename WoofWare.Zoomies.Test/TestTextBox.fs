namespace WoofWare.Zoomies.Test

open System
open System.Collections.Immutable
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

    /// Common WorldProcessor for tests that use a TextBox with State.
    /// Handles TextEdit events by applying TextBoxHelpers.applyAction.
    let textBoxProcessor : WorldProcessor<AppEvent, unit, State> =
        { new WorldProcessor<AppEvent, unit, State> with
            member _.ProcessWorld (inputs, _renderState, state) =
                let mutable newState = state

                for input in inputs do
                    match input with
                    | WorldStateChange.ApplicationEvent (TextEdit action) ->
                        let content, cursor =
                            TextBoxHelpers.applyAction newState.Content newState.Cursor action

                        newState <-
                            {
                                Content = content
                                Cursor = cursor
                            }
                    | _ -> ()

                ProcessWorldResult.make newState

            member _.ProcessPostLayoutEvents (_, _, state) = state
        }

    [<Test>]
    let ``tab moves focus from textbox to button when framework focus enabled`` () =
        task {
            let textBoxKey = NodeKey.make "textbox"
            let buttonKey = NodeKey.make "button"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
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

            let clock = MockTime.make ()

            let renderState = MockTime.makeRenderState console clock.GetUtcNow None

            let mutable state =
                {
                    Content = ""
                    Cursor = 0
                }

            // Initial render - textbox focused
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            expect {
                snapshot
                    @"
|                                       |
              [  Submit  ]              |
                                        |
"

                return ConsoleHarness.toString terminal
            }

            // Press Tab to move focus to button
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            // Verify textbox state is unchanged (Tab was not inserted as text)
            state.Content |> shouldEqual ""
            state.Cursor |> shouldEqual 0

            expect {
                snapshot
                    @"
                                        |
              [[ Submit ]]              |
                                        |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``all keystrokes including tab pass to ProcessWorld when framework focus disabled`` () =
        task {
            let textBoxKey = NodeKey.make "textbox"

            let vdom (ctx : IVdomContext<_>) (_ : ImmutableArray<ConsoleKeyInfo>) : Vdom<DesiredBounds> =
                TextBox.make (ctx, textBoxKey, "", 0, isInitiallyFocused = true)

            let console, _terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let resolver = ActivationResolver.none

            let processWorld =
                { new WorldProcessor<AppEvent, unit, ImmutableArray<_>> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        let mutable newState = state.ToBuilder ()

                        for input in inputs do
                            match input with
                            | WorldStateChange.Keystroke k -> newState.Add k
                            | _ -> ()

                        ProcessWorldResult.make (newState.ToImmutable ())

                    member _.ProcessPostLayoutEvents (_, _, state) = state
                }

            let clock = MockTime.make ()

            let renderState = MockTime.makeRenderState console clock.GetUtcNow None

            let mutable state = ImmutableArray.Empty

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

            // Press Tab
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

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

            // Press 'a'
            world.SendKey (ConsoleKeyInfo ('a', ConsoleKey.A, false, false, false))

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

            // Press Backspace
            world.SendKey (ConsoleKeyInfo ('\b', ConsoleKey.Backspace, false, false, false))

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

            // Verify all three keystrokes reached ProcessWorld
            state
            |> Seq.map _.Key
            |> Seq.toList
            |> shouldEqual [ ConsoleKey.Tab ; ConsoleKey.A ; ConsoleKey.Backspace ]
        }

    [<Test>]
    let ``typing uppercase and punctuation updates text correctly`` () =
        task {
            let textBoxKey = NodeKey.make "textbox"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
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

            let clock = MockTime.make ()

            let renderState = MockTime.makeRenderState console clock.GetUtcNow None

            let mutable state =
                {
                    Content = ""
                    Cursor = 0
                }

            // Initial render
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            // Type 'H' (Shift+h)
            world.SendKey (ConsoleKeyInfo ('H', ConsoleKey.H, true, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "H"
            state.Cursor |> shouldEqual 1

            // Type 'e'
            world.SendKey (ConsoleKeyInfo ('e', ConsoleKey.E, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "He"
            state.Cursor |> shouldEqual 2

            // Type 'l'
            world.SendKey (ConsoleKeyInfo ('l', ConsoleKey.L, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "Hel"
            state.Cursor |> shouldEqual 3

            // Type 'l'
            world.SendKey (ConsoleKeyInfo ('l', ConsoleKey.L, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "Hell"
            state.Cursor |> shouldEqual 4

            // Type 'o'
            world.SendKey (ConsoleKeyInfo ('o', ConsoleKey.O, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "Hello"
            state.Cursor |> shouldEqual 5

            // Type '!' (Shift+1)
            world.SendKey (ConsoleKeyInfo ('!', ConsoleKey.D1, true, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

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

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
                TextBox.make (ctx, textBoxKey, state.Content, state.Cursor, isInitiallyFocused = true)

            let console, _terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.textBox textBoxKey TextEdit

            let clock = MockTime.make ()

            let renderState = MockTime.makeRenderState console clock.GetUtcNow None

            let mutable state =
                {
                    Content = "Hello"
                    Cursor = 5
                }

            // Initial render
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            // Press Backspace
            world.SendKey (ConsoleKeyInfo ('\b', ConsoleKey.Backspace, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "Hell"
            state.Cursor |> shouldEqual 4

            // Move cursor to 1 (between 'H' and 'e')
            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.Home, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.RightArrow, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Cursor |> shouldEqual 1

            // Press Delete (should delete 'e')
            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.Delete, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "Hll"
            state.Cursor |> shouldEqual 1

            // Press Backspace (should delete 'H')
            world.SendKey (ConsoleKeyInfo ('\b', ConsoleKey.Backspace, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "ll"
            state.Cursor |> shouldEqual 0

            // Press Backspace again (should do nothing, at start)
            world.SendKey (ConsoleKeyInfo ('\b', ConsoleKey.Backspace, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "ll"
            state.Cursor |> shouldEqual 0
        }

    [<Test>]
    let ``arrow keys and home end move cursor without changing content`` () =
        task {
            let textBoxKey = NodeKey.make "textbox"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
                TextBox.make (ctx, textBoxKey, state.Content, state.Cursor, isInitiallyFocused = true)

            let console, _terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.textBox textBoxKey TextEdit

            let clock = MockTime.make ()

            let renderState = MockTime.makeRenderState console clock.GetUtcNow None

            let mutable state =
                {
                    Content = "Hello"
                    Cursor = 5
                }

            // Initial render
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            // Press Left
            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.LeftArrow, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "Hello"
            state.Cursor |> shouldEqual 4

            // Press Left
            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.LeftArrow, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "Hello"
            state.Cursor |> shouldEqual 3

            // Press Home
            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.Home, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "Hello"
            state.Cursor |> shouldEqual 0

            // Press Right
            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.RightArrow, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "Hello"
            state.Cursor |> shouldEqual 1

            // Press End
            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.End, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "Hello"
            state.Cursor |> shouldEqual 5

            // Press Right (should do nothing, at end)
            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.RightArrow, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "Hello"
            state.Cursor |> shouldEqual 5
        }

    [<Test>]
    let ``cursor rendering maintains constant width across focus states`` () =
        task {
            let content = "Hello"
            let cursor = 2

            // Create vdom for unfocused state
            let unfocusedVdom = TextBox.make' (content, cursor, false)

            // Create vdom for focused state
            let focusedVdom = TextBox.make' (content, cursor, true)

            // Measure both with the same constraints
            let constraints =
                {
                    MaxWidth = 1000
                    MaxHeight = 1000
                }

            let unfocusedMeasured = VdomBounds.measure unfocusedVdom constraints
            let focusedMeasured = VdomBounds.measure focusedVdom constraints

            // Both should have the same preferred width to ensure zero layout jitter
            // Unfocused: "Hello " (6 chars: 5 content + 1 trailing space)
            // Focused: "He|llo" (6 chars: 5 content + 1 cursor)
            unfocusedMeasured.PreferredWidth |> shouldEqual focusedMeasured.PreferredWidth
            unfocusedMeasured.PreferredWidth |> shouldEqual 6
        }

    [<Test>]
    let ``focused textbox renders cursor at correct position`` () =
        task {
            let textBox1Key = NodeKey.make "textbox1"
            let textBox2Key = NodeKey.make "textbox2"

            let vdom (ctx : IVdomContext<_>) (_ : State) : Vdom<DesiredBounds> =
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
                { new WorldProcessor<AppEvent, unit, State> with
                    member _.ProcessWorld (_, _, state) = ProcessWorldResult.make state
                    member _.ProcessPostLayoutEvents (_, _, state) = state
                }

            let clock = MockTime.make ()

            let renderState = MockTime.makeRenderState console clock.GetUtcNow None

            let mutable state =
                {
                    Content = ""
                    Cursor = 0
                }

            // Initial render - textbox1 focused
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

            // Note: This test verifies cursor position only. Style verification (CellStyle.inverted)
            // is not tested because ConsoleHarness doesn't capture styling information.
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

    [<Test>]
    let ``textbox handles invalid cursor positions gracefully`` () =
        task {
            let content = "Hello"

            let constraints =
                {
                    MaxWidth = 1000
                    MaxHeight = 1000
                }

            // Measure with valid cursor for comparison
            let validVdom = TextBox.make' (content, 2, true)
            let validMeasured = VdomBounds.measure validVdom constraints
            // Valid case: "Hello" with cursor = 6 width (5 content + 1 cursor)
            validMeasured.PreferredWidth |> shouldEqual 6

            // Test negative cursor position
            let negativeVdom = TextBox.make' (content, -1, true)
            // Should not throw; cursor is suppressed so width is just content (5)
            let negativeMeasured = VdomBounds.measure negativeVdom constraints
            negativeMeasured.PreferredWidth |> shouldEqual 5
            // Height should still be reasonable (1 line)
            negativeMeasured.PreferredHeightForWidth 100 |> shouldEqual 1

            // Test cursor position beyond content length
            let beyondVdom = TextBox.make' (content, 100, true)
            // Should not throw; cursor is suppressed so width is just content (5)
            let beyondMeasured = VdomBounds.measure beyondVdom constraints
            beyondMeasured.PreferredWidth |> shouldEqual 5
            // Height should still be reasonable (1 line)
            beyondMeasured.PreferredHeightForWidth 100 |> shouldEqual 1
        }

    [<Test>]
    let ``delete at end of text does nothing`` () =
        task {
            let textBoxKey = NodeKey.make "textbox"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
                TextBox.make (ctx, textBoxKey, state.Content, state.Cursor, isInitiallyFocused = true)

            let console, _terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.textBox textBoxKey TextEdit

            let clock = MockTime.make ()

            let renderState = MockTime.makeRenderState console clock.GetUtcNow None

            let mutable state =
                {
                    Content = "Hello"
                    Cursor = 5 // At end
                }

            // Initial render
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            // Press Delete (should do nothing, cursor is at end)
            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.Delete, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "Hello"
            state.Cursor |> shouldEqual 5
        }

    [<Test>]
    let ``multiple keystrokes before next pump are all processed`` () =
        // Note: With framework focus enabled, each keystroke becomes a separate TextEdit event.
        // This test verifies that multiple keystrokes queued before the next pump cycle are
        // all processed correctly, but it does NOT test true same-batch accumulation where
        // multiple TextEdit actions arrive in a single inputs collection.
        task {
            let textBoxKey = NodeKey.make "textbox"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
                TextBox.make (ctx, textBoxKey, state.Content, state.Cursor, isInitiallyFocused = true)

            let console, _terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.textBox textBoxKey TextEdit

            let clock = MockTime.make ()

            let renderState = MockTime.makeRenderState console clock.GetUtcNow None

            let mutable state =
                {
                    Content = ""
                    Cursor = 0
                }

            // Initial render
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            // Send multiple keystrokes before next pump
            world.SendKey (ConsoleKeyInfo ('H', ConsoleKey.H, true, false, false))
            world.SendKey (ConsoleKeyInfo ('i', ConsoleKey.I, false, false, false))
            world.SendKey (ConsoleKeyInfo ('!', ConsoleKey.D1, true, false, false))

            // Process all keystrokes in one pump cycle
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            // All three characters should be inserted
            state.Content |> shouldEqual "Hi!"
            state.Cursor |> shouldEqual 3
        }

    [<Test>]
    let ``Ctrl+A and Ctrl+E move cursor to beginning and end`` () =
        task {
            let textBoxKey = NodeKey.make "textbox"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
                TextBox.make (ctx, textBoxKey, state.Content, state.Cursor, isInitiallyFocused = true)

            let console, _terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.textBox textBoxKey TextEdit

            let clock = MockTime.make ()

            let renderState = MockTime.makeRenderState console clock.GetUtcNow None

            let mutable state =
                {
                    Content = "Hello World"
                    Cursor = 5 // Middle of text
                }

            // Initial render
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            // Press Ctrl+A (beginning of line)
            world.SendKey (ConsoleKeyInfo ('\001', ConsoleKey.A, false, false, true))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "Hello World"
            state.Cursor |> shouldEqual 0

            // Press Ctrl+E (end of line)
            world.SendKey (ConsoleKeyInfo ('\005', ConsoleKey.E, false, false, true))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "Hello World"
            state.Cursor |> shouldEqual 11
        }

    [<Test>]
    let ``Ctrl+B and Ctrl+F move cursor left and right`` () =
        task {
            let textBoxKey = NodeKey.make "textbox"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
                TextBox.make (ctx, textBoxKey, state.Content, state.Cursor, isInitiallyFocused = true)

            let console, _terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.textBox textBoxKey TextEdit

            let clock = MockTime.make ()

            let renderState = MockTime.makeRenderState console clock.GetUtcNow None

            let mutable state =
                {
                    Content = "Hello"
                    Cursor = 3
                }

            // Initial render
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            // Press Ctrl+B (backward)
            world.SendKey (ConsoleKeyInfo ('\002', ConsoleKey.B, false, false, true))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Cursor |> shouldEqual 2
            state.Content |> shouldEqual "Hello"

            // Press Ctrl+F (forward)
            world.SendKey (ConsoleKeyInfo ('\006', ConsoleKey.F, false, false, true))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Cursor |> shouldEqual 3
            state.Content |> shouldEqual "Hello"
        }

    [<Test>]
    let ``Ctrl+D deletes character at cursor and Ctrl+H is backspace`` () =
        task {
            let textBoxKey = NodeKey.make "textbox"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
                TextBox.make (ctx, textBoxKey, state.Content, state.Cursor, isInitiallyFocused = true)

            let console, _terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.textBox textBoxKey TextEdit

            let clock = MockTime.make ()

            let renderState = MockTime.makeRenderState console clock.GetUtcNow None

            let mutable state =
                {
                    Content = "Hello"
                    Cursor = 2
                }

            // Initial render
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            // Press Ctrl+D (delete char at cursor, should delete 'l')
            world.SendKey (ConsoleKeyInfo ('\004', ConsoleKey.D, false, false, true))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "Helo"
            state.Cursor |> shouldEqual 2

            // Press Ctrl+H (backspace, should delete 'e')
            world.SendKey (ConsoleKeyInfo ('\008', ConsoleKey.H, false, false, true))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "Hlo"
            state.Cursor |> shouldEqual 1
        }

    [<Test>]
    let ``Ctrl+K deletes from cursor to end of line`` () =
        task {
            let textBoxKey = NodeKey.make "textbox"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
                TextBox.make (ctx, textBoxKey, state.Content, state.Cursor, isInitiallyFocused = true)

            let console, _terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.textBox textBoxKey TextEdit

            let clock = MockTime.make ()

            let renderState = MockTime.makeRenderState console clock.GetUtcNow None

            let mutable state =
                {
                    Content = "Hello World"
                    Cursor = 5
                }

            // Initial render
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            // Press Ctrl+K (kill to end)
            world.SendKey (ConsoleKeyInfo ('\011', ConsoleKey.K, false, false, true))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "Hello"
            state.Cursor |> shouldEqual 5
        }

    [<Test>]
    let ``Ctrl+U deletes from cursor to beginning of line`` () =
        task {
            let textBoxKey = NodeKey.make "textbox"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
                TextBox.make (ctx, textBoxKey, state.Content, state.Cursor, isInitiallyFocused = true)

            let console, _terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.textBox textBoxKey TextEdit

            let clock = MockTime.make ()

            let renderState = MockTime.makeRenderState console clock.GetUtcNow None

            let mutable state =
                {
                    Content = "Hello World"
                    Cursor = 6
                }

            // Initial render
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            // Press Ctrl+U (kill to beginning)
            world.SendKey (ConsoleKeyInfo ('\021', ConsoleKey.U, false, false, true))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "World"
            state.Cursor |> shouldEqual 0
        }

    [<Test>]
    let ``Ctrl+W deletes word backward`` () =
        task {
            let textBoxKey = NodeKey.make "textbox"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
                TextBox.make (ctx, textBoxKey, state.Content, state.Cursor, isInitiallyFocused = true)

            let console, _terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let resolver = ActivationResolver.textBox textBoxKey TextEdit

            let clock = MockTime.make ()

            let renderState = MockTime.makeRenderState console clock.GetUtcNow None

            let mutable state =
                {
                    Content = "Hello World Test"
                    Cursor = 11 // After "World"
                }

            // Initial render
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            // Press Ctrl+W (delete word backward, should delete "World")
            world.SendKey (ConsoleKeyInfo ('\023', ConsoleKey.W, false, false, true))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual "Hello  Test"
            state.Cursor |> shouldEqual 6

            // Press Ctrl+W again (should delete "Hello")
            world.SendKey (ConsoleKeyInfo ('\023', ConsoleKey.W, false, false, true))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    textBoxProcessor
                    vdom
                    resolver
                    (fun () -> false)

            state.Content |> shouldEqual " Test"
            state.Cursor |> shouldEqual 0
        }
