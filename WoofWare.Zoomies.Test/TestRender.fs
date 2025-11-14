namespace WoofWare.Zoomies.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies

type State =
    {
        IsToggle1Checked : bool
        IsToggle2Checked : bool
    }

    static member Empty () : State =
        {
            IsToggle1Checked = false
            IsToggle2Checked = false
        }

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestRender =
    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    let vdom (vdomContext : VdomContext) (state : State) : Vdom<DesiredBounds, _> =
        let left =
            Vdom.textContent
                false
                "not praising the praiseworthy keeps people uncompetitive; not prizing rare treasures keeps people from stealing; not looking at the desirable keeps the mind quiet"
            |> Vdom.bordered

        let right =
            Vdom.textContent
                false
                "errybody wants to be a bodybuilder, but don't nobody want to lift no heavy-ass weights"
            |> Vdom.bordered

        let topHalf = Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, left, right)

        let toggle1Key = NodeKey.make "toggle1"
        let currentFocus = VdomContext.focusedKey vdomContext

        let bottomHalf =
            Components.LabelledCheckbox.make
                (currentFocus = Some toggle1Key)
                state.IsToggle1Checked
                "Press Space to toggle"
            |> Vdom.withKey toggle1Key
            |> Vdom.withFocusTracking

        let vdom =
            Vdom.panelSplitAbsolute (SplitDirection.Horizontal, -3, topHalf, bottomHalf)

        if state.IsToggle1Checked then
            let toggle2Key = NodeKey.make "toggle2"

            let inner =
                Vdom.panelSplitProportion (
                    SplitDirection.Vertical,
                    0.5,
                    Vdom.textContent false "only displayed when checked",
                    Components.LabelledCheckbox.make
                        (currentFocus = Some toggle2Key)
                        state.IsToggle2Checked
                        "this one is focusable!"
                    |> Vdom.withKey toggle2Key
                    |> Vdom.withFocusTracking
                )

            Vdom.panelSplitProportion (SplitDirection.Horizontal, 0.7, vdom, inner)
        else
            vdom

    [<Test>]
    let ``there is no rerender if nothing changes`` () =
        let terminalOps = ResizeArray ()

        let console =
            { IConsole.defaultForTests with
                Execute = fun x -> terminalOps.Add x
            }

        let state = State.Empty ()

        let renderState = RenderState.make console None

        Render.oneStep renderState state (vdom (RenderState.vdomContext renderState))

        terminalOps.Clear ()

        Render.oneStep renderState state (vdom (RenderState.vdomContext renderState))

        terminalOps |> shouldBeEmpty

    [<Test>]
    let ``example 1`` () =
        let processWorld =
            { new WorldProcessor<unit, State> with
                member _.ProcessWorld (worldChanges, renderState, state) =
                    let focusedKey = VdomContext.focusedKey renderState
                    let mutable newState = state

                    for change in worldChanges do
                        match change with
                        | Keystroke c when c.KeyChar = ' ' ->
                            match focusedKey with
                            | Some key when key = NodeKey.make "toggle1" ->
                                newState <-
                                    { newState with
                                        IsToggle1Checked = not newState.IsToggle1Checked
                                    }
                            | Some key when key = NodeKey.make "toggle2" ->
                                newState <-
                                    { newState with
                                        IsToggle2Checked = not newState.IsToggle2Checked
                                    }
                            | _ -> ()
                        | Keystroke _ -> ()
                        | KeyboardEvent _ -> failwith "no keyboard events"
                        | MouseEvent _ -> failwith "no mouse events"
                        | ApplicationEvent () -> failwith "no app events"
                        | ApplicationEventException _ -> failwith "no exceptions possible"

                    ProcessWorldResult.make newState
            }

        task {
            let console, terminal = ConsoleHarness.make ()

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let mutable state = State.Empty ()

            let renderState = RenderState.make console None

            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
┌──────────────────────────────────────┐┌──────────────────────────────────────┐|
│not praising the praiseworthy keeps pe││errybody wants to be a bodybuilder, bu│|
│ople uncompetitive; not prizing rare t││t don't nobody want to lift no heavy-a│|
│reasures keeps people from stealing; n││ss weights                            │|
│ot looking at the desirable keeps the ││                                      │|
│mind quiet                            ││                                      │|
└──────────────────────────────────────┘└──────────────────────────────────────┘|
   Press Space to toggle                                                        |
 ☐                                                                              |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Switching focus moves focus to the first focusable element
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
┌──────────────────────────────────────┐┌──────────────────────────────────────┐|
│not praising the praiseworthy keeps pe││errybody wants to be a bodybuilder, bu│|
│ople uncompetitive; not prizing rare t││t don't nobody want to lift no heavy-a│|
│reasures keeps people from stealing; n││ss weights                            │|
│ot looking at the desirable keeps the ││                                      │|
│mind quiet                            ││                                      │|
└──────────────────────────────────────┘└──────────────────────────────────────┘|
   Press Space to toggle                                                        |
[☐]                                                                             |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Switching focus again does nothing because there are no more focusable elements
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
┌──────────────────────────────────────┐┌──────────────────────────────────────┐|
│not praising the praiseworthy keeps pe││errybody wants to be a bodybuilder, bu│|
│ople uncompetitive; not prizing rare t││t don't nobody want to lift no heavy-a│|
│reasures keeps people from stealing; n││ss weights                            │|
│ot looking at the desirable keeps the ││                                      │|
│mind quiet                            ││                                      │|
└──────────────────────────────────────┘└──────────────────────────────────────┘|
   Press Space to toggle                                                        |
[☐]                                                                             |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Turn on the toggle, revealing a new interface element!
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
┌──────────────────────────────────────┐┌──────────────────────────────────────┐|
│not praising the praiseworthy keeps pe││errybody wants to be a bodybuilder, bu│|
│ople uncompetitive; not prizing rare t││t don't nobody want to lift no heavy-a│|
└──────────────────────────────────────┘└──────────────────────────────────────┘|
   Press Space to toggle                                                        |
[☑]                                                                             |
                                                                                |
only displayed when checked                this one is focusable!               |
                                         ☐                                      |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Switch to the other checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
┌──────────────────────────────────────┐┌──────────────────────────────────────┐|
│not praising the praiseworthy keeps pe││errybody wants to be a bodybuilder, bu│|
│ople uncompetitive; not prizing rare t││t don't nobody want to lift no heavy-a│|
└──────────────────────────────────────┘└──────────────────────────────────────┘|
   Press Space to toggle                                                        |
 ☑                                                                              |
                                                                                |
only displayed when checked                this one is focusable!               |
                                        [☐]                                     |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Toggle the other one on
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
┌──────────────────────────────────────┐┌──────────────────────────────────────┐|
│not praising the praiseworthy keeps pe││errybody wants to be a bodybuilder, bu│|
│ople uncompetitive; not prizing rare t││t don't nobody want to lift no heavy-a│|
└──────────────────────────────────────┘└──────────────────────────────────────┘|
   Press Space to toggle                                                        |
 ☑                                                                              |
                                                                                |
only displayed when checked                this one is focusable!               |
                                        [☑]                                     |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Switch back to the first one
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
┌──────────────────────────────────────┐┌──────────────────────────────────────┐|
│not praising the praiseworthy keeps pe││errybody wants to be a bodybuilder, bu│|
│ople uncompetitive; not prizing rare t││t don't nobody want to lift no heavy-a│|
└──────────────────────────────────────┘└──────────────────────────────────────┘|
   Press Space to toggle                                                        |
[☑]                                                                             |
                                                                                |
only displayed when checked                this one is focusable!               |
                                         ☑                                      |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Disable it again
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
┌──────────────────────────────────────┐┌──────────────────────────────────────┐|
│not praising the praiseworthy keeps pe││errybody wants to be a bodybuilder, bu│|
│ople uncompetitive; not prizing rare t││t don't nobody want to lift no heavy-a│|
│reasures keeps people from stealing; n││ss weights                            │|
│ot looking at the desirable keeps the ││                                      │|
│mind quiet                            ││                                      │|
└──────────────────────────────────────┘└──────────────────────────────────────┘|
   Press Space to toggle                                                        |
[☐]                                                                             |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Re-enable; it remembered its state
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
┌──────────────────────────────────────┐┌──────────────────────────────────────┐|
│not praising the praiseworthy keeps pe││errybody wants to be a bodybuilder, bu│|
│ople uncompetitive; not prizing rare t││t don't nobody want to lift no heavy-a│|
└──────────────────────────────────────┘└──────────────────────────────────────┘|
   Press Space to toggle                                                        |
[☑]                                                                             |
                                                                                |
only displayed when checked                this one is focusable!               |
                                         ☑                                      |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``focusable text content can gain focus`` () =
        task {
            let console, terminal = ConsoleHarness.make ()

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (vdomContext : VdomContext) (_ : FakeUnit) =
                let currentFocus = VdomContext.focusedKey vdomContext
                let textKey = NodeKey.make "focusable-text"
                let checkboxKey = NodeKey.make "checkbox"

                let text =
                    Vdom.textContent (currentFocus = Some textKey) "This is focusable text"
                    |> Vdom.withKey textKey
                    |> Vdom.withFocusTracking

                let checkbox =
                    Vdom.checkbox (currentFocus = Some checkboxKey) false
                    |> Vdom.withKey checkboxKey
                    |> Vdom.withFocusTracking

                Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 3, text, checkbox)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) =
                        for change in worldChanges do
                            match change with
                            | Keystroke _ -> ()
                            | KeyboardEvent _ -> failwith "no keyboard events"
                            | MouseEvent _ -> failwith "no mouse events"
                            | ApplicationEvent () -> failwith "no app events"
                            | ApplicationEventException _ -> failwith "no exceptions possible"

                        ProcessWorldResult.make state
                }

            let renderState = RenderState.make console None

            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
This is focusable text                                                          |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                        ☐                                       |
                                                                                |
                                                                                |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Tab to focus the text
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
This is focusable text                                                          |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                        ☐                                       |
                                                                                |
                                                                                |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Tab to focus the checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
This is focusable text                                                          |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                       [☐]                                      |
                                                                                |
                                                                                |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Tab back to text
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
This is focusable text                                                          |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                        ☐                                       |
                                                                                |
                                                                                |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``initial focus is respected`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (vdomContext : VdomContext) (_ : FakeUnit) =
                let currentFocus = VdomContext.focusedKey vdomContext
                let checkbox1Key = NodeKey.make "checkbox1"
                let checkbox2Key = NodeKey.make "checkbox2"
                let checkbox3Key = NodeKey.make "checkbox3"

                let checkbox1 =
                    Vdom.checkbox (currentFocus = Some checkbox1Key) false
                    |> Vdom.withKey checkbox1Key
                    |> Vdom.withFocusTracking

                let checkbox2 =
                    Vdom.checkbox (currentFocus = Some checkbox2Key) false
                    |> Vdom.withKey checkbox2Key
                    |> fun v -> Vdom.withFocusTracking (v, isInitialFocus = true)

                let checkbox3 =
                    Vdom.checkbox (currentFocus = Some checkbox3Key) false
                    |> Vdom.withKey checkbox3Key
                    |> Vdom.withFocusTracking

                Vdom.panelSplitProportion (
                    SplitDirection.Vertical,
                    0.33,
                    checkbox1,
                    Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, checkbox2, checkbox3)
                )

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) =
                        for change in worldChanges do
                            match change with
                            | Keystroke _ -> ()
                            | KeyboardEvent _ -> failwith "no keyboard events"
                            | MouseEvent _ -> failwith "no mouse events"
                            | ApplicationEvent () -> failwith "no app events"
                            | ApplicationEventException _ -> failwith "no exceptions possible"

                        ProcessWorldResult.make state
                }

            let renderState = RenderState.make console None

            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
                                                                                |
             ☐                         ☐                          ☐             |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Tab should focus checkbox2 (marked with isInitialFocus=true), not checkbox1
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
                                                                                |
             ☐                        [☐]                         ☐             |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Tab again should cycle to checkbox3
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
                                                                                |
             ☐                         ☐                         [☐]            |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Tab again should cycle to checkbox1
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
                                                                                |
            [☐]                        ☐                          ☐             |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Tab again should cycle back to checkbox2
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
                                                                                |
             ☐                        [☐]                         ☐             |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``PanelSplit does not repaint background when only child changes`` () =
        let terminalOps = ResizeArray<TerminalOp> ()

        let panelWidth = 15

        let console =
            { IConsole.defaultForTests with
                Execute = fun x -> terminalOps.Add x
                WindowWidth = fun _ -> 2 * panelWidth
                WindowHeight = fun _ -> 3
            }

        let renderState = RenderState.make console None

        // Create vdom with a PanelSplit where a bordered child changes
        // Bordered doesn't repaint its entire area, so we can detect if PanelSplit is adding extra repaints
        let vdom (text : string) =
            let left = Vdom.textContent false text |> Vdom.bordered
            let right = Vdom.textContent false "static" |> Vdom.bordered
            Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, left, right)

        // First render
        Render.oneStep renderState () (fun _ -> vdom "initial")

        // Clear ops from first render
        terminalOps.Clear ()

        // Change only the left child's text content
        Render.oneStep renderState () (fun _ -> vdom "changed")

        // Collect all the cells that were written to
        let writtenCells =
            let result = ResizeArray<int * int> ()

            for i = 0 to terminalOps.Count - 1 do
                match terminalOps.[i] with
                | TerminalOp.MoveCursor (x, y) ->
                    if i + 1 < terminalOps.Count then
                        match terminalOps.[i + 1] with
                        | TerminalOp.WriteChar _ -> result.Add (x, y)
                        | _ -> ()
                | _ -> ()

            result

        // The left bordered text changed, but not the right.
        // (The border cells don't repaint, which means the entire first and last row don't repaint, nor do the leftmost
        // and rightmost single cells of the middle row.)

        // We could do better here by diffing the text so we don't repaint the unchanged characters;
        // there's a TODO for that in the code.
        writtenCells.Count |> shouldEqual (panelWidth - 2)

    [<Test>]
    let ``Bordered does not repaint border when only child changes`` () =
        let terminalOps = ResizeArray<TerminalOp> ()

        let panelWidth = 20

        let console =
            { IConsole.defaultForTests with
                Execute = fun x -> terminalOps.Add x
                WindowWidth = fun _ -> panelWidth
                WindowHeight = fun _ -> 3
            }

        let renderState = RenderState.make console None

        // Create vdom with bordered text that can change
        let vdom (content : string) =
            Vdom.textContent false content |> Vdom.bordered

        // First render
        Render.oneStep renderState () (fun _ -> vdom "initial text")

        // Clear ops from first render
        terminalOps.Clear ()

        // Change only the text content inside the border
        Render.oneStep renderState () (fun _ -> vdom "changed text")

        // Collect all the cells that were written to
        let writtenCells = ResizeArray<int * int> ()

        for i = 0 to terminalOps.Count - 1 do
            match terminalOps.[i] with
            | TerminalOp.MoveCursor (x, y) ->
                if i + 1 < terminalOps.Count then
                    match terminalOps.[i + 1] with
                    | TerminalOp.WriteChar _ -> writtenCells.Add (x, y)
                    | _ -> ()
            | _ -> ()

        // The text content changed, so we should write to text cells
        // But we should NOT repaint the border (which would be the perimeter cells)

        let borderCells =
            writtenCells
            |> Seq.filter (fun (x, y) -> x = 0 || x = panelWidth - 1 || y = 0 || y = 2)
            |> Seq.length

        // We should not have written to any border cells
        borderCells |> shouldEqual 0

        // And we changed the right number of cells: just the middle row, because that's the one that doesn't contain
        // the border.
        // Again, this will get smaller when we've implemented a more efficient text render with diffing.
        writtenCells.Count |> shouldEqual (panelWidth - 2)

    [<Test>]
    let ``SetCursorInvisible is only called once per render`` () =
        let terminalOps = ResizeArray<TerminalOp> ()

        let console =
            { IConsole.defaultForTests with
                Execute = fun x -> terminalOps.Add x
                WindowWidth = fun _ -> 20
                WindowHeight = fun _ -> 5
            }

        let renderState = RenderState.make console None

        // Create vdom with some content that will result in multiple cells being written
        let vdom =
            Vdom.textContent false "This is some text that spans multiple cells"
            |> Vdom.bordered

        // Do a render
        Render.oneStep renderState () (fun _ -> vdom)

        // Count how many SetCursorVisibility false operations were emitted
        let setCursorInvisibleCount =
            terminalOps
            |> Seq.filter (
                function
                | TerminalOp.SetCursorVisibility false -> true
                | _ -> false
            )
            |> Seq.length

        // Should only be called once
        setCursorInvisibleCount |> shouldEqual 1

    [<Test>]
    let ``layoutOf works for unchanged keyed nodes across multiple renders`` () =
        let terminalOps = ResizeArray ()

        let console =
            { IConsole.defaultForTests with
                Execute = fun x -> terminalOps.Add x
            }

        let renderState = RenderState.make console None

        let key = NodeKey.make "test-key"

        // Create a vdom with a keyed node - memoize it so we get the same reference each time
        // Use a wrapper (Bordered) to ensure we have an Unkeyed vdom as required by oneStep
        let cachedKeyedContent = Vdom.textContent false "test content" |> Vdom.withKey key

        let vdom (_ : FakeUnit) : Vdom<DesiredBounds, _> =
            // Wrap the keyed content in a bordered panel (which is Unkeyed)
            Vdom.bordered cachedKeyedContent

        // First render
        Render.oneStep renderState (FakeUnit.fake ()) vdom

        // layoutOf should work after first render
        let layout1 = RenderState.layoutOf key renderState
        layout1.IsSome |> shouldEqual true

        // Second render with the same vdom (should trigger early cutoff on the keyed node)
        Render.oneStep renderState (FakeUnit.fake ()) vdom

        // layoutOf should still work after second render, even though early cutoff was taken
        let layout2 = RenderState.layoutOf key renderState
        layout2.IsSome |> shouldEqual true

    [<Test>]
    let ``Keyed PanelSplit clears background on initial render`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let splitKey = NodeKey.make "split"

            let vdom (vdomContext : VdomContext) (showSplit : bool) =
                if showSplit then
                    // A keyed PanelSplit with small children
                    // The background should be cleared
                    let left = Vdom.textContent false "L"
                    let right = Vdom.textContent false "R"

                    let split =
                        Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, left, right)
                        |> Vdom.withKey splitKey
                    // Wrap in bordered to make it Unkeyed at the top level
                    Vdom.bordered split
                else
                    // Fill the screen with characters to create "artifacts"
                    Vdom.textContent false "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                    |> Vdom.bordered

            let processWorld =
                { new WorldProcessor<unit, bool> with
                    member _.ProcessWorld (worldChanges, _, state) =
                        // Toggle state on any keystroke
                        let newState = if worldChanges.Length > 0 then not state else state
                        ProcessWorldResult.make newState
                }

            let renderState = RenderState.make console None

            // First render: fill with X's
            let mutable state =
                App.pumpOnce worldFreezer false (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
┌──────────────────────────────────────┐|
│XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX│|
│XX                                    │|
│                                      │|
└──────────────────────────────────────┘|
"

                return ConsoleHarness.toString terminal
            }

            // Send a keystroke to trigger state change
            world.SendKey (ConsoleKeyInfo ('x', ConsoleKey.NoName, false, false, false))

            // Second render: show keyed PanelSplit
            // The X's should be cleared (replaced with spaces), not left as artifacts
            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
┌──────────────────────────────────────┐|
│L                  R                  │|
│                                      │|
│                                      │|
└──────────────────────────────────────┘|
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``Keyed Bordered draws its border`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let borderedKey = NodeKey.make "bordered"

            let vdom (vdomContext : VdomContext) (showBordered : bool) =
                if showBordered then
                    // A keyed Bordered with small text content
                    // The border should be drawn
                    let content = Vdom.textContent false "content"

                    let keyedBordered = Vdom.bordered content |> Vdom.withKey borderedKey
                    // Wrap in another bordered to make it Unkeyed at the top level
                    Vdom.bordered keyedBordered
                else
                    // Fill the screen with characters to create "artifacts"
                    Vdom.textContent false "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

            let processWorld =
                { new WorldProcessor<unit, bool> with
                    member _.ProcessWorld (worldChanges, _, state) =
                        // Toggle state on any keystroke
                        let newState = if worldChanges.Length > 0 then not state else state
                        ProcessWorldResult.make newState
                }

            let renderState = RenderState.make console None

            // First render: fill with X's
            let mutable state =
                App.pumpOnce worldFreezer false (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX|
                                        |
                                        |
                                        |
                                        |
"

                return ConsoleHarness.toString terminal
            }

            // Send a keystroke to trigger state change
            world.SendKey (ConsoleKeyInfo ('x', ConsoleKey.NoName, false, false, false))

            // Second render: show keyed Bordered
            // The border should be drawn, and the X's should be cleared
            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
┌──────────────────────────────────────┐|
│┌────────────────────────────────────┐│|
││content                             ││|
│└────────────────────────────────────┘│|
└──────────────────────────────────────┘|
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``Checkbox with focus does not write brackets when Height is 0`` () =
        task {
            // Regression test for: "Checkbox focus brackets can write with Height=0"
            // The guard should check bounds.Width >= 3 && bounds.Height > 0 before drawing brackets
            let terminalOps = ResizeArray<TerminalOp> ()

            let console =
                { IConsole.defaultForTests with
                    Execute = fun x -> terminalOps.Add x
                    WindowWidth = fun _ -> 10
                    WindowHeight = fun _ -> 5
                }

            let renderState = RenderState.make console None

            let checkboxKey = NodeKey.make "checkbox"

            // Create a vdom where the checkbox has focus and is allocated bounds with Height=0
            // We use an absolute split to force the checkbox into a zero-height allocation
            let vdom (vdomContext : VdomContext) (_ : FakeUnit) =
                let currentFocus = VdomContext.focusedKey vdomContext
                let topContent = Vdom.textContent false "top"

                let checkbox =
                    Vdom.checkbox (currentFocus = Some checkboxKey) false
                    |> Vdom.withKey checkboxKey
                    |> Vdom.withFocusTracking

                // Give the checkbox 0 rows (split at row 5 in a 5-row terminal)
                Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 5, topContent, checkbox)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            // Render without focus
            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            terminalOps.Clear ()

            // Tab to give focus to the checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            // Check that the checkbox has bounds with Height=0
            let checkboxLayout = RenderState.layoutOf checkboxKey renderState
            checkboxLayout.IsSome |> shouldEqual true
            checkboxLayout.Value.Height |> shouldEqual 0

            // The bug would cause bracket characters '[' or ']' to be written at invalid positions
            // With the fix, no brackets should be written when Height=0
            let hasBrackets =
                terminalOps
                |> Seq.exists (
                    function
                    | TerminalOp.WriteChar cell when cell.Char = '[' || cell.Char = ']' -> true
                    | _ -> false
                )

            hasBrackets |> shouldEqual false
        }

    [<Test>]
    let ``Text rendering handles zero-width bounds without error`` () =
        task {
            // Regression test for: "Text rendering does not handle zero-size bounds"
            // With Width=0 (from a proportion split), rendering can write off-bounds and throw
            let terminalOps = ResizeArray<TerminalOp> ()

            // Use a very small terminal width so that after splitting, one side has 0 width
            let console =
                { IConsole.defaultForTests with
                    Execute = fun x -> terminalOps.Add x
                    WindowWidth = fun _ -> 1
                    WindowHeight = fun _ -> 5
                }

            let renderState = RenderState.make console None

            // Create a vdom where text content has Width=0
            // With a terminal width of 1 and a 50/50 split, each side gets 0 or 1 width
            let vdom (_ : VdomContext) (_ : FakeUnit) =
                let leftText = Vdom.textContent false "some text content"
                let rightText = Vdom.textContent false "other text"
                // Split with 0.5 proportion, terminal has width 1, so left gets 0 width
                Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, leftText, rightText)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            // This should not throw an IndexOutOfRangeException
            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>
        }

    [<TestCase true>]
    [<TestCase false>]
    let ``Keyed text rendering handles zero-width bounds without error`` (leftIsKeyed : bool) =
        task {
            // Regression test for: "Text rendering does not handle zero-size bounds"
            // Test the keyed branch of text rendering
            let terminalOps = ResizeArray<TerminalOp> ()

            // Use a very small terminal width so that after splitting, one side has 0 width
            let console =
                { IConsole.defaultForTests with
                    Execute = fun x -> terminalOps.Add x
                    WindowWidth = fun _ -> 1
                    WindowHeight = fun _ -> 5
                }

            let renderState = RenderState.make console None

            let textKey = NodeKey.make "text"

            // Create a vdom where keyed text content has Width=0
            // With a terminal width of 1 and a 50/50 split, each side gets 0 or 1 width
            let vdom (_ : VdomContext) (_ : FakeUnit) =
                let leftText = Vdom.textContent false "some text content"
                let rightText = Vdom.textContent false "other text"

                // Split with 0.5 proportion so that one side gets 0 width
                if leftIsKeyed then
                    Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, Vdom.withKey textKey leftText, rightText)
                else
                    Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, leftText, Vdom.withKey textKey rightText)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            // This should not throw an IndexOutOfRangeException
            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>
        }

    [<Test>]
    let ``Proportion splits can yield zero-size children`` () =
        task {
            // Demonstrates that proportion splits with small terminals can allocate zero width/height
            let console, _ = ConsoleHarness.make' (fun () -> 1) (fun () -> 1)

            let renderState = RenderState.make console None

            let leftKey = NodeKey.make "left"
            let rightKey = NodeKey.make "right"

            // Create a vdom with proportion split
            // Terminal width is 1, split at 0.1 proportion
            // This means left gets: int (float 1 * 0.1) = int 0.1 = 0
            // And right gets: 1 - 0 = 1
            let vdom (_ : VdomContext) (_ : FakeUnit) =
                let leftText = Vdom.textContent false "left" |> Vdom.withKey leftKey
                let rightText = Vdom.textContent false "right" |> Vdom.withKey rightKey
                Vdom.panelSplitProportion (SplitDirection.Vertical, 0.1, leftText, rightText)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            // Render
            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            // Check that the left child has zero width
            match RenderState.layoutOf leftKey renderState with
            | None -> failwith "should have found leftKey node"
            | Some leftLayout -> leftLayout.Width |> shouldEqual 0

            // Check that the right child has the remaining width
            match RenderState.layoutOf rightKey renderState with
            | None -> failwith "should have found rightKey node"
            | Some rightLayout -> rightLayout.Width |> shouldEqual 1
        }

    [<Test>]
    let ``Horizontal proportion splits can yield zero-size children`` () =
        task {
            // Same as above but for horizontal splits
            let console, _ = ConsoleHarness.make' (fun () -> 10) (fun () -> 2)

            let renderState = RenderState.make console None

            let topKey = NodeKey.make "top"
            let bottomKey = NodeKey.make "bottom"

            // Create a vdom with horizontal proportion split
            // Terminal height is 2, split at 0.1 proportion
            // This means top gets: int (float 2 * 0.1) = int 0.2 = 0
            // And bottom gets: 2 - 0 = 2
            let vdom (_ : VdomContext) (_ : FakeUnit) =
                let topText = Vdom.textContent false "top" |> Vdom.withKey topKey
                let bottomText = Vdom.textContent false "bottom" |> Vdom.withKey bottomKey
                Vdom.panelSplitProportion (SplitDirection.Horizontal, 0.1, topText, bottomText)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            // Render
            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            // Check that the top child has zero height
            match RenderState.layoutOf topKey renderState with
            | None -> failwith "should have found topKey node"
            | Some topLayout -> topLayout.Height |> shouldEqual 0

            // Check that the bottom child has the remaining height
            match RenderState.layoutOf bottomKey renderState with
            | None -> failwith "should have found bottomKey node"
            | Some bottomLayout -> bottomLayout.Height |> shouldEqual 2
        }

    [<Test>]
    let ``panelSplitAuto distributes space normally when all components fit`` () =
        task {
            // Test the normal case where there's enough space for all preferred sizes
            let console, terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Create two text components with different preferred widths
                // "Hello world" has preferred width ~11, "Hi" has preferred width ~2
                let left = Vdom.textContent false "Hello world"
                let right = Vdom.textContent false "Hi"

                Vdom.panelSplitAuto (SplitDirection.Vertical, left, right)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console None

            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
Hello world                                                        Hi           |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``panelSplitAuto handles stress when not enough space for all preferences`` () =
        task {
            // Test when available space is between minimums and preferences
            // Terminal is 20 wide, but text wants more
            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Long text content that will wrap when space is limited.
                // The longer one (`left`) has its longest word of length 6, vs the shorter one having longest word
                // of length 4, so satisfying their minimum requests allocates more space to `left`;
                // then leftover spare space gets allocated equally between them, and `left` ends up a bit bigger.
                let left = Vdom.textContent false "This is a longer piece of text"
                let right = Vdom.textContent false "Also long text here"

                Vdom.panelSplitAuto (SplitDirection.Vertical, left, right)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console None

            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
This is a lAlso long|
onger piece text her|
 of text   e        |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``panelSplitAuto handles extreme stress when not enough space for minimums`` () =
        task {
            // Test when available space is less than sum of minimums
            // This forces proportional scaling of minimums
            let console, terminal = ConsoleHarness.make' (fun () -> 8) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Words with minimum widths that exceed available space
                let left = Vdom.textContent false "Hello" // min width ~5
                let right = Vdom.textContent false "World" // min width ~5

                Vdom.panelSplitAuto (SplitDirection.Vertical, left, right)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console None

            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
HellWorl|
o   d   |
        |
        |
        |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``panelSplitAuto works with horizontal splits in normal conditions`` () =
        task {
            // Test horizontal auto splits with enough space
            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Short text (prefers 1 line) and longer text (prefers multiple lines)
                let top = Vdom.textContent false "Short"

                let bottom =
                    Vdom.textContent false "This is a much longer text that will take multiple lines when rendered"

                Vdom.panelSplitAuto (SplitDirection.Horizontal, top, bottom)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console None

            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
Short                                   |
This is a much longer text that will tak|
e multiple lines when rendered          |
                                        |
                                        |
                                        |
                                        |
                                        |
                                        |
                                        |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``panelSplitAuto handles horizontal stress when height is limited but still sufficient`` () =
        task {
            // Test horizontal auto splits when not enough vertical space
            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 4)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Both components want more height than available
                let top = Vdom.textContent false "Top section with some content that wraps around"
                let bottom = Vdom.textContent false "Bottom section also with content"

                Vdom.panelSplitAuto (SplitDirection.Horizontal, top, bottom)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console None

            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
Top section with some content |
that wraps around             |
Bottom section also with conte|
nt                            |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``panelSplitAuto with bordered components shows layout adaptation`` () =
        task {
            // Test auto splits with bordered components to make the allocation visible
            let console, terminal = ConsoleHarness.make' (fun () -> 50) (fun () -> 8)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Bordered panels to clearly show space allocation
                let left = Vdom.textContent false "Small" |> Vdom.bordered

                let right = Vdom.textContent false "This is much larger content" |> Vdom.bordered

                Vdom.panelSplitAuto (SplitDirection.Vertical, left, right)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console None

            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
┌───────┐┌───────────────────────────────────────┐|
│Small  ││This is much larger content            │|
│       ││                                       │|
│       ││                                       │|
│       ││                                       │|
│       ││                                       │|
│       ││                                       │|
└───────┘└───────────────────────────────────────┘|
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``panelSplitAuto handles zero-width allocation gracefully`` () =
        task {
            // Test extreme stress where one component gets zero space
            let console, terminal = ConsoleHarness.make' (fun () -> 1) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                let left = Vdom.textContent false "A"
                let right = Vdom.textContent false "B"

                Vdom.panelSplitAuto (SplitDirection.Vertical, left, right)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console None

            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
B|
 |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``regression test for wordWrapCount underestimating height for long words that exceed width`` () =
        task {
            // Test case: a single long word that exceeds the available width
            // wordWrapCount treats it as 1 line (places the whole word on one line)
            // But rendering wraps character-by-character, so it actually takes multiple lines
            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // A 25-character word in a 10-character wide terminal
                // Should wrap to 3 lines: "AAAAAAAAAA" + "AAAAAAAAAA" + "AAAAA"
                let longWord = String.replicate 25 "A"
                let text = Vdom.textContent false longWord

                // Put it in an auto split with another component
                let bottom = Vdom.textContent false "bottom"
                Vdom.panelSplitAuto (SplitDirection.Horizontal, text, bottom)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console None

            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
AAAAAAAAAA|
AAAAAAAAAA|
AAAAA     |
bottom    |
          |
          |
          |
          |
          |
          |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``Unkeyed PanelSplit clears exposed area when child shrinks`` () =
        task {
            // Regression test for: "PanelSplit background only cleared when container node/bounds change"
            // When a PanelSplit rebalances (same container bounds, different child bounds),
            // exposed areas should be cleared
            let console, terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (useLargeSplit : bool) =
                if useLargeSplit then
                    // 50/50 split - left side filled with X's
                    let left = Vdom.textContent false (String.replicate 200 "X")
                    let right = Vdom.textContent false "right"
                    Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, left, right)
                else
                    // 25/75 split - left side has only "AAA"
                    // Bug: the area between where "AAA" ends and where the old 50% split was
                    // still contained X's from the previous render
                    let left = Vdom.textContent false "AAA"
                    let right = Vdom.textContent false "right"
                    Vdom.panelSplitProportion (SplitDirection.Vertical, 0.25, left, right)

            let processWorld =
                { new WorldProcessor<unit, bool> with
                    member _.ProcessWorld (worldChanges, _, state) =
                        let newState = if worldChanges.Length > 0 then not state else state
                        ProcessWorldResult.make newState
                }

            let renderState = RenderState.make console None

            // First render: 50/50 split with X's filling the left side
            let mutable state =
                App.pumpOnce worldFreezer true (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXright                                   |
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                        |
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                        |
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                        |
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                        |
"

                return ConsoleHarness.toString terminal
            }

            // Send keystroke to trigger rebalance
            world.SendKey (ConsoleKeyInfo ('x', ConsoleKey.NoName, false, false, false))

            // Second render: 25/75 split with only "AAA" on left
            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            // For reference, although the actual test assertion comes afterwards:
            let secondRender = ConsoleHarness.toString terminal

            expect {
                snapshot
                    @"
AAA                 right                                                       |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
"

                return secondRender
            }

            // The bug: stale X's remain in the area between "AAA" and where the right panel starts
            if secondRender.Contains 'X' then
                failwith $"Bug detected: Second render contains stale X's. %s{secondRender}"
        }

    [<Test>]
    let ``Keyed PanelSplit clears exposed area when child shrinks`` () =
        task {
            // Same as above but with a keyed PanelSplit
            let console, terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let splitKey = NodeKey.make "split"

            let vdom (_ : VdomContext) (useLargeSplit : bool) =
                let split =
                    if useLargeSplit then
                        // 50/50 split
                        let left = Vdom.textContent false (String.replicate 200 "X")
                        let right = Vdom.textContent false "right"
                        Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, left, right)
                    else
                        // 25/75 split
                        let left = Vdom.textContent false "AAA"
                        let right = Vdom.textContent false "right"
                        Vdom.panelSplitProportion (SplitDirection.Vertical, 0.25, left, right)

                // Wrap in bordered to make it Unkeyed at the top level
                split |> Vdom.withKey splitKey |> Vdom.bordered

            let processWorld =
                { new WorldProcessor<unit, bool> with
                    member _.ProcessWorld (worldChanges, _, state) =
                        let newState = if worldChanges.Length > 0 then not state else state
                        ProcessWorldResult.make newState
                }

            let renderState = RenderState.make console None

            // First render: 50/50 split
            let mutable state =
                App.pumpOnce worldFreezer true (fun _ -> true) renderState processWorld vdom

            // Send keystroke to trigger rebalance
            world.SendKey (ConsoleKeyInfo ('x', ConsoleKey.NoName, false, false, false))

            // Second render: 25/75 split
            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            let secondRender = ConsoleHarness.toString terminal

            // Bug: the keyed PanelSplit's bounds don't change, so it doesn't clear its background
            // When children are re-laid out, stale content remains in the exposed areas
            if secondRender.Contains 'X' then
                failwith $"Bug detected: Keyed PanelSplit contains stale X's after rebalance. %s{secondRender}"
        }

    [<Test>]
    let ``PanelSplit with Bordered children should not show stale content when rebalancing`` () =
        task {
            // This test demonstrates the bug scenario:
            // PanelSplit containing Bordered panels. When the split rebalances,
            // the PanelSplit's background might not be fully covered by the new child positions.
            let console, terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (useWideLeft : bool) =
                if useWideLeft then
                    // Left side gets 70% - fill it with X's in a bordered panel
                    let left = Vdom.textContent false (String.replicate 500 "X") |> Vdom.bordered
                    let right = Vdom.textContent false "R" |> Vdom.bordered
                    Vdom.panelSplitProportion (SplitDirection.Vertical, 0.7, left, right)
                else
                    // Left side gets 30% - just add a couple of A's in a bordered panel
                    // The area from 30% to 70% should be cleared, but if the bug exists,
                    // it will still contain X's from the previous render
                    let left = Vdom.textContent false "AAA" |> Vdom.bordered
                    let right = Vdom.textContent false "R" |> Vdom.bordered
                    Vdom.panelSplitProportion (SplitDirection.Vertical, 0.3, left, right)

            let processWorld =
                { new WorldProcessor<unit, bool> with
                    member _.ProcessWorld (worldChanges, _, state) =
                        let newState = if worldChanges.Length > 0 then not state else state
                        ProcessWorldResult.make newState
                }

            let renderState = RenderState.make console None

            // First render: 70/30 split with X's on the left
            let mutable state =
                App.pumpOnce worldFreezer true (fun _ -> true) renderState processWorld vdom

            ConsoleHarness.toString terminal |> shouldContainText "XXXX"

            // Send keystroke to trigger rebalance
            world.SendKey (ConsoleKeyInfo ('x', ConsoleKey.NoName, false, false, false))

            // Second render: 30/70 split
            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            let secondRender = ConsoleHarness.toString terminal

            // Check if any line contains X's - they should all be cleared
            if secondRender.Contains 'X' then
                failwith $"Bug detected: Stale X's remain after PanelSplit rebalance. %s{secondRender}"
        }

    [<Test>]
    let ``Unkeyed Bordered clears exposed area when child content shrinks`` () =
        task {
            // Test for Bordered containers with shrinking child content
            let console, terminal = ConsoleHarness.make' (fun () -> 60) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (useLongText : bool) =
                if useLongText then
                    // Long text filling the bordered area
                    Vdom.textContent false (String.replicate 200 "X") |> Vdom.bordered
                else
                    // Short text - exposed area should be cleared
                    Vdom.textContent false "AAA" |> Vdom.bordered

            let processWorld =
                { new WorldProcessor<unit, bool> with
                    member _.ProcessWorld (worldChanges, _, state) =
                        let newState = if worldChanges.Length > 0 then not state else state
                        ProcessWorldResult.make newState
                }

            let renderState = RenderState.make console None

            // First render: long text
            let mutable state =
                App.pumpOnce worldFreezer true (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
┌──────────────────────────────────────────────────────────┐|
│XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX│|
│XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX│|
│XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX│|
└──────────────────────────────────────────────────────────┘|
"

                return ConsoleHarness.toString terminal
            }

            // Send keystroke to change content
            world.SendKey (ConsoleKeyInfo ('x', ConsoleKey.NoName, false, false, false))

            // Second render: short text
            // Border bounds unchanged, child bounds unchanged, but content shrinks
            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
┌──────────────────────────────────────────────────────────┐|
│AAA                                                       │|
│                                                          │|
│                                                          │|
└──────────────────────────────────────────────────────────┘|
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``Keyed Bordered clears exposed area when child content shrinks`` () =
        task {
            // Same as above but with a keyed Bordered
            let console, terminal = ConsoleHarness.make' (fun () -> 60) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let borderedKey = NodeKey.make "bordered"

            let vdom (_ : VdomContext) (useLongText : bool) =
                let bordered =
                    if useLongText then
                        Vdom.textContent false (String.replicate 200 "X") |> Vdom.bordered
                    else
                        Vdom.textContent false "AAA" |> Vdom.bordered

                // Wrap in another bordered to make it Unkeyed at the top level
                bordered |> Vdom.withKey borderedKey |> Vdom.bordered

            let processWorld =
                { new WorldProcessor<unit, bool> with
                    member _.ProcessWorld (worldChanges, _, state) =
                        let newState = if worldChanges.Length > 0 then not state else state
                        ProcessWorldResult.make newState
                }

            let renderState = RenderState.make console None

            // First render: long text
            let mutable state =
                App.pumpOnce worldFreezer true (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
┌──────────────────────────────────────────────────────────┐|
│┌────────────────────────────────────────────────────────┐│|
││XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX││|
│└────────────────────────────────────────────────────────┘│|
└──────────────────────────────────────────────────────────┘|
"

                return ConsoleHarness.toString terminal
            }

            // Send keystroke to change content
            world.SendKey (ConsoleKeyInfo ('x', ConsoleKey.NoName, false, false, false))

            // Second render: short text
            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
┌──────────────────────────────────────────────────────────┐|
│┌────────────────────────────────────────────────────────┐│|
││AAA                                                     ││|
│└────────────────────────────────────────────────────────┘│|
└──────────────────────────────────────────────────────────┘|
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``Absolute split with wide non-fixed child violates MinWidth constraint`` () =
        task {
            // Regression test for: "In measureVerticalSplitAbsolute, only the fixed child is constrained"
            // This test exposes the bug where container MinWidth can exceed parent's MaxWidth constraint.
            //
            // Setup: Terminal width = 50
            //   - Absolute vertical split: give child1 20 pixels (positive n=20), child2 gets remainder
            //   - child1: simple text (MinWidth ~5)
            //   - child2: long text in bordered panel (MinWidth ~40)
            //   - Container reports MinWidth = 20 + 40 = 60 > 50 (VIOLATES INVARIANT!)
            //
            // The bug is subtle: it violates documented invariants but may not cause visible artifacts
            // because the arrange phase works with actual allocated space, not reported MinWidth.
            // However, if this absolute split is nested in an Auto split, the Auto split sees
            // the inflated MinWidth and makes incorrect space allocation decisions.

            let console, terminal = ConsoleHarness.make' (fun () -> 50) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Outer Auto split allocates space to its children based on their reported MinWidth
                let left =
                    // This is the problematic absolute split
                    // It's measured with MaxWidth=50 (full terminal width for Auto split)
                    // But it will report MinWidth = 20 + child2.MinWidth
                    let child1 = Vdom.textContent false "small"
                    // Long text in bordered panel: needs ~35 chars minimum for longest word "unnecessarily"
                    // Plus 2 for border = ~37
                    let child2 =
                        Vdom.textContent false "This text contains unnecessarily long words that wrap"
                        |> Vdom.bordered

                    // Fixed width split: child1 gets 20, child2 gets the rest
                    // When this is measured with MaxWidth=50:
                    // - child1 measured with MaxWidth=min(20,50)=20, reports MinWidth=5
                    // - child2 measured with MaxWidth=50, reports MinWidth=~37
                    // - Container reports MinWidth = 20 + 37 = 57 > 50 ❌ VIOLATES INVARIANT
                    Vdom.panelSplitAbsolute (SplitDirection.Vertical, 20, child1, child2)

                let right = Vdom.textContent false "Right side content"

                // Auto split sees left.MinWidth=57, right.MinWidth=18
                // minSum = 75 > 50 (terminal width)
                // Auto split thinks it can't satisfy minimums and scales proportionally
                // This is the wrong decision - the absolute split could actually fit in less space
                Vdom.panelSplitAuto (SplitDirection.Vertical, left, right)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console None

            App.pumpOnce worldFreezer (FakeUnit.fake ()) (fun _ -> true) renderState processWorld vdom
            |> ignore<FakeUnit>

            // Expected behavior after fix:
            // - Absolute split now constrains child2 to remainder: MaxWidth=50-20=30
            // - child2 (bordered text) reports MinWidth=min(longest_word+2, 30) = 30
            // - Container reports MinWidth = 20 + 30 = 50 (respects constraint ✓)
            // - Auto split sees left.MinWidth=50, right.MinWidth=18, total=68 > 50
            // - Auto split still needs to scale, but makes better decisions
            // - Result: bordered panel is narrower, but "Right side content" is no longer cut off

            expect {
                // This snapshot documents the corrected behavior
                // The layout is now better: "Right side content" displays completely
                snapshot
                    @"
small               ┌──────────────────┐Right side|
                    │This text contains│ content  |
                    │ unnecessarily lon│          |
                    │g words that wrap │          |
                    │                  │          |
                    │                  │          |
                    │                  │          |
                    │                  │          |
                    │                  │          |
                    └──────────────────┘          |
"

                return ConsoleHarness.toString terminal
            }
        }
