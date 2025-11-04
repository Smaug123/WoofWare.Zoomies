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
            Vdom.labelledCheckbox (currentFocus = Some toggle1Key) state.IsToggle1Checked "Press Space to toggle"
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
                    Vdom.labelledCheckbox
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

        let renderState = RenderState.make' console

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

            let renderState = RenderState.make' console

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

            let renderState = RenderState.make' console

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

            let renderState = RenderState.make' console

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

        let renderState = RenderState.make' console

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

        let renderState = RenderState.make' console

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

        let renderState = RenderState.make' console

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

        let renderState = RenderState.make' console

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
                        ProcessWorldResult.make state
                }

            let renderState = RenderState.make' console

            // First render: fill with X's
            let state = App.pumpOnce worldFreezer false (fun _ -> true) renderState processWorld vdom

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

            // Second render: show keyed PanelSplit
            // The X's should be cleared (replaced with spaces), not left as artifacts
            let state = App.pumpOnce worldFreezer true (fun _ -> true) renderState processWorld vdom

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
