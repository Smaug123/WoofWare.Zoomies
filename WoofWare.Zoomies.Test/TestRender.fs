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

    let vdom (vdomContext : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
        let left =
            Vdom.textContent
                "not praising the praiseworthy keeps people uncompetitive; not prizing rare treasures keeps people from stealing; not looking at the desirable keeps the mind quiet"
            |> Vdom.bordered

        let right =
            Vdom.textContent "errybody wants to be a bodybuilder, but don't nobody want to lift no heavy-ass weights"
            |> Vdom.bordered

        let topHalf = Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, left, right)

        let toggle1Key = NodeKey.make "toggle1"

        let bottomHalf =
            Components.LabelledCheckbox.make (vdomContext, "Press Space to toggle", toggle1Key, state.IsToggle1Checked)

        let vdom =
            Vdom.panelSplitAbsolute (SplitDirection.Horizontal, -3, topHalf, bottomHalf)

        if state.IsToggle1Checked then
            let toggle2Key = NodeKey.make "toggle2"

            let inner =
                Vdom.panelSplitProportion (
                    SplitDirection.Vertical,
                    0.5,
                    Vdom.textContent "only displayed when checked",
                    Components.LabelledCheckbox.make (
                        vdomContext,
                        "this one is focusable!",
                        toggle2Key,
                        state.IsToggle2Checked
                    )
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

        let renderState =
            MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

        Render.oneStep renderState state (vdom (VdomContext.asTyped<unit> (RenderState.vdomContext renderState)))

        terminalOps.Clear ()

        Render.oneStep renderState state (vdom (VdomContext.asTyped<unit> (RenderState.vdomContext renderState)))

        terminalOps |> shouldBeEmpty

    [<Test>]
    let ``example 1`` () =
        let processWorld =
            { new WorldProcessor<unit, unit, State> with
                member _.ProcessWorld (worldChanges, renderState, state) =
                    let focusedKey = renderState.FocusedKey
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
                        | MouseEvent _ -> failwith "no mouse events"
                        | Paste _ -> failwith "no paste events"
                        | ApplicationEvent () -> failwith "no app events"
                        | ApplicationEventException _ -> failwith "no exceptions possible"

                    ProcessWorldResult.make newState

                member _.ProcessPostLayoutEvents (_events, _ctx, state) = state
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

            let renderState =
                MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

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

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

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

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

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

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

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

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

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

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

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

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

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

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

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

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

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
    let ``SetCursorInvisible is only called once per render`` () =
        let terminalOps = ResizeArray<TerminalOp> ()

        let console =
            { IConsole.defaultForTests with
                Execute = fun x -> terminalOps.Add x
                WindowWidth = fun _ -> 20
                WindowHeight = fun _ -> 5
            }

        let renderState =
            MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

        // Create vdom with some content that will result in multiple cells being written
        let vdom =
            Vdom.textContent "This is some text that spans multiple cells" |> Vdom.bordered

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

        // Count how many SetCursorVisibility true operations were emitted
        let setCursorVisibleCount =
            terminalOps
            |> Seq.filter (
                function
                | TerminalOp.SetCursorVisibility true -> true
                | _ -> false
            )
            |> Seq.length

        // Should also be called once (to restore cursor visibility after render)
        setCursorVisibleCount |> shouldEqual 1

    [<Test>]
    let ``layoutOf works for unchanged keyed nodes across multiple renders`` () =
        let terminalOps = ResizeArray ()

        let console =
            { IConsole.defaultForTests with
                Execute = fun x -> terminalOps.Add x
            }

        let renderState =
            MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

        let key = NodeKey.make "test-key"

        // Create a vdom with a keyed node - memoize it so we get the same reference each time
        // Use a wrapper (Bordered) to ensure we have an Unkeyed vdom as required by oneStep
        let cachedKeyedContent = Vdom.textContent "test content" |> Vdom.withKey key

        let vdom (_ : FakeUnit) : Vdom<DesiredBounds> =
            // Wrap the keyed content in a bordered panel (which is Unkeyed)
            Vdom.bordered cachedKeyedContent

        // First render
        Render.oneStep renderState (FakeUnit.fake ()) vdom

        // layoutOf should work after first render
        let layout1 = RenderState.layoutOf key renderState
        layout1.IsSome |> shouldEqual true

        // Count the writes from the first render
        let firstRenderWriteCount =
            terminalOps
            |> Seq.filter (
                function
                | TerminalOp.WriteRun _ -> true
                | _ -> false
            )
            |> Seq.length

        terminalOps.Clear ()

        // Second render with the same vdom (should trigger early cutoff on the keyed node)
        Render.oneStep renderState (FakeUnit.fake ()) vdom

        // layoutOf should still work after second render, even though early cutoff was taken
        let layout2 = RenderState.layoutOf key renderState
        layout2.IsSome |> shouldEqual true

        // Verify early-cutoff was taken: second render should produce fewer writes
        // (specifically, no writes since the keyed content is unchanged)
        let secondRenderWriteCount =
            terminalOps
            |> Seq.filter (
                function
                | TerminalOp.WriteRun _ -> true
                | _ -> false
            )
            |> Seq.length

        // First render must have had writes
        firstRenderWriteCount |> shouldBeGreaterThan 0

        // Second render should have no writes due to early-cutoff
        secondRenderWriteCount |> shouldEqual 0

    [<Test>]
    let ``Vdom.empty allows right justification`` () =
        task {
            let console, terminal = ConsoleHarness.make ()

            let vdom (_ : IVdomContext<_>) (_ : unit) : Vdom<DesiredBounds> =
                // Use Vdom.empty with panelSplitAbsolute to right-justify content
                // Negative absolute value gives the right side a fixed width, left side gets the rest
                // Empty fills the left side, pushing "Right" to the right edge
                Vdom.panelSplitAbsolute (
                    SplitDirection.Vertical,
                    -5,
                    (Vdom.empty |> Vdom.withKey (NodeKey.make "spacer")),
                    (Vdom.textContent "Right" |> Vdom.withKey (NodeKey.make "content"))
                )

            let renderState =
                MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

            Render.oneStep renderState () (vdom (VdomContext.asTyped<unit> (RenderState.vdomContext renderState)))

            expect {
                snapshot
                    @"
                                                                           Right|
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
