namespace WoofWare.Zoomies.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies

type State =
    {
        mutable IsToggle1Checked : bool
        mutable IsToggle2Checked : bool
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

    let vdom (renderState : RenderState) (state : State) : Vdom<DesiredBounds, _> =
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

        let topHalf = Vdom.panelSplitProportion (Direction.Vertical, 0.5, left, right)

        let toggle1Key = NodeKey.make "toggle1"
        let currentFocus = RenderState.focusedKey renderState

        let bottomHalf =
            Vdom.labelledCheckbox (currentFocus = Some toggle1Key) state.IsToggle1Checked "Press Space to toggle"
            |> Vdom.withKey toggle1Key
            |> Vdom.focusable

        let vdom = Vdom.panelSplitAbsolute (Direction.Horizontal, -3, topHalf, bottomHalf)

        if state.IsToggle1Checked then
            let toggle2Key = NodeKey.make "toggle2"

            let inner =
                Vdom.panelSplitProportion (
                    Direction.Vertical,
                    0.5,
                    Vdom.textContent false "only displayed when checked",
                    (Vdom.labelledCheckbox
                        (currentFocus = Some toggle2Key)
                        state.IsToggle2Checked
                        "this one is focusable!"
                     |> Vdom.withKey toggle2Key
                     |> Vdom.focusable)
                )

            Vdom.panelSplitProportion (Direction.Horizontal, 0.7, vdom, inner)
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

        Render.oneStep renderState state (vdom renderState)

        terminalOps.Clear ()

        Render.oneStep renderState state (vdom renderState)

        terminalOps |> shouldBeEmpty

    [<Test>]
    let ``example 1`` () =
        let mutableRenderState = ref None

        let processWorld =
            { new WorldProcessor<unit, State> with
                member _.ProcessWorld (worldChanges, _, state) =
                    let focusedKey =
                        match mutableRenderState.Value with
                        | Some rs -> RenderState.focusedKey rs
                        | None -> None

                    for change in worldChanges do
                        match change with
                        | Keystroke c when c.KeyChar = ' ' ->
                            match focusedKey with
                            | Some key when key = NodeKey.make "toggle1" ->
                                state.IsToggle1Checked <- not state.IsToggle1Checked
                            | Some key when key = NodeKey.make "toggle2" ->
                                state.IsToggle2Checked <- not state.IsToggle2Checked
                            | _ -> ()
                        | Keystroke _ -> ()
                        | KeyboardEvent _ -> failwith "no keyboard events"
                        | MouseEvent _ -> failwith "no mouse events"
                        | ApplicationEvent () -> failwith "no app events"
                        | ApplicationEventException _ -> failwith "no exceptions possible"
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

            let state = State.Empty ()

            let renderState = RenderState.make' console
            mutableRenderState.Value <- Some renderState

            // TODO: semantics have changed and need to be fixed.
            // Before the change, the initial state had toggle 1 focused.
            // After the change, we don't have a way to specify that!
            App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

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

            App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

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

            App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

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
            App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

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
            App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

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
            App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

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
            App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

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
            App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

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
            App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

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
