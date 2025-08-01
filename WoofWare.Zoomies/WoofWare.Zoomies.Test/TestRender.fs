namespace WoofWare.Zoomies.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies

type State =
    {
        mutable IsChecked : bool
    }

    static member Empty : State =
        {
            IsChecked = false
        }

[<TestFixture>]
module TestRender =
    let vdom (state : State) =
        let left =
            Vdom.textContent
                "not praising the praiseworthy keeps people uncompetitive; not prizing rare treasures keeps people from stealing; not looking at the desirable keeps the mind quiet"
            |> Vdom.bordered

        let right =
            Vdom.textContent "errybody wants to be a bodybuilder, but don't nobody want to lift no heavy-ass weights"
            |> Vdom.bordered

        let topHalf = Vdom.panelSplit Direction.Vertical 0.5 left right

        let bottomHalf = Vdom.labelledCheckbox state.IsChecked "Press Space to toggle"

        let vdom = Vdom.panelSplit Direction.Horizontal 0.9 topHalf bottomHalf

        if state.IsChecked then
            Vdom.textContent "This gets displayed when the thing is checked"
            |> Vdom.panelSplit Direction.Horizontal 0.7 vdom
        else
            vdom

    [<Test>]
    let ``there is no rerender if nothing changes`` () =
        let state = State.Empty

        let terminalOps = ResizeArray ()
        let renderState = RenderState.make' (fun () -> 80) (fun () -> 80) terminalOps.Add

        Render.oneStep renderState state vdom

        terminalOps.Clear ()

        Render.oneStep renderState state vdom

        terminalOps |> shouldBeEmpty

    [<Test>]
    let ``example 1`` () =
        let state = State.Empty

        let terminal = ConsoleHarness.empty 10 80

        let renderState =
            RenderState.make' (fun () -> 80) (fun () -> 10) (fun c -> ConsoleHarness.execute c terminal)

        Render.oneStep renderState state vdom

        expect {
            snapshot
                @"
┌──────────────────────────────────────┐┌──────────────────────────────────────┐
│not praising the praiseworthy keeps pe││errybody wants to be a bodybuilder, bu│
│ople uncompetitive; not prizing rare t││t don't nobody want to lift no heavy-a│
│reasures keeps people from stealing; n││ss weights                            │
│ot looking at the desirable keeps the ││                                      │
│mind quiet                            ││                                      │
│                                      ││                                      │
│                                      ││                                      │
└──────────────────────────────────────────────────────────────────────────────┘
Press Space to toggle                                                           
"

            return ConsoleHarness.toString terminal
        }
