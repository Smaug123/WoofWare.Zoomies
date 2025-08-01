namespace WoofWare.Zoomies.App

open System.Threading
open WoofWare.Zoomies

type State =
    {
        mutable IsChecked : bool
    }

    static member Empty : State =
        {
            IsChecked = false
        }

module Program =

    let vdom (state : State) =
        let left =
            Vdom.textContent
                "not praising the praiseworthy keeps people uncompetitive; not prizing rare treasures keeps people from stealing; not looking at the desirable keeps the mind quiet"
            |> Vdom.bordered

        let right =
            Vdom.textContent "errybody wants to be a bodybuilder, but don't nobody want to lift no heavy-ass weights"
            |> Vdom.bordered

        let topHalf = Vdom.panelSplitProportion Direction.Vertical 0.5 left right

        let bottomHalf = Vdom.labelledCheckbox state.IsChecked "Press Space to toggle"

        let vdom = Vdom.panelSplitAbsolute Direction.Horizontal -3 topHalf bottomHalf

        if state.IsChecked then
            Vdom.textContent "This gets displayed when the thing is checked"
            |> Vdom.panelSplitProportion Direction.Horizontal 0.7 vdom
        else
            vdom

    let processWorld (worldChanges : WorldStateChange seq) (state : State) : unit =
        for change in worldChanges do
            match change with
            | Keystroke c when c.KeyChar = ' ' -> state.IsChecked <- not state.IsChecked
            | Keystroke _ -> ()


    [<EntryPoint>]
    let main argv =

        Terminal.clearScreen ()

        let listener = WorldFreezer.listen CancellationToken.None

        let state = State.Empty
        // TODO: react to changes in dimension
        let renderState = RenderState.make ()

        while true do
            listener.Refresh ()

            processWorld listener.Changes state

            Render.oneStep renderState state vdom

        0
