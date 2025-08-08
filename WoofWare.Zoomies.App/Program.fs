namespace WoofWare.Zoomies.App

open System.Threading
open WoofWare.Zoomies

type FocusedElement =
    | Toggle1
    | Toggle2

type State =
    {
        mutable IsToggle1Checked : bool
        mutable IsToggle2Checked : bool
        mutable FocusedElement : FocusedElement
    }

    static member Empty () : State =
        {
            IsToggle1Checked = false
            IsToggle2Checked = false
            FocusedElement = FocusedElement.Toggle1
        }

module Program =

    let vdom (state : State) =
        let left =
            Vdom.textContent
                None
                "not praising the praiseworthy keeps people uncompetitive; not prizing rare treasures keeps people from stealing; not looking at the desirable keeps the mind quiet"
            |> Vdom.bordered

        let right =
            Vdom.textContent
                None
                "errybody wants to be a bodybuilder, but don't nobody want to lift no heavy-ass weights"
            |> Vdom.bordered

        let topHalf = Vdom.panelSplitProportion Direction.Vertical 0.5 left right

        let bottomHalf =
            Vdom.labelledCheckbox
                (fun () -> state.FocusedElement <- FocusedElement.Toggle1)
                state.FocusedElement.IsToggle1
                state.IsToggle1Checked
                "Press Space to toggle"

        let vdom = Vdom.panelSplitAbsolute Direction.Horizontal -3 topHalf bottomHalf

        if state.IsToggle1Checked then
            Vdom.panelSplitProportion
                Direction.Vertical
                0.5
                (Vdom.textContent None "This gets displayed when the thing is checked; left")
                (Vdom.labelledCheckbox
                    (fun () -> state.FocusedElement <- FocusedElement.Toggle2)
                    state.FocusedElement.IsToggle2
                    state.IsToggle2Checked
                    "this one is focusable! space to toggle")
            |> Vdom.panelSplitProportion Direction.Horizontal 0.7 vdom
        else
            vdom

    let processWorld (worldChanges : WorldStateChange<unit> seq) (state : State) : unit =
        for change in worldChanges do
            match change with
            | Keystroke c when c.KeyChar = ' ' ->
                match state.FocusedElement with
                | FocusedElement.Toggle1 -> state.IsToggle1Checked <- not state.IsToggle1Checked
                | FocusedElement.Toggle2 -> state.IsToggle2Checked <- not state.IsToggle2Checked
            | Keystroke _ -> ()
            | ApplicationEvent () -> failwith "no app events"

    [<EntryPoint>]
    let main argv =

        App.run (State.Empty ()) (fun _ -> true) processWorld vdom |> _.Wait()

        0
