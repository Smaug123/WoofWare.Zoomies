namespace WoofWare.Zoomies.Demos.DemoDefinitions

open System
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

/// A simple demo showing checkbox interaction with keyboard navigation.
module CheckboxToggle =

    type State =
        {
            IsChecked1 : bool
            IsChecked2 : bool
        }

        static member Initial =
            {
                IsChecked1 = false
                IsChecked2 = true
            }

    type Event =
        | Toggle1
        | Toggle2

    let private checkbox1Key = NodeKey.make "checkbox1"
    let private checkbox2Key = NodeKey.make "checkbox2"

    let private processWorld (_worldBridge : IWorldBridge<Event>) =
        { new WorldProcessor<Event, unit, State> with
            member _.ProcessWorld (changes, _ctx, state) =
                let mutable isChecked1 = state.IsChecked1
                let mutable isChecked2 = state.IsChecked2

                for change in changes do
                    match change with
                    | WorldStateChange.ApplicationEvent Toggle1 -> isChecked1 <- not isChecked1
                    | WorldStateChange.ApplicationEvent Toggle2 -> isChecked2 <- not isChecked2
                    | WorldStateChange.MouseEvent _
                    | WorldStateChange.Keystroke _
                    | WorldStateChange.Paste _
                    | WorldStateChange.ApplicationEventException _ -> ()

                ProcessWorldResult.make
                    {
                        IsChecked1 = isChecked1
                        IsChecked2 = isChecked2
                    }

            member _.ProcessPostLayoutEvents (_, _, state) = state
        }

    let private vdom (ctx : IVdomContext<unit>) (state : State) : Vdom<DesiredBounds> =
        let title = Vdom.textContent "Checkbox Demo"

        let checkbox1 =
            LabelledCheckbox.make (ctx, "Option A", checkbox1Key, state.IsChecked1, isFirstToFocus = true)

        let checkbox2 =
            LabelledCheckbox.make (ctx, "Option B", checkbox2Key, state.IsChecked2)

        Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 1, title, checkbox1)
        |> fun content -> Vdom.panelSplitAbsolute (SplitDirection.Horizontal, -1, content, checkbox2)
        |> Vdom.bordered

    let private resolver : ActivationResolver<Event, State> =
        ActivationResolver.combine
            [
                ActivationResolver.button checkbox1Key Toggle1
                ActivationResolver.button checkbox2Key Toggle2
            ]

    let run (getEnv : string -> string option) : AppHandle =
        App.run getEnv State.Initial (fun _ -> true) processWorld vdom resolver
