namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

[<RequireQualifiedAccess>]
type LabelledCheckbox =

    /// Creates a checkbox with a text label positioned to its right.
    static member make
        (
            ctx : IVdomContext,
            label : string,
            key : NodeKey,
            isChecked : bool,
            ?isFirstToFocus : bool,
            ?isInitiallyFocused : bool
        )
        =
        ctx.Builder {
            let! checkbox =
                Checkbox.make (
                    ctx,
                    key,
                    isChecked,
                    ?isFirstToFocus = isFirstToFocus,
                    ?isInitiallyFocused = isInitiallyFocused
                )

            // TODO: centre this text horizontally so it's next to the checkbox
            return
                Vdom.panelSplitAbsolute (SplitDirection.Vertical, 3, checkbox, Vdom.textContent label)
                |> Vdom.withTag "labelled-checkbox"
        }
