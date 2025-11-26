namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

[<RequireQualifiedAccess>]
type LabelledCheckbox =

    /// Creates a checkbox with a text label positioned to its right.
    static member make
        (
            ctx : VdomContext,
            label : string,
            key : NodeKey,
            isChecked : bool,
            ?isFirstToFocus : bool,
            ?isInitiallyFocused : bool
        )
        =
        // TODO: centre this text horizontally so it's next to the checkbox
        Vdom.panelSplitAbsolute (
            SplitDirection.Vertical,
            3,
            Checkbox.make (
                ctx,
                key,
                isChecked,
                ?isFirstToFocus = isFirstToFocus,
                ?isInitiallyFocused = isInitiallyFocused
            ),
            Vdom.textContent false label
        )
