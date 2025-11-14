namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

[<RequireQualifiedAccess>]
module LabelledCheckbox =

    /// Creates a checkbox with a text label positioned to its right.
    let make (isFocused : bool) (isChecked : bool) (label : string) : Vdom<DesiredBounds, Unkeyed> =
        // TODO: centre this text horizontally so it's next to the checkbox
        Vdom.panelSplitAbsolute (
            SplitDirection.Vertical,
            3,
            Vdom.checkbox isFocused isChecked,
            Vdom.textContent false label
        )
