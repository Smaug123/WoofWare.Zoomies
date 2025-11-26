namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

[<RequireQualifiedAccess>]
type Toggle =
    /// <summary>Creates a toggle component with custom glyphs.</summary>
    /// <param name="ctx">The VdomContext for checking focus state.</param>
    /// <param name="key">The NodeKey identifying this toggle.</param>
    /// <param name="uncheckedGlyph">The character to display when the toggle is in the unchecked/collapsed state.</param>
    /// <param name="checkedGlyph">The character to display when the toggle is in the checked/expanded state.</param>
    /// <param name="isChecked">Specifies that this toggle is currently checked. Derive the value of this parameter
    /// from your application state.</param>
    /// <param name="isFirstToFocus">
    /// Set to `true` to put this element first in the focus order, when using automatic focus tracking.
    /// That is, pressing "tab" will highlight this element first.
    /// </param>
    /// <param name="isInitiallyFocused">
    /// Set to `true` to have this element be focused from the very first tick. (<c>isFirstToFocus</c> is ignored if this
    /// is set to `true`.)
    /// </param>
    /// <remarks>
    /// This component automatically handles focus visual state by consulting the VdomContext.
    /// </remarks>
    static member make
        (
            ctx : VdomContext,
            key : NodeKey,
            uncheckedGlyph : char,
            checkedGlyph : char,
            isChecked : bool,
            ?isFirstToFocus : bool,
            ?isInitiallyFocused : bool
        )
        : Vdom<DesiredBounds, Unkeyed>
        =
        let isFocused = VdomContext.focusedKey ctx = Some key
        let glyph = if isChecked then checkedGlyph else uncheckedGlyph
        let content = if isFocused then $"[%c{glyph}]" else glyph.ToString ()

        let toggle = Vdom.styledText content CellStyle.none |> Vdom.withKey key
        Vdom.withFocusTracking (toggle, ?isFirstToFocus = isFirstToFocus, ?isInitiallyFocused = isInitiallyFocused)
