namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

[<RequireQualifiedAccess>]
type RadioButton =
    static member private selectedGlyph = '◉'
    static member private unselectedGlyph = '○'

    static member make' (isSelected : bool, isFocused : bool) : Vdom<DesiredBounds> =
        Toggle.make' (RadioButton.unselectedGlyph, RadioButton.selectedGlyph, isSelected, isFocused)
        |> Vdom.withTag "radio-button"

    /// <summary>Creates a radio button component with automatic focus state.</summary>
    /// <param name="ctx">The VdomContext for checking focus state.</param>
    /// <param name="key">The NodeKey identifying this radio button.</param>
    /// <param name="isSelected">Specifies that this radio button is currently selected. Derive the value of this parameter
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
    /// This component uses the standard radio button glyphs: ○ (unselected) and ◉ (selected).
    /// It automatically handles focus visual state by consulting the VdomContext.
    /// </remarks>
    static member make
        (ctx : IVdomContext, key : NodeKey, isSelected : bool, ?isFirstToFocus : bool, ?isInitiallyFocused : bool)
        : Vdom<DesiredBounds>
        =
        Toggle.make (
            ctx,
            key,
            RadioButton.unselectedGlyph,
            RadioButton.selectedGlyph,
            isSelected,
            ?isFirstToFocus = isFirstToFocus,
            ?isInitiallyFocused = isInitiallyFocused
        )
        // `make` doesn't call through to `make'`, so need to tag separately
        |> Vdom.withTag "radio-button"
