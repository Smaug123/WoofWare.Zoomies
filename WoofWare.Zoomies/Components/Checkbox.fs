namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

[<RequireQualifiedAccess>]
type Checkbox =
    static member private toggledGlyph = '☑'
    static member private untoggledGlyph = '☐'

    static member make' (isChecked : bool, isFocused : bool) : Vdom<DesiredBounds> =
        Toggle.make' (Checkbox.untoggledGlyph, Checkbox.toggledGlyph, isChecked, isFocused)
        |> Vdom.withTag "checkbox"

    /// <summary>Creates a checkbox component with automatic focus state.</summary>
    /// <param name="ctx">The VdomContext for checking focus state.</param>
    /// <param name="key">The NodeKey identifying this checkbox.</param>
    /// <param name="isChecked">Specifies that this checkbox is currently checked. Derive the value of this parameter
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
    /// This component uses the standard checkbox glyphs: ☐ (unchecked) and ☑ (checked).
    /// It automatically handles focus visual state by consulting the VdomContext.
    /// </remarks>
    static member make
        (ctx : IVdomContext, key : NodeKey, isChecked : bool, ?isFirstToFocus : bool, ?isInitiallyFocused : bool)
        : Vdom<DesiredBounds>
        =
        Toggle.make (
            ctx,
            key,
            Checkbox.untoggledGlyph,
            Checkbox.toggledGlyph,
            isChecked,
            ?isFirstToFocus = isFirstToFocus,
            ?isInitiallyFocused = isInitiallyFocused
        )
        // `make` doesn't call through to `make'`, so need to tag separately
        |> Vdom.withTag "checkbox"
