namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

[<RequireQualifiedAccess>]
type Toggle =
    /// <summary>A toggle, without any of the WoofWare.Zoomies framework handling.</summary>
    /// <remarks><c>Toggle.make</c> wraps this if you want automatic framework focus handling.</remarks>
    /// <param name="untoggledGlyph">The single character to display if the toggle is in the "untoggled" state, as determined by <c>isToggled</c>.</param>
    /// <param name="toggledGlyph">The single character to display if the toggle is in the "toggled" state, as determined by <c>isToggled</c>.</param>
    /// <param name="isToggled">True if the toggle should render as if it is in the "toggled" state - e.g. a checkbox that has been checked, or an expansion toggle that has been expanded.</param>
    /// <param name="isFocused">True if the toggle should render as if it currently has focus.</param>
    static member make'
        (untoggledGlyph : char, toggledGlyph : char, isToggled : bool, isFocused : bool)
        : Vdom<DesiredBounds>
        =
        let glyph = if isToggled then toggledGlyph else untoggledGlyph

        let content =
            if isFocused then
                $"[%c{glyph}]"
            else
                " " + glyph.ToString () + " "

        Vdom.styledText (content, CellStyle.none, ContentAlignment.Centered)
        |> Vdom.withTag "toggle"

    /// <summary>Creates a toggle component with custom glyphs.</summary>
    /// <param name="ctx">The VdomContext for checking focus state.</param>
    /// <param name="key">The NodeKey identifying this toggle.</param>
    /// <param name="untoggledGlyph">The character to display when the toggle is in the untoggled state (e.g. an unchecked checkbox), as determined by <c>isToggled</c>.</param>
    /// <param name="toggledGlyph">The character to display when the toggle is in the toggled state (e.g. a checked checkbox), as determined by <c>isToggled</c>.</param>
    /// <param name="isToggled">
    /// Specifies that this toggle is currently toggled on. Derive the value of this parameter from your application
    /// state.
    /// </param>
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
            ctx : IVdomContext,
            key : NodeKey,
            untoggledGlyph : char,
            toggledGlyph : char,
            isToggled : bool,
            ?isFirstToFocus : bool,
            ?isInitiallyFocused : bool
        )
        : Vdom<DesiredBounds>
        =
        let isFocused = ctx.FocusedKey = Some key

        let toggle =
            Toggle.make' (untoggledGlyph, toggledGlyph, isToggled, isFocused)
            |> Vdom.withKey key

        Vdom.withFocusTracking (toggle, ?isFirstToFocus = isFirstToFocus, ?isInitiallyFocused = isInitiallyFocused)
