namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

[<RequireQualifiedAccess>]
type Button =
    /// <summary>A button, without any of the WoofWare.Zoomies framework handling.</summary>
    /// <remarks><c>Button.make</c> wraps this if you want automatic framework and button-press handling.</remarks>
    /// <param name="label">The text within the button.</param>
    /// <param name="isPressed">True if the button should render as if it were very recently pressed.</param>
    /// <param name="isFocused">True if the button should render as if it currently has focus.</param>
    static member make' (label : string, isFocused : bool, isPressed : bool) : Vdom<DesiredBounds> =
        // Calculate button content: brackets vary based on both focus and pressed state
        let leftBracket, rightBracket =
            match isFocused, isPressed with
            | true, true -> "[*", "*]"
            | true, false -> "[[", "]]"
            | false, true -> " *", "* "
            | false, false -> "[ ", " ]"

        let content = $"%s{leftBracket} %s{label} %s{rightBracket}"
        let style = if isPressed then CellStyle.inverted else CellStyle.none

        Vdom.styledText (content, style, ContentAlignment.Centered)
        |> Vdom.withTag "button"

    /// <summary>Creates a button with automatic focus and activation visual state.</summary>
    /// <param name="ctx">The VdomContext for checking focus and activation state.</param>
    /// <param name="key">The NodeKey identifying this button. You must also register an ActivationResolver for this key.</param>
    /// <param name="label">The text to display on the button.</param>
    /// <param name="isFirstToFocus">
    /// Set to `true` to put this element first in the focus order, when using automatic focus tracking.
    /// That is, pressing "tab" will highlight this element first.
    /// </param>
    /// <param name="isInitiallyFocused">
    /// Set to `true` to have this element be focused from the very first tick. (<c>isFirstToFocus</c> is ignored if this
    /// is set to `true`.)
    /// </param>
    /// <remarks>
    /// This component automatically handles focus and pressed visual states by consulting the VdomContext.
    /// You must also provide an ActivationResolver to `App.run` to handle button activation events; the Vdom is only
    /// concerned with layout, not action.
    /// </remarks>
    static member make
        (ctx : IVdomContext, key : NodeKey, label : string, ?isFirstToFocus : bool, ?isInitiallyFocused : bool)
        =
        let isFocused = ctx.FocusedKey = Some key
        let isPressed = ctx.WasRecentlyActivated key

        let button = Button.make' (label, isFocused, isPressed) |> Vdom.withKey key

        Vdom.withFocusTracking (button, ?isFirstToFocus = isFirstToFocus, ?isInitiallyFocused = isInitiallyFocused)
