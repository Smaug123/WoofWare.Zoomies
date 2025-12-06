namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

[<RequireQualifiedAccess>]
type TextBox =
    /// <summary>Low-level TextBox rendering without framework integration.</summary>
    /// <remarks>Shows the text content with a cursor indicator at the specified position.</remarks>
    /// <param name="content">The text content to display.</param>
    /// <param name="cursorPos">The cursor position (character index, 0 to content.Length inclusive), or -1 to show no cursor.</param>
    /// <param name="isFocused">Whether the textbox should render as focused (with cursor visible and inverted style).</param>
    /// <param name="wrap">
    /// If true (the default), text wraps to the next line when it reaches the edge of the bounds.
    /// If false, text is truncated at the edge of the bounds and does not wrap.
    /// </param>
    static member make' (content : string, cursorPos : int, isFocused : bool, ?wrap : bool) : Vdom<DesiredBounds> =
        // Reserved-width cursor: always allocate space for cursor marker
        // This maintains constant width across focus states (zero layout jitter)
        let displayText, style =
            if cursorPos >= 0 && cursorPos <= content.Length then
                if isFocused then
                    let before = content.Substring (0, cursorPos)
                    let after = content.Substring cursorPos
                    // Show cursor as | with inverted style for clear focus indication.
                    // The entire textbox content is inverted while focused (mirrors other controls'
                    // focus styling). If you need a caret-only inversion, render the caret as its
                    // own styled segment instead.
                    ($"%s{before}|%s{after}", CellStyle.inverted)
                else
                    // Reserve cursor space at the end (maintains width without visual oddity)
                    ($"%s{content} ", CellStyle.none)
            else
                // Invalid cursor position: just show content
                (content, CellStyle.none)

        Vdom.styledText (displayText, style, ?wrap = wrap) |> Vdom.withTag "textbox"

    /// <summary>Framework-integrated TextBox with automatic focus handling.</summary>
    /// <param name="ctx">The VdomContext for checking focus state.</param>
    /// <param name="key">The NodeKey identifying this textbox. You must also register an ActivationResolver for this key.</param>
    /// <param name="content">The current text content.</param>
    /// <param name="cursorPos">The current cursor position.</param>
    /// <param name="isFirstToFocus">
    /// Set to `true` to put this element first in the focus order, when using automatic focus tracking.
    /// </param>
    /// <param name="isInitiallyFocused">
    /// Set to `true` to have this element be focused from the very first tick.
    /// </param>
    /// <param name="wrap">
    /// If true (the default), text wraps to the next line when it reaches the edge of the bounds.
    /// If false, text is truncated at the edge of the bounds and does not wrap.
    /// </param>
    /// <remarks>
    /// This component automatically handles focus visual state by consulting the VdomContext.
    /// You must also provide an ActivationResolver to `App.run` to handle text editing events.
    /// </remarks>
    static member make
        (
            ctx : VdomContext,
            key : NodeKey,
            content : string,
            cursorPos : int,
            ?isFirstToFocus : bool,
            ?isInitiallyFocused : bool,
            ?wrap : bool
        )
        : Vdom<DesiredBounds>
        =
        let isFocused = VdomContext.focusedKey ctx = Some key

        let textbox =
            TextBox.make' (content, cursorPos, isFocused, ?wrap = wrap) |> Vdom.withKey key

        Vdom.withFocusTracking (textbox, ?isFirstToFocus = isFirstToFocus, ?isInitiallyFocused = isInitiallyFocused)
