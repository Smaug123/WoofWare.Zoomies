namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

[<RequireQualifiedAccess>]
type Button =
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
        (ctx : VdomContext, key : NodeKey, label : string, ?isFirstToFocus : bool, ?isInitiallyFocused : bool)
        =
        let isFocused = VdomContext.focusedKey ctx = Some key
        let isPressed = VdomContext.wasRecentlyActivated key ctx

        let button = Vdom.button isFocused isPressed label |> Vdom.withKey key
        Vdom.withFocusTracking (button, ?isFirstToFocus = isFirstToFocus, ?isInitiallyFocused = isInitiallyFocused)
