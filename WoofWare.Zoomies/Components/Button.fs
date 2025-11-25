namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

[<RequireQualifiedAccess>]
module Button =

    /// <summary>Creates a button with automatic focus and activation visual state.</summary>
    /// <param name="ctx">The VdomContext for checking focus and activation state.</param>
    /// <param name="key">The NodeKey identifying this button. You must also register an ActivationResolver for this key.</param>
    /// <param name="label">The text to display on the button.</param>
    /// <remarks>
    /// This component automatically handles focus and pressed visual states by consulting the VdomContext.
    /// You must also provide an ActivationResolver to handle button activation events.
    /// </remarks>
    let make (ctx : VdomContext) (key : NodeKey) (label : string) =
        let isFocused = VdomContext.focusedKey ctx = Some key
        let isPressed = VdomContext.wasRecentlyActivated key ctx

        Vdom.button isFocused isPressed label
        |> Vdom.withKey key
        |> Vdom.withFocusTracking
