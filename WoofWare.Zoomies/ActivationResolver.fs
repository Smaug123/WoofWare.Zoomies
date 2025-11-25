namespace WoofWare.Zoomies

open System

/// Transforms keystrokes on focused elements into application events.
/// Return Some to intercept the keystroke and emit the event.
/// Return None to pass the keystroke through to ProcessWorld unchanged.
///
/// The state passed into the `ActivationResolver` is the source of truth.
/// You should not close over external state that may change during app
/// execution, for example.
type ActivationResolver<'appEvent, 'state> =
    delegate of focusedKey : NodeKey * keystroke : ConsoleKeyInfo * state : 'state -> 'appEvent option

[<RequireQualifiedAccess>]
module ActivationResolver =
    let private isActivationKey (k : ConsoleKeyInfo) =
        (k.Key = ConsoleKey.Spacebar || k.Key = ConsoleKey.Enter)
        && k.Modifiers = ConsoleModifiers.None

    /// A resolver that never handles any keystrokes (always returns None).
    /// Useful for applications that don't use buttons or other activation-based components.
    let none<'e, 's> : ActivationResolver<'e, 's> =
        ActivationResolver (fun _ _ _ -> None)

    /// Combine multiple resolvers. First one to return Some wins.
    let combine (resolvers : ActivationResolver<'e, 's> list) : ActivationResolver<'e, 's> =
        ActivationResolver (fun key keystroke state ->
            // TODO: this is pretty inefficient; consider this more carefully,
            // perhaps with a map of "keystroke" to "component that accepts the keystroke".
            // This isn't a blocking architectural problem.
            resolvers |> List.tryPick (fun r -> r.Invoke (key, keystroke, state))
        )

    /// Create a resolver for a button (activates on Space/Enter)
    let button (key : NodeKey) (event : 'e) : ActivationResolver<'e, 's> =
        ActivationResolver (fun k keystroke _ ->
            if k = key && isActivationKey keystroke then
                Some event
            else
                None
        )

    /// Create a resolver for a text input field
    let textInput (key : NodeKey) (onChar : char -> 'e) (onBackspace : 'e) : ActivationResolver<'e, 's> =
        ActivationResolver (fun k keystroke _ ->
            if k <> key then
                None
            elif keystroke.Key = ConsoleKey.Backspace then
                Some onBackspace
            elif keystroke.KeyChar <> '\000' then
                Some (onChar keystroke.KeyChar)
            else
                None
        )
