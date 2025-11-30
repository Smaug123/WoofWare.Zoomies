namespace WoofWare.Zoomies

/// Actions that can be performed on a TextBox.
/// This type is defined standalone to maintain composability:
/// component-specific types stay separate from the generic ActivationResolver module.
type TextBoxAction =
    | InsertChar of char
    | Backspace
    | Delete
    | MoveLeft
    | MoveRight
    | Home
    | End
