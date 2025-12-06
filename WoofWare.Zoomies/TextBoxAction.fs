namespace WoofWare.Zoomies

/// Actions that can be performed on a TextBox.
/// This type is defined standalone to maintain composability:
/// component-specific types stay separate from the generic ActivationResolver module.
type TextBoxAction =
    | InsertChar of char
    /// Insert a string at the cursor position (e.g., from a paste operation).
    | InsertString of string
    | Backspace
    | Delete
    | MoveLeft
    | MoveRight
    | Home
    | End
    /// Delete from cursor to end of line (Ctrl+K)
    | DeleteToEnd
    /// Delete from cursor to beginning of line (Ctrl+U)
    | DeleteToBeginning
    /// Delete word backward (Ctrl+W)
    | DeleteWordBackward
