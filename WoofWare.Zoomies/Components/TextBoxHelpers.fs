namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

[<RequireQualifiedAccess>]
module TextBoxHelpers =
    /// Apply a TextBoxAction to the current content and cursor position.
    /// Returns the new (content, cursorPos) tuple.
    /// Clamps invalid cursor positions to valid bounds before applying the action.
    let applyAction (content : string) (cursor : int) (action : TextBoxAction) : string * int =
        // Clamp cursor to valid range first to prevent Substring exceptions
        let cursor = max 0 (min cursor content.Length)

        match action with
        | InsertChar c ->
            let before = content.Substring (0, cursor)
            let after = content.Substring cursor
            (before + (string<char> c) + after, cursor + 1)

        | Backspace ->
            if cursor > 0 then
                let before = content.Substring (0, cursor - 1)
                let after = content.Substring cursor
                (before + after, cursor - 1)
            else
                (content, cursor)

        | Delete ->
            if cursor < content.Length then
                let before = content.Substring (0, cursor)
                let after = content.Substring (cursor + 1)
                (before + after, cursor)
            else
                (content, cursor)

        | MoveLeft -> (content, max 0 (cursor - 1))
        | MoveRight -> (content, min content.Length (cursor + 1))
        | Home -> (content, 0)
        | End -> (content, content.Length)
