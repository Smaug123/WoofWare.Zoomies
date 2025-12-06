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

        | InsertString s ->
            let before = content.Substring (0, cursor)
            let after = content.Substring cursor
            (before + s + after, cursor + s.Length)

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

        | DeleteToEnd ->
            // Delete from cursor to end of line
            let before = content.Substring (0, cursor)
            (before, cursor)

        | DeleteToBeginning ->
            // Delete from cursor to beginning of line
            let after = content.Substring cursor
            (after, 0)

        | DeleteWordBackward ->
            if cursor = 0 then
                (content, cursor)
            else
                // Find the start of the word to delete
                // First, skip any whitespace immediately before the cursor
                let mutable pos = cursor - 1

                while pos > 0 && System.Char.IsWhiteSpace content.[pos] do
                    pos <- pos - 1

                // Then, skip the word characters
                while pos > 0 && not (System.Char.IsWhiteSpace content.[pos - 1]) do
                    pos <- pos - 1

                // Handle case where we're at position 0 but didn't find whitespace
                if pos > 0 || System.Char.IsWhiteSpace content.[0] then
                    ()
                else
                    pos <- 0

                let before = content.Substring (0, pos)
                let after = content.Substring cursor
                (before + after, pos)
