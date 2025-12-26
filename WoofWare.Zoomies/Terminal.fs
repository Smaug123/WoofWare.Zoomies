namespace WoofWare.Zoomies

open System

type ColorMode =
    | NoColor
    | Color

[<Struct>]
type TerminalCell =
    {
        Char : char
        BackgroundColor : Color
        TextColor : Color
        Bold : bool
    }

    static member OfChar (c : char) =
        {
            Char = c
            BackgroundColor = Color.Default
            TextColor = Color.Default
            Bold = false
        }

[<RequireQualifiedAccess>]
module Terminal =

    let clearLine () = Console.Write "\x1B[2K"
    let saveCursorPosition () = Console.Write "\x1B[s"
    let restoreCursorPosition () = Console.Write "\x1B[u"

type TerminalOp =
    | MoveCursor of x : int * y : int
    /// Write a run of characters with uniform styling.
    /// The text should not contain control characters or newlines.
    | WriteRun of text : string * backgroundColor : Color * textColor : Color * bold : bool
    | SetCursorVisibility of toVisible : bool
    | ClearScreen
    | EnterAlternateScreen
    | ExitAlternateScreen
    | RegisterMouseMode
    | UnregisterMouseMode
    | RegisterBracketedPaste
    | UnregisterBracketedPaste
    /// Begin synchronized update (DEC mode 2026). Terminals batch output until EndSynchronizedUpdate.
    /// Unsupporting terminals ignore this sequence.
    | BeginSynchronizedUpdate
    /// End synchronized update (DEC mode 2026). Terminal displays the batched output.
    | EndSynchronizedUpdate

[<RequireQualifiedAccess>]
module TerminalOp =
    let execute (colorMode : ColorMode) (consoleWrite : string -> unit) (o : TerminalOp) : unit =
        match o with
        | TerminalOp.WriteRun (text, backgroundColor, textColor, bold) ->
            match colorMode with
            | ColorMode.Color ->
                // Always emit colors (including Default, which emits SGR 39/49).
                // This ensures consistent behavior: each run explicitly sets its colors,
                // so Default-colored runs behave the same whether they're first or after
                // a colored run.
                consoleWrite (Color.toBackgroundEscapeCode backgroundColor)
                consoleWrite (Color.toForegroundEscapeCode textColor)

                // Emit bold if set
                if bold then
                    consoleWrite "\u001b[1m"

                consoleWrite text

                // Reset bold if it was set
                if bold then
                    consoleWrite "\u001b[22m"

            // No need to reset colors after: the next run will set its own colors.

            | ColorMode.NoColor -> consoleWrite text

        | TerminalOp.MoveCursor (x, y) -> consoleWrite $"\x1B[%d{y + 1};%d{x + 1}H"
        | TerminalOp.SetCursorVisibility visible ->
            if visible then
                consoleWrite "\x1B[?25h"
            else
                consoleWrite "\x1B[?25l"
        | TerminalOp.ClearScreen -> consoleWrite "\x1B[2J"
        | TerminalOp.EnterAlternateScreen -> consoleWrite "\x1B[?1049h"
        | TerminalOp.ExitAlternateScreen -> consoleWrite "\x1B[?1049l"
        | TerminalOp.RegisterMouseMode -> consoleWrite "\u001b[?1000;1006h"
        | TerminalOp.UnregisterMouseMode -> consoleWrite "\u001b[?1000;1006l"
        | TerminalOp.RegisterBracketedPaste -> consoleWrite "\u001b[?2004h"
        | TerminalOp.UnregisterBracketedPaste -> consoleWrite "\u001b[?2004l"
        | TerminalOp.BeginSynchronizedUpdate -> consoleWrite "\u001b[?2026h"
        | TerminalOp.EndSynchronizedUpdate -> consoleWrite "\u001b[?2026l"
