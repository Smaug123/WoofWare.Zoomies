namespace WoofWare.Zoomies

open System

type ColorMode =
    | NoColor
    | Color

[<Struct>]
type TerminalCell =
    {
        Char : char
        BackgroundColor : ConsoleColor voption
        TextColor : ConsoleColor voption
    }

    static member OfChar (c : char) =
        {
            Char = c
            BackgroundColor = ValueNone
            TextColor = ValueNone
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
    | WriteRun of text : string * backgroundColor : ConsoleColor voption * textColor : ConsoleColor voption
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
    let execute
        (colorMode : ColorMode)
        (currentBackground : ConsoleColor)
        (currentForeground : ConsoleColor)
        (consoleWrite : string -> unit)
        (o : TerminalOp)
        : unit
        =
        match o with
        | TerminalOp.WriteRun (text, backgroundColor, textColor) ->
            let backgroundEscape =
                match colorMode, backgroundColor with
                | ColorMode.Color, ValueSome bg when bg <> currentBackground ->
                    Some (ConsoleColor.toBackgroundEscapeCode bg, ConsoleColor.toBackgroundEscapeCode currentBackground)
                | _ -> None

            let foregroundEscape =
                match colorMode, textColor with
                | ColorMode.Color, ValueSome fg when fg <> currentForeground ->
                    Some (ConsoleColor.toForegroundEscapeCode fg, ConsoleColor.toForegroundEscapeCode currentForeground)
                | _ -> None

            backgroundEscape |> Option.iter (fst >> consoleWrite)
            foregroundEscape |> Option.iter (fst >> consoleWrite)
            consoleWrite text
            foregroundEscape |> Option.iter (snd >> consoleWrite)
            backgroundEscape |> Option.iter (snd >> consoleWrite)
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
