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
    | WriteChar of TerminalCell
    | SetCursorVisibility of toVisible : bool
    | ClearScreen
    | EnterAlternateScreen
    | ExitAlternateScreen
    | RegisterMouseMode
    | UnregisterMouseMode
    | RegisterBracketedPaste
    | UnregisterBracketedPaste

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
        | TerminalOp.WriteChar c ->
            let backgroundEscape =
                match colorMode, c.BackgroundColor with
                | ColorMode.Color, ValueSome bg when bg <> currentBackground ->
                    Some (ConsoleColor.toBackgroundEscapeCode bg, ConsoleColor.toBackgroundEscapeCode currentBackground)
                | _ -> None

            let foregroundEscape =
                match colorMode, c.TextColor with
                | ColorMode.Color, ValueSome fg when fg <> currentForeground ->
                    Some (ConsoleColor.toForegroundEscapeCode fg, ConsoleColor.toForegroundEscapeCode currentForeground)
                | _ -> None

            backgroundEscape |> Option.iter (fst >> consoleWrite)
            foregroundEscape |> Option.iter (fst >> consoleWrite)
            consoleWrite $"%c{c.Char}"
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
