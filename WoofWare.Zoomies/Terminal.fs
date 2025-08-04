namespace WoofWare.Zoomies

open System

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

[<RequireQualifiedAccess>]
module TerminalOp =
    let execute
        (currentBackground : ConsoleColor)
        (currentForeground : ConsoleColor)
        (consoleWrite : string -> unit)
        (o : TerminalOp)
        : unit
        =
        match o with
        | TerminalOp.WriteChar c ->
            let backgroundEscape =
                match c.BackgroundColor with
                | ValueSome bg when bg <> currentBackground ->
                    Some (ConsoleColor.toBackgroundEscapeCode bg, ConsoleColor.toBackgroundEscapeCode currentBackground)
                | _ -> None

            let foregroundEscape =
                match c.TextColor with
                | ValueSome fg when fg <> currentForeground ->
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
