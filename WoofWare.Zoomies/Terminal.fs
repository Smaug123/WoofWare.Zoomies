namespace WoofWare.Zoomies

open System
open System.Text

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

    let moveCursor (x : int) (y : int) =
        Console.Write $"\x1B[%d{y + 1};%d{x + 1}H"

    let clearLine () = Console.Write "\x1B[2K"
    let hideCursor () = Console.Write "\x1B[?25l"
    let showCursor () = Console.Write "\x1B[?25h"
    let saveCursorPosition () = Console.Write "\x1B[s"
    let restoreCursorPosition () = Console.Write "\x1B[u"

type TerminalOp =
    | MoveCursor of x : int * y : int
    | WriteChar of TerminalCell
    | SetCursorVisibility of toVisible : bool
    | ClearScreen

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
