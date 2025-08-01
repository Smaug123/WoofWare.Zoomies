namespace WoofWare.Zoomies

open System

[<Struct>]
type TerminalCell =
    {
        Char : char
    }

[<RequireQualifiedAccess>]
module Terminal =

    let moveCursor (x : int) (y : int) =
        Console.Write $"\x1B[%d{y + 1};%d{x + 1}H"

    let clearScreen () = Console.Write "\x1B[2J"
    let clearLine () = Console.Write "\x1B[2K"
    let hideCursor () = Console.Write "\x1B[?25l"
    let showCursor () = Console.Write "\x1B[?25h"
    let saveCursorPosition () = Console.Write "\x1B[s"
    let restoreCursorPosition () = Console.Write "\x1B[u"

type TerminalOp =
    | MoveCursor of x : int * y : int
    | WriteChar of char

[<RequireQualifiedAccess>]
module TerminalOp =
    let execute (consoleWrite : string -> unit) (o : TerminalOp) : unit =
        match o with
        | TerminalOp.WriteChar c -> consoleWrite $"%c{c}"
        | TerminalOp.MoveCursor (x, y) -> consoleWrite $"\x1B[%d{y + 1};%d{x + 1}H"
