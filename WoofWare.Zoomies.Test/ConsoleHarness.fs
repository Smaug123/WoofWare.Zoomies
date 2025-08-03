namespace WoofWare.Zoomies.Test

open System
open System.Text
open WoofWare.Zoomies

type ConsoleHarness =
    private
        {
            Display : char[,]
            mutable CursorX : int
            mutable CursorY : int
        }

[<RequireQualifiedAccess>]
module ConsoleHarness =

    let empty (height : int) (width : int) : ConsoleHarness =
        {
            Display = Array2D.create height width ' '
            CursorX = 0
            CursorY = 0
        }

    let execute (c : ConsoleHarness) (op : TerminalOp) =
        match op with
        | TerminalOp.EnterAlternateScreen -> ()
        | TerminalOp.ExitAlternateScreen -> ()
        | TerminalOp.MoveCursor (x, y) ->
            c.CursorX <- x
            c.CursorY <- y
        | TerminalOp.WriteChar ch -> c.Display.[c.CursorY, c.CursorX] <- ch.Char
        | SetCursorVisibility _ -> ()
        | ClearScreen ->
            for y = 0 to Array2D.length1 c.Display - 1 do
                for x = 0 to Array2D.length2 c.Display - 1 do
                    c.Display.[y, x] <- ' '

    let toString (c : ConsoleHarness) : string =
        let sb = StringBuilder ()
        sb.Append '\n' |> ignore

        for y = 0 to Array2D.length1 c.Display - 1 do
            for x = 0 to Array2D.length2 c.Display - 1 do
                sb.Append c.Display.[y, x] |> ignore

            // Don't want whitespace stripping in the IDE to catch us out
            sb.Append "|\n" |> ignore

        sb.ToString ()

    let make () : IConsole * ConsoleHarness =
        let fake = empty 10 80

        let result =
            {
                BackgroundColor = fun () -> ConsoleColor.Black
                ForegroundColor = fun () -> ConsoleColor.White
                WindowWidth = fun () -> 80
                WindowHeight = fun () -> 10
                Execute = execute fake
            }

        result, fake
