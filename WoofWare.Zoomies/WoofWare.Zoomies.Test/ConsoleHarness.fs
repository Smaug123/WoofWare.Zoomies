namespace WoofWare.Zoomies.Test

open System.Text
open WoofWare.Zoomies

type FakeConsole =
    private
        {
            Display : char[,]
            mutable CursorX : int
            mutable CursorY : int
        }

[<RequireQualifiedAccess>]
module ConsoleHarness =

    let empty (height : int) (width : int) : FakeConsole =
        {
            Display = Array2D.create height width ' '
            CursorX = 0
            CursorY = 0
        }

    let execute (op : TerminalOp) (c : FakeConsole) =
        match op with
        | TerminalOp.MoveCursor (x, y) ->
            c.CursorX <- x
            c.CursorY <- y
        | TerminalOp.WriteChar ch -> c.Display.[c.CursorY, c.CursorX] <- ch

    let toString (c : FakeConsole) : string =
        let sb = StringBuilder ()
        sb.Append '\n' |> ignore

        for y = 0 to Array2D.length1 c.Display - 1 do
            for x = 0 to Array2D.length2 c.Display - 1 do
                sb.Append c.Display.[y, x] |> ignore

            sb.Append '\n' |> ignore

        sb.ToString ()
