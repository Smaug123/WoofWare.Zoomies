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
        | TerminalOp.RegisterMouseMode -> ()
        | TerminalOp.UnregisterMouseMode -> ()
        | TerminalOp.RegisterBracketedPaste -> ()
        | TerminalOp.UnregisterBracketedPaste -> ()
        | TerminalOp.BeginSynchronizedUpdate -> ()
        | TerminalOp.EndSynchronizedUpdate -> ()
        | TerminalOp.ResetAttributes -> ()
        | TerminalOp.MoveCursor (x, y) ->
            c.CursorX <- x
            c.CursorY <- y
        | TerminalOp.WriteRun (text, _, _, _) ->
            for ch in text do
                c.Display.[c.CursorY, c.CursorX] <- ch
                // Advance cursor right (real terminals do this automatically).
                // At row end, set to absurd values to catch code that incorrectly
                // assumes cursor position after hitting the edge.
                if c.CursorX + 1 < Array2D.length2 c.Display then
                    c.CursorX <- c.CursorX + 1
                else
                    c.CursorX <- Int32.MinValue
                    c.CursorY <- Int32.MinValue
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

    let make' (width : unit -> int) (height : unit -> int) : IConsole * ConsoleHarness =
        // TODO: make console dynamically resize
        let fake = empty (height ()) (width ())

        let result =
            {
                WindowWidth = width
                WindowHeight = height
                ColorMode = ColorMode.Color
                Execute = execute fake
                Flush = fun () -> ()
            }

        result, fake

    let make () : IConsole * ConsoleHarness = make' (fun () -> 80) (fun () -> 10)
