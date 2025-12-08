namespace WoofWare.Zoomies

open System
open System.Text

type IConsole =
    {
        BackgroundColor : unit -> ConsoleColor
        ForegroundColor : unit -> ConsoleColor
        WindowWidth : unit -> int
        WindowHeight : unit -> int
        ColorMode : ColorMode
        Execute : TerminalOp -> unit
        /// Flush any buffered output to the console.
        /// Call this at the end of each render frame.
        Flush : unit -> unit
    }

[<RequireQualifiedAccess>]
module IConsole =
    let make (getEnv : string -> string option) =
        let colorMode =
            match getEnv "NO_COLOR" with
            | Some s when not (String.IsNullOrEmpty s) ->
                // https://no-color.org/
                // Command-line software which adds ANSI color to its output by default
                // should check for a NO_COLOR environment variable that, when present
                // and not an empty string (regardless of its value),
                // prevents the addition of ANSI color.
                ColorMode.NoColor
            | _ -> ColorMode.Color

        // Buffer all writes and flush once per frame to minimize syscalls
        let buffer = StringBuilder ()

        // Track the current terminal colors to avoid redundant color changes.
        // These are updated as we execute operations and reset to defaults at flush.
        let mutable currentBackground = Console.BackgroundColor
        let mutable currentForeground = Console.ForegroundColor

        {
            BackgroundColor = fun () -> Console.BackgroundColor
            ForegroundColor = fun () -> Console.ForegroundColor
            WindowWidth = fun () -> Console.WindowWidth
            WindowHeight = fun () -> Console.WindowHeight
            ColorMode = colorMode
            Execute =
                fun o ->
                    let struct (newBg, newFg) =
                        TerminalOp.execute colorMode currentBackground currentForeground (buffer.Append >> ignore) o

                    currentBackground <- newBg
                    currentForeground <- newFg
            Flush =
                fun () ->
                    // Reset colors to terminal defaults before flushing, if we changed them
                    if currentBackground <> Console.BackgroundColor then
                        buffer.Append (ConsoleColor.toBackgroundEscapeCode Console.BackgroundColor)
                        |> ignore

                    if currentForeground <> Console.ForegroundColor then
                        buffer.Append (ConsoleColor.toForegroundEscapeCode Console.ForegroundColor)
                        |> ignore

                    if buffer.Length > 0 then
                        Console.Write (buffer.ToString ())
                        buffer.Clear () |> ignore

                    // Reset tracked state to match terminal defaults for next frame
                    currentBackground <- Console.BackgroundColor
                    currentForeground <- Console.ForegroundColor
        }

    let defaultForTests =
        {
            BackgroundColor = fun () -> ConsoleColor.Black
            ForegroundColor = fun () -> ConsoleColor.White
            WindowWidth = fun () -> 80
            WindowHeight = fun () -> 10
            ColorMode = ColorMode.Color
            Execute = fun _ -> failwith "not implemented"
            Flush = fun () -> ()
        }
