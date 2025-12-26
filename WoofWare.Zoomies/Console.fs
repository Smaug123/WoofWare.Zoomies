namespace WoofWare.Zoomies

open System
open System.Text

type IConsole =
    {
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

        {
            WindowWidth = fun () -> Console.WindowWidth
            WindowHeight = fun () -> Console.WindowHeight
            ColorMode = colorMode
            Execute = fun o -> TerminalOp.execute colorMode (buffer.Append >> ignore) o
            Flush =
                fun () ->
                    if buffer.Length > 0 then
                        Console.Write (buffer.ToString ())
                        buffer.Clear () |> ignore
        }

    let defaultForTests =
        {
            WindowWidth = fun () -> 80
            WindowHeight = fun () -> 10
            ColorMode = ColorMode.Color
            Execute = fun _ -> failwith "not implemented"
            Flush = fun () -> ()
        }
