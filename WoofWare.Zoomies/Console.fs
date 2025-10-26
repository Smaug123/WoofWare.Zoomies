namespace WoofWare.Zoomies

open System

type IConsole =
    {
        BackgroundColor : unit -> ConsoleColor
        ForegroundColor : unit -> ConsoleColor
        WindowWidth : unit -> int
        WindowHeight : unit -> int
        ColorMode : ColorMode
        Execute : TerminalOp -> unit
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

        {
            BackgroundColor = fun () -> Console.BackgroundColor
            ForegroundColor = fun () -> Console.ForegroundColor
            WindowWidth = fun () -> Console.WindowWidth
            WindowHeight = fun () -> Console.WindowHeight
            ColorMode = colorMode
            Execute =
                fun o -> TerminalOp.execute colorMode Console.BackgroundColor Console.ForegroundColor Console.Write o
        }

    let defaultForTests =
        {
            BackgroundColor = fun () -> ConsoleColor.Black
            ForegroundColor = fun () -> ConsoleColor.White
            WindowWidth = fun () -> 80
            WindowHeight = fun () -> 10
            ColorMode = ColorMode.Color
            Execute = fun _ -> failwith "not implemented"
        }
