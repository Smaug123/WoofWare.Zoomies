namespace WoofWare.Zoomies

open System

type IConsole =
    {
        BackgroundColor : unit -> ConsoleColor
        ForegroundColor : unit -> ConsoleColor
        WindowWidth : unit -> int
        WindowHeight : unit -> int
        Execute : TerminalOp -> unit
    }

[<RequireQualifiedAccess>]
module IConsole =
    let make () =
        {
            BackgroundColor = fun () -> Console.BackgroundColor
            ForegroundColor = fun () -> Console.ForegroundColor
            WindowWidth = fun () -> Console.WindowWidth
            WindowHeight = fun () -> Console.WindowHeight
            Execute = fun o -> TerminalOp.execute Console.BackgroundColor Console.ForegroundColor Console.Write o
        }

    let defaultForTests =
        {
            BackgroundColor = fun () -> ConsoleColor.Black
            ForegroundColor = fun () -> ConsoleColor.White
            WindowWidth = fun () -> 80
            WindowHeight = fun () -> 10
            Execute = fun _ -> failwith "not implemented"
        }
