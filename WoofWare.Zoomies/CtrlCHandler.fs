namespace WoofWare.Zoomies

open System

type CtrlCHandler =
    {
        Register : ConsoleCancelEventHandler -> unit
        Unregister : ConsoleCancelEventHandler -> unit
    }

[<RequireQualifiedAccess>]
module CtrlCHandler =
    let make () =
        {
            Register = Console.CancelKeyPress.AddHandler
            Unregister = Console.CancelKeyPress.RemoveHandler
        }
