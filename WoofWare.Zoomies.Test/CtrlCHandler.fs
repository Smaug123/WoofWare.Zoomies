namespace WoofWare.Zoomies.Test

open System.Threading
open WoofWare.Zoomies

[<RequireQualifiedAccess>]
module FakeCtrlCHandler =

    let make () : CtrlCHandler * (unit -> int) * (unit -> int) =
        let mutable registers = 0
        let mutable unregisters = 0

        let mock =
            {
                Register = fun _ -> Interlocked.Increment &registers |> ignore<int>
                Unregister = fun _ -> Interlocked.Increment &unregisters |> ignore<int>
            }

        let r () = registers
        let u () = unregisters

        mock, r, u
