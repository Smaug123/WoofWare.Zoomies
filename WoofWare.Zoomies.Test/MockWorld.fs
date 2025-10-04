namespace WoofWare.Zoomies.Test

open System.Collections.Concurrent

type MockWorld =
    {
        KeyAvailable : unit -> bool
        ReadKey : unit -> System.ConsoleKeyInfo
        SendKey : System.ConsoleKeyInfo -> unit
    }

[<RequireQualifiedAccess>]
module MockWorld =

    let make () : MockWorld =
        let queue = ConcurrentQueue ()

        let isReady () = queue.Count > 0

        let rec getLatest () =
            match queue.TryDequeue () with
            | false, _ -> getLatest ()
            | true, v -> v

        let send k = queue.Enqueue k

        {
            KeyAvailable = isReady
            ReadKey = getLatest
            SendKey = send
        }
