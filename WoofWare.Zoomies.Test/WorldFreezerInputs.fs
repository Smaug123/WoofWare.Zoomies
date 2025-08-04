namespace WoofWare.Zoomies.Test

open System.Collections.Concurrent

[<RequireQualifiedAccess>]
module WorldFreezerInputs =

    let make () : (unit -> bool) * (unit -> System.ConsoleKeyInfo) * (System.ConsoleKeyInfo -> unit) =
        let queue = ConcurrentQueue ()

        let isReady () = queue.Count > 0

        let rec getLatest () =
            match queue.TryDequeue () with
            | false, _ -> getLatest ()
            | true, v -> v

        let send k = queue.Enqueue k

        isReady, getLatest, send
