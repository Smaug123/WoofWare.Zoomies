namespace WoofWare.Zoomies.Test

open System.Collections.Concurrent
open WoofWare.Zoomies

/// Test utilities for WorldProcessor
[<RequireQualifiedAccess>]
module WorldProcessor =

    /// A WorldProcessor that ignores all events and returns the state unchanged.
    /// Generic only on state; assumes unit for app events and post-layout events (the overwhelmingly common case).
    let passthrough<'state> : WorldProcessor<unit, unit, 'state> =
        { new WorldProcessor<unit, unit, 'state> with
            member _.ProcessWorld (_, _, state) = ProcessWorldResult.make state
            member _.ProcessPostLayoutEvents (_, _, state) = state
        }

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
