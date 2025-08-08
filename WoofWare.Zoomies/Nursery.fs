namespace WoofWare.Zoomies

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks

type internal Nursery () =
    let cts = new CancellationTokenSource ()
    let mutable taskCounter = 0

    let mutable activeSubmissions = 0

    let mutable isDisposing = 0
    let hasDisposed = TaskCompletionSource ()

    let tasks = ConcurrentDictionary<int, Task> ()

    member _.Submit<'a> (t : CancellationToken -> Task<'a>) : Task<'a> =
        if isDisposing = 0 then
            Interlocked.Increment &activeSubmissions |> ignore<int>
            let counter = Interlocked.Increment &taskCounter

            let t =
                task {
                    do! Task.Yield ()
                    let! result = t cts.Token

                    match tasks.TryRemove counter with
                    | true, _ ->
                        // happy path
                        ()
                    | false, _ ->
                        // we won the race with the outside task
                        // so we just leak the task here
                        // TODO: don't leak the completed task
                        ()

                    return result
                }

            tasks.TryAdd (counter, t) |> ignore<bool>

            Interlocked.Decrement &activeSubmissions |> ignore<int>
            t
        else
            raise (ObjectDisposedException (nameof Nursery))

    interface IAsyncDisposable with
        member _.DisposeAsync () : ValueTask =
            task {
                if Interlocked.Increment &isDisposing = 1 then
                    cts.Cancel ()
                    // Wait for no more submissions to be in flight
                    while activeSubmissions > 0 do
                        do! Task.Delay (TimeSpan.FromMilliseconds 10.0)

                    // Now `tasks` is no longer being mutated.
                    do! tasks |> Seq.map (fun (KeyValue (_, t)) -> t) |> Task.WhenAll

                    cts.Dispose ()
                    hasDisposed.SetResult ()
                else
                    do! hasDisposed.Task
            }
            |> ValueTask
