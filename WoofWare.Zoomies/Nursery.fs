namespace WoofWare.Zoomies

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks

type internal Nursery () =
    let cts = new CancellationTokenSource ()
    let mutable taskCounter = 0

    /// Currently-running calls to `Submit` are tracked so that `Dispose` can know whether any
    /// tasks are in the process of being created.
    ///
    /// Invariant:
    /// Once `isDisposing` becomes nonzero, if `activeSubmissions` ever hits zero then `tasks` will never grow
    /// (even if `activeSubmissions` becomes nonzero again in the future).
    let mutable activeSubmissions = 0

    let mutable isDisposing = 0
    /// A second call to `Dispose` must avoid repeating the work of the first call.
    /// We instead just give subsequent calls a Task to wait for.
    let hasDisposed = TaskCompletionSource ()

    let childTasks = ConcurrentDictionary<int, Task> ()

    member _.Submit<'a> (t : CancellationToken -> Task<'a>) : Task<'a> =
        Interlocked.Increment &activeSubmissions |> ignore<int>

        try
            if isDisposing <> 0 then
                raise (ObjectDisposedException (nameof Nursery))

            let counter = Interlocked.Increment &taskCounter

            let t =
                task {
                    do! Task.Yield ()
                    let! result = t cts.Token

                    match childTasks.TryRemove counter with
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

            childTasks.TryAdd (counter, t) |> ignore<bool>
            t
        finally
            Interlocked.Decrement &activeSubmissions |> ignore<int>

    interface IAsyncDisposable with
        member _.DisposeAsync () : ValueTask =
            task {
                if Interlocked.Increment &isDisposing = 1 then
                    cts.Cancel ()

                    // Wait for no more submissions to be in flight.
                    // The `activeSubmissions` number might grow and shrink, but `Submit` will send
                    // `ObjectDisposedException` to any callers from this point, so hopefully they
                    // stop sending us submissions and eventually all the currently-running `Submit` calls
                    // (which are individually very cheap; they're only creating new Tasks) complete.
                    while activeSubmissions > 0 do
                        do! Task.Delay (TimeSpan.FromMilliseconds 10.0)

                    // Now `tasks` is no longer being mutated, because `isDisposing` is preventing any new
                    // callers to `Submit`, and all existing callers have got through to the last line,
                    // which decremented `activeSubmissions`.
                    // So we can simply await all the tasks in it.
                    do! childTasks |> Seq.map (fun (KeyValue (_, t)) -> t) |> Task.WhenAll

                    // Nothing else now depends on the CancellationTokenSource.
                    cts.Dispose ()

                    // Signal to any parallel callers of `Dispose` that the disposal has completed.
                    hasDisposed.SetResult ()
                else
                    do! hasDisposed.Task
            }
            |> ValueTask
