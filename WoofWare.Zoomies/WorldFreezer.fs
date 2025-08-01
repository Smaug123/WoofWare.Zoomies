namespace WoofWare.Zoomies

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

type WorldStateChange = | Keystroke of ConsoleKeyInfo

type WorldFreezer =
    {
        IsShutDown : Task
        /// Calling `Refresh` causes this IReadOnlyList to completely change its contents. You're expected not to
        /// be touching `Changes` when you call `Refresh` or while `Refresh` is running.
        Changes : IReadOnlyList<WorldStateChange>
        Refresh : unit -> unit
    }

[<RequireQualifiedAccess>]
module WorldFreezer =
    /// Pass `fun () -> Console.KeyAvailable` for `keyAvailable`, and `fun () -> Console.ReadKey true` for `readKey`.
    let listen'
        (keyAvailable : unit -> bool)
        (readKey : unit -> ConsoleKeyInfo)
        (cancel : CancellationToken)
        : WorldFreezer
        =
        let isShutDown = TaskCompletionSource ()
        let keystrokeBuffer = ConcurrentQueue ()

        let _keystrokeCollector =
            Task.Factory.StartNew (
                (fun () ->
                    while not cancel.IsCancellationRequested do
                        if keyAvailable () then
                            keystrokeBuffer.Enqueue (readKey ())

                    isShutDown.SetResult ()
                ),
                TaskCreationOptions.LongRunning
            )

        let worldChanges = ResizeArray ()

        let freeze () =
            worldChanges.Clear ()
            let mutable change = Unchecked.defaultof<_>

            while keystrokeBuffer.TryDequeue &change do
                worldChanges.Add (WorldStateChange.Keystroke change)

        {
            IsShutDown = isShutDown.Task
            Changes = worldChanges :> IReadOnlyList<_>
            Refresh = freeze
        }

    let listen (cancel : CancellationToken) : WorldFreezer =
        listen' (fun () -> Console.KeyAvailable) (fun () -> Console.ReadKey true) cancel
