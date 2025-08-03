namespace WoofWare.Zoomies

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

type WorldStateChange = | Keystroke of ConsoleKeyInfo

type WorldFreezer =
    private
        {
            Cts : CancellationTokenSource
            _IsShutDown : Task
            /// Calling `Refresh` causes this IReadOnlyList to completely change its contents. You're expected not to
            /// be touching `Changes` when you call `Refresh` or while `Refresh` is running.
            _Changes : IReadOnlyList<WorldStateChange>
            _Refresh : unit -> unit
        }

    member this.IsShutDown = this._IsShutDown
    member this.Refresh () = this._Refresh ()
    member this.Changes () = this._Changes

    interface IDisposable with
        member this.Dispose () : unit =
            this.Cts.Cancel ()
            this.IsShutDown.Wait ()
            this.Cts.Dispose ()

[<RequireQualifiedAccess>]
module WorldFreezer =
    /// Pass `fun () -> Console.KeyAvailable` for `keyAvailable`, and `fun () -> Console.ReadKey true` for `readKey`.
    let listen' (keyAvailable : unit -> bool) (readKey : unit -> ConsoleKeyInfo) : WorldFreezer =
        let isShutDown = TaskCompletionSource ()
        let keystrokeBuffer = ConcurrentQueue ()

        let cts = new CancellationTokenSource ()

        let _keystrokeCollector =
            Task.Factory.StartNew (
                (fun () ->
                    while not cts.Token.IsCancellationRequested do
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
            _IsShutDown = isShutDown.Task
            _Changes = worldChanges :> IReadOnlyList<_>
            _Refresh = freeze
            Cts = cts
        }

    let listen () : WorldFreezer =
        listen' (fun () -> Console.KeyAvailable) (fun () -> Console.ReadKey true)
