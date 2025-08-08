namespace WoofWare.Zoomies

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks

type WorldStateChange<'appEvent> =
    | Keystroke of ConsoleKeyInfo
    | ApplicationEvent of 'appEvent
    | ApplicationEventException of exn

type WorldFreezer<'appEvent> =
    private
        {
            _Nursery : Nursery
            _Changes : ConcurrentQueue<WorldStateChange<'appEvent>>
            _RefreshExternal : unit -> unit
            _Post : (CancellationToken -> Task<'appEvent>) -> unit
        }

    /// Load pending changes from the external world, like keystrokes, into the change list.
    member this.RefreshExternal () = this._RefreshExternal ()

    /// Dump any pending changes into a freshly cloned array. This clears the state of the internal buffer.
    /// To save allocations, we don't give you back an array for the extremely common case where that array
    /// is empty.
    member this.Changes () =
        lock
            this._Changes
            (fun () ->
                if this._Changes.IsEmpty then
                    ValueNone
                else
                    let result = this._Changes.ToArray ()
                    this._Changes.Clear ()
                    result |> ValueSome
            )

    member this.PostAppEvent a = this._Post a

    interface IAsyncDisposable with
        member this.DisposeAsync () =
            (this._Nursery :> IAsyncDisposable).DisposeAsync ()

[<RequireQualifiedAccess>]
module WorldFreezer =
    /// Pass `fun () -> Console.KeyAvailable` for `keyAvailable`, and `fun () -> Console.ReadKey true` for `readKey`.
    let listen'<'appEvent> (keyAvailable : unit -> bool) (readKey : unit -> ConsoleKeyInfo) : WorldFreezer<'appEvent> =
        let worldChanges = ConcurrentQueue<WorldStateChange<_>> ()

        let runningTasks = Nursery ()

        let refreshExternal () =
            while keyAvailable () do
                readKey () |> WorldStateChange.Keystroke |> worldChanges.Enqueue

        let postAppEvent (evt : CancellationToken -> Task<'appEvent>) : unit =
            task {
                let running = runningTasks.Submit evt

                try
                    let! result = running
                    worldChanges.Enqueue (WorldStateChange.ApplicationEvent result)
                with e ->
                    worldChanges.Enqueue (WorldStateChange.ApplicationEventException e)
            }
            |> ignore<Task>

        {
            _Changes = worldChanges
            _RefreshExternal = refreshExternal
            _Post = postAppEvent
            _Nursery = runningTasks
        }

    let listen<'appEvent> () : WorldFreezer<'appEvent> =
        listen' (fun () -> Console.KeyAvailable) (fun () -> Console.ReadKey true)
