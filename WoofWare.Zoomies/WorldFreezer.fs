namespace WoofWare.Zoomies

open System
open System.Collections.Concurrent

type WorldStateChange<'appEvent> =
    | Keystroke of ConsoleKeyInfo
    | ApplicationEvent of 'appEvent

type WorldFreezer<'appEvent> =
    private
        {
            _Changes : ConcurrentQueue<WorldStateChange<'appEvent>>
            _RefreshExternal : unit -> unit
            _Post : 'appEvent -> unit
        }

    /// Load pending changes from the external world, like keystrokes, into the change list.
    member this.RefreshExternal () = this._RefreshExternal ()

    /// Dump any pending changes into a freshly cloned array. This clears the state of the internal buffer.
    member this.Changes () =
        lock
            this._Changes
            (fun () ->
                let result = this._Changes.ToArray ()
                this._Changes.Clear ()
                result
            )

    member this.PostAppEvent a = this._Post a

[<RequireQualifiedAccess>]
module WorldFreezer =
    /// Pass `fun () -> Console.KeyAvailable` for `keyAvailable`, and `fun () -> Console.ReadKey true` for `readKey`.
    let listen' (keyAvailable : unit -> bool) (readKey : unit -> ConsoleKeyInfo) : WorldFreezer<'appEvent> =
        let worldChanges = ConcurrentQueue<WorldStateChange<_>> ()

        let refreshExternal () =
            while keyAvailable () do
                readKey () |> WorldStateChange.Keystroke |> worldChanges.Enqueue

        let postAppEvent evt =
            worldChanges.Enqueue (WorldStateChange.ApplicationEvent evt)

        {
            _Changes = worldChanges
            _RefreshExternal = refreshExternal
            _Post = postAppEvent
        }

    let listen () : WorldFreezer<'appEvent> =
        listen' (fun () -> Console.KeyAvailable) (fun () -> Console.ReadKey true)
