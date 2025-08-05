namespace WoofWare.Zoomies

open System
open System.Collections.Generic

type WorldStateChange = | Keystroke of ConsoleKeyInfo

type WorldFreezer =
    private
        {
            /// Calling `Refresh` causes this IReadOnlyList to completely change its contents. You're expected not to
            /// be touching `Changes` when you call `Refresh` or while `Refresh` is running.
            _Changes : IReadOnlyList<WorldStateChange>
            _Refresh : unit -> unit
        }

    member this.Refresh () = this._Refresh ()
    member this.Changes () = this._Changes

[<RequireQualifiedAccess>]
module WorldFreezer =
    /// Pass `fun () -> Console.KeyAvailable` for `keyAvailable`, and `fun () -> Console.ReadKey true` for `readKey`.
    let listen' (keyAvailable : unit -> bool) (readKey : unit -> ConsoleKeyInfo) : WorldFreezer =
        let worldChanges = ResizeArray ()

        let freeze () =
            worldChanges.Clear ()

            while keyAvailable () do
                readKey () |> WorldStateChange.Keystroke |> worldChanges.Add

        {
            _Changes = worldChanges :> IReadOnlyList<_>
            _Refresh = freeze
        }

    let listen () : WorldFreezer =
        listen' (fun () -> Console.KeyAvailable) (fun () -> Console.ReadKey true)
