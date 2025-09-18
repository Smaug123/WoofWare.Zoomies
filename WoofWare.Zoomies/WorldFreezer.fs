namespace WoofWare.Zoomies

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks

type MouseButton =
    | Left
    | Middle
    | Right
    | ScrollUp
    | ScrollDown

type MouseModifiers =
    | None = 0
    | Shift = 4
    | Alt = 8
    | Control = 16

/// The top-left of the screen is (1, 1).
type MouseCoordinates =
    {
        /// High X means far to the right of the screen. The top-left corner is (1, 1).
        X : int
        /// High Y means far to the bottom of the screen. The top-left corner is (1, 1).
        Y : int
    }

type MouseEvent =
    | Press of MouseButton * MouseModifiers * MouseCoordinates
    | Release of MouseButton * MouseModifiers * MouseCoordinates

type KeyboardEvent =
    | BeginBracketedPaste
    | EndBracketedPaste

type RawWorldStateChange<'appEvent> =
    | Keystroke of ConsoleKeyInfo
    | ApplicationEvent of 'appEvent
    | ApplicationEventException of exn

type WorldStateChange<'appEvent> =
    | Keystroke of ConsoleKeyInfo
    | KeyboardEvent of KeyboardEvent
    | MouseEvent of MouseEvent
    | ApplicationEvent of 'appEvent
    | ApplicationEventException of exn

type private DequeueState =
    | Normal
    | EscReceived of ConsoleKeyInfo
    | OpenCSI of esc : ConsoleKeyInfo * bracket : ConsoleKeyInfo
    | SgrTracking of
        esc : ConsoleKeyInfo *
        bracket : ConsoleKeyInfo *
        angle : ConsoleKeyInfo *
        mode : (MouseButton * MouseModifiers) option *
        parameters : int list *
        pending : ConsoleKeyInfo list *
        processed : ConsoleKeyInfo list
    | ConsumingCSI of esc : ConsoleKeyInfo * bracket : ConsoleKeyInfo * keys : ConsoleKeyInfo list

    override this.ToString () =
        match this with
        | DequeueState.Normal -> "<waiting>"
        | DequeueState.EscReceived _ -> "ESC"
        | DequeueState.OpenCSI _ -> "ESC]"
        | DequeueState.SgrTracking (_, _, _, mode, parameters, pending, _) ->
            let parameters = parameters |> List.rev |> List.map string<int> |> String.concat ";"

            let pending =
                pending |> List.map (fun c -> c.KeyChar) |> List.rev |> List.toArray |> String

            $"ESC]< %O{mode} %s{parameters} ; pending: %s{pending}"
        | DequeueState.ConsumingCSI (_, _, keys) ->
            let keys =
                keys |> List.map (fun c -> c.KeyChar) |> List.rev |> List.toArray |> String

            $"<consuming CSI code: %s{keys}>"

type WorldFreezer<'appEvent> =
    private
        {
            _Nursery : Nursery
            /// This is populated character-by-character of input keystroke.
            /// Note, for example, that this may contain partially-read sequences of ANSI control characters!
            _Changes : ConcurrentQueue<RawWorldStateChange<'appEvent>>
            /// Invariant: this is only mutated within `this.Changes()`.
            mutable _DequeueState : DequeueState
            _RefreshExternal : unit -> unit
            _Post : (CancellationToken -> Task<'appEvent>) -> unit
        }

    /// Load pending changes from the external world, like keystrokes, into the change list.
    member this.RefreshExternal () = this._RefreshExternal ()

    /// Dump any pending changes into a freshly cloned array. This clears the state of the internal buffer.
    /// To save allocations, we don't give you back an array for the extremely common case where that array
    /// is empty.
    ///
    /// This function is *not* safe to call multiple times concurrently.
    member this.Changes () : WorldStateChange<'appEvent>[] voption =
        if this._Changes.IsEmpty then
            // Fine to have a TOCTTOU here. The next render loop will catch it if any events get added.
            ValueNone
        else
            let result = ResizeArray ()
            let mutable out = Unchecked.defaultof<_>

            while this._Changes.TryDequeue &out do
                // TODO: if it's been enough time since we saw the opening of an escape sequence, flush
                match this._DequeueState, out with
                | _, RawWorldStateChange.ApplicationEvent evt -> result.Add (WorldStateChange.ApplicationEvent evt)
                | _, RawWorldStateChange.ApplicationEventException exc ->
                    result.Add (WorldStateChange.ApplicationEventException exc)
                | DequeueState.Normal, RawWorldStateChange.Keystroke key ->
                    // not in the middle of an escape sequence
                    if key.Key = ConsoleKey.Escape && key.Modifiers = ConsoleModifiers.None then
                        this._DequeueState <- DequeueState.EscReceived key
                    else
                        result.Add (WorldStateChange.Keystroke key)
                | DequeueState.EscReceived esc, RawWorldStateChange.Keystroke key ->
                    // escape sequence begun
                    if key.KeyChar = '[' && key.Modifiers = ConsoleModifiers.None then
                        this._DequeueState <- DequeueState.OpenCSI (esc, key)
                    else
                        result.Add (WorldStateChange.Keystroke esc)
                        result.Add (WorldStateChange.Keystroke key)
                        this._DequeueState <- DequeueState.Normal
                | DequeueState.OpenCSI (esc, bracket), RawWorldStateChange.Keystroke key ->
                    match key.KeyChar with
                    | '<' ->
                        // SGR tracking mouse sequence
                        this._DequeueState <- DequeueState.SgrTracking (esc, bracket, key, None, [], [], [])
                    | i when '0' <= i && i <= '9' ->
                        this._DequeueState <- DequeueState.ConsumingCSI (esc, bracket, [ key ])
                    | c ->
                        failwith
                            $"TODO (Unrecognised ANSI control sequence: received char '%c{c}'): emit the processed keys instead"
                | DequeueState.ConsumingCSI (esc, bracket, pending), RawWorldStateChange.Keystroke key ->
                    if '0' <= key.KeyChar && key.KeyChar <= '9' then
                        this._DequeueState <- DequeueState.ConsumingCSI (esc, bracket, key :: pending)
                    else
                        let code =
                            (0, List.rev pending)
                            ||> Seq.fold (fun sum key -> sum * 10 + (int key.KeyChar - int '0'))

                        match code, key.KeyChar with
                        | 200, '~' ->
                            result.Add (WorldStateChange.KeyboardEvent KeyboardEvent.BeginBracketedPaste)
                            this._DequeueState <- DequeueState.Normal
                        | 201, '~' ->
                            result.Add (WorldStateChange.KeyboardEvent KeyboardEvent.EndBracketedPaste)
                            this._DequeueState <- DequeueState.Normal
                        | _ -> failwith $"TODO: don't know how to handle CSI code %i{code} with key %c{key.KeyChar}"
                | DequeueState.SgrTracking (esc, bracket, angle, button, parameters, pending, processed),
                  RawWorldStateChange.Keystroke key ->
                    match key.KeyChar with
                    | ';' ->
                        match button with
                        | None ->
                            let code =
                                (0, List.rev pending)
                                ||> Seq.fold (fun sum key -> sum * 10 + (int key.KeyChar - int '0'))

                            let button =
                                if code &&& 64 > 0 then
                                    if code &&& 1 = 0 then
                                        MouseButton.ScrollUp
                                    else
                                        MouseButton.ScrollDown
                                else
                                    match code &&& 3 with
                                    | 0 -> MouseButton.Left
                                    | 1 -> MouseButton.Middle
                                    | 2 -> MouseButton.Right
                                    | _ ->
                                        failwith
                                            $"TODO (unexpected mouse specifier: %i{code}): emit the processed keys instead"

                            let mutable modifiers = MouseModifiers.None

                            modifiers <-
                                modifiers
                                ||| (if code &&& 4 > 0 then
                                         MouseModifiers.Shift
                                     else
                                         MouseModifiers.None)

                            modifiers <-
                                modifiers
                                ||| (if code &&& 8 > 0 then
                                         MouseModifiers.Alt
                                     else
                                         MouseModifiers.None)

                            modifiers <-
                                modifiers
                                ||| (if code &&& 16 > 0 then
                                         MouseModifiers.Control
                                     else
                                         MouseModifiers.None)

                            this._DequeueState <-
                                DequeueState.SgrTracking (
                                    esc,
                                    bracket,
                                    angle,
                                    Some (button, modifiers),
                                    parameters,
                                    [],
                                    key :: processed
                                )
                        | Some button ->
                            // End the parameter!
                            let paramValue =
                                (0, List.rev pending)
                                ||> Seq.fold (fun sum key -> sum * 10 + (int key.KeyChar - int '0'))

                            this._DequeueState <-
                                DequeueState.SgrTracking (
                                    esc,
                                    bracket,
                                    angle,
                                    Some button,
                                    paramValue :: parameters,
                                    [],
                                    key :: processed
                                )
                    | x when '0' <= x && x <= '9' ->
                        this._DequeueState <-
                            DequeueState.SgrTracking (
                                esc,
                                bracket,
                                angle,
                                button,
                                parameters,
                                key :: pending,
                                key :: processed
                            )
                    | 'm'
                    | 'M' ->
                        match button with
                        | None -> failwith "TODO: expected mouse button, got M"
                        | Some (button, modifiers) ->
                            match parameters with
                            | [ x ] ->
                                let code =
                                    (0, List.rev pending)
                                    ||> Seq.fold (fun sum key -> sum * 10 + (int key.KeyChar - int '0'))

                                let coordinates =
                                    {
                                        X = x
                                        Y = code
                                    }

                                if key.KeyChar = 'M' then
                                    result.Add (
                                        WorldStateChange.MouseEvent (MouseEvent.Press (button, modifiers, coordinates))
                                    )
                                else
                                    assert (key.KeyChar = 'm')

                                    result.Add (
                                        WorldStateChange.MouseEvent (
                                            MouseEvent.Release (button, modifiers, coordinates)
                                        )
                                    )
                            | _ -> failwith $"TODO: expected exactly one parameter already parsed"

                            this._DequeueState <- DequeueState.Normal
                    | c -> failwith $"TODO: unrecognised char '%c{c}'"

            result.ToArray () |> ValueSome

    member this.PostAppEvent a = this._Post a

    interface IAsyncDisposable with
        member this.DisposeAsync () =
            (this._Nursery :> IAsyncDisposable).DisposeAsync ()

[<RequireQualifiedAccess>]
module WorldFreezer =
    /// Pass `fun () -> Console.KeyAvailable` for `keyAvailable`, and `fun () -> Console.ReadKey true` for `readKey`.
    let listen'<'appEvent> (keyAvailable : unit -> bool) (readKey : unit -> ConsoleKeyInfo) : WorldFreezer<'appEvent> =
        let worldChanges = ConcurrentQueue<RawWorldStateChange<_>> ()

        let runningTasks = Nursery ()

        let refreshExternal () =
            while keyAvailable () do
                let key = readKey ()
                RawWorldStateChange.Keystroke key |> worldChanges.Enqueue

        let postAppEvent (evt : CancellationToken -> Task<'appEvent>) : unit =
            task {
                // The only exception `runningTasks.Submit` can throw is OperationDisposedException.
                // If we get that, the listener is already being shut down, so we're no longer rerendering
                // over on the render thread.
                // That means there's no point sending a message to the render thread about any errors
                // (because the render thread will never do anything with that message),
                // so it's fine to simply ignore any exceptions that are thrown during the `Submit` call itself.
                let running = runningTasks.Submit evt

                try
                    let! result = running
                    worldChanges.Enqueue (RawWorldStateChange.ApplicationEvent result)
                with e ->
                    worldChanges.Enqueue (RawWorldStateChange.ApplicationEventException e)
            }
            |> ignore<Task>

        {
            _Changes = worldChanges
            _RefreshExternal = refreshExternal
            _Post = postAppEvent
            _Nursery = runningTasks
            _DequeueState = DequeueState.Normal
        }

    let listen<'appEvent> () : WorldFreezer<'appEvent> =
        listen' (fun () -> Console.KeyAvailable) (fun () -> Console.ReadKey true)
