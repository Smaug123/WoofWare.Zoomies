namespace WoofWare.Zoomies

open System
open System.Collections.Concurrent
open System.Runtime.InteropServices
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

type private SgrDecode =
    {
        mutable Mode : (MouseButton * MouseModifiers) option
        Parameters : ResizeArray<int>
        mutable Cursor : int
    }

    static member Empty (cursor : int) =
        {
            Mode = None
            Parameters = ResizeArray ()
            Cursor = cursor
        }

type private CsiDecodeState =
    | ConsumingIdentifier
    | SgrTracking of SgrDecode

type private AnsiDecodeState = | Csi of bracket : ConsoleKeyInfo * CsiDecodeState

type private DequeueState =
    {
        /// All the characters after the initial `ESC`.
        Processed : ResizeArray<ConsoleKeyInfo>
        /// The int64 is the timestamp at which we consumed this Esc.
        mutable Esc : (int64 * ConsoleKeyInfo) voption
        mutable Bracket : ConsoleKeyInfo voption
        mutable AngleBracket : ConsoleKeyInfo voption
        mutable State : AnsiDecodeState voption
        ParsedParameters : ResizeArray<int>
    }

    static member Empty () =
        {
            Processed = ResizeArray ()
            Esc = ValueNone
            Bracket = ValueNone
            AngleBracket = ValueNone
            State = ValueNone
            ParsedParameters = ResizeArray ()
        }

    member inline this.IsNormal = this.Esc.IsNone

    member this.ReemitIfTime (sw : IStopwatch) =
        match this.Esc with
        | ValueNone -> None
        | ValueSome (ts, esc) ->
            if (sw.GetTimestamp () - ts |> float) / float sw.Frequency > 0.01 then
                let result = Array.zeroCreate (this.Processed.Count + 1)
                Array.set result 0 esc
                CollectionsMarshal.AsSpan(this.Processed).CopyTo (result.AsSpan 1)
                Some result
            else
                None

    member this.Clear () =
        this.Processed.Clear ()
        this.ParsedParameters.Clear ()
        this.AngleBracket <- ValueNone
        this.Bracket <- ValueNone
        this.Esc <- ValueNone
        this.State <- ValueNone

type UnrecognisedEscapeCodeBehaviour =
    | Throw
    | PassThrough

type WorldFreezer<'appEvent> =
    private
        {
            _Nursery : Nursery
            /// This is populated character-by-character of input keystroke.
            /// Note, for example, that this may contain partially-read sequences of ANSI control characters!
            _Changes : ConcurrentQueue<RawWorldStateChange<'appEvent>>
            /// Invariant: this is only mutated within `this.Changes()`.
            mutable _DequeueState : DequeueState
            _Stopwatch : IStopwatch
            _RefreshExternal : unit -> unit
            _Behaviour : UnrecognisedEscapeCodeBehaviour
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
        if this._Changes.IsEmpty && this._DequeueState.IsNormal then
            // Fine to have a TOCTTOU here. The next render loop will catch it if any events get added.
            ValueNone
        else
            let result = ResizeArray ()
            let mutable out = Unchecked.defaultof<_>

            while this._Changes.TryDequeue &out do
                // Note: the tests assume that key.Key is only ever compared to ConsoleKey.Escape, and that the
                // Modifiers are only ever compared to None.
                // Don't break that property!

                match out with
                | RawWorldStateChange.ApplicationEvent evt -> result.Add (WorldStateChange.ApplicationEvent evt)
                | RawWorldStateChange.ApplicationEventException exc ->
                    result.Add (WorldStateChange.ApplicationEventException exc)
                | RawWorldStateChange.Keystroke key ->

                match this._DequeueState.Esc with
                | ValueNone ->
                    // not in the middle of an escape sequence
                    if key.Key = ConsoleKey.Escape && key.Modifiers = ConsoleModifiers.None then
                        this._DequeueState.Esc <- ValueSome (this._Stopwatch.GetTimestamp (), key)
                    else
                        result.Add (WorldStateChange.Keystroke key)
                | ValueSome (_ts, esc) ->

                let emitBuffered () =
                    result.Add (WorldStateChange.Keystroke esc)

                    for p in this._DequeueState.Processed do
                        result.Add (WorldStateChange.Keystroke p)

                    this._DequeueState.Clear ()

                this._DequeueState.Processed.Add key

                match this._DequeueState.Bracket with
                | ValueNone ->
                    if key.KeyChar = '[' && key.Modifiers = ConsoleModifiers.None then
                        // escape sequence begun
                        this._DequeueState.Bracket <- ValueSome key
                    else
                        emitBuffered ()
                | ValueSome bracket ->

                match this._DequeueState.State with
                | ValueNone ->
                    if key.KeyChar = '<' then
                        // SGR tracking mouse sequence
                        this._DequeueState.State <-
                            ValueSome (
                                AnsiDecodeState.Csi (
                                    key,
                                    CsiDecodeState.SgrTracking (SgrDecode.Empty this._DequeueState.Processed.Count)
                                )
                            )
                    elif '0' <= key.KeyChar && key.KeyChar <= '9' then
                        // consuming a numerical parameter
                        this._DequeueState.State <-
                            ValueSome (AnsiDecodeState.Csi (key, CsiDecodeState.ConsumingIdentifier))
                    else
                        match this._Behaviour with
                        | UnrecognisedEscapeCodeBehaviour.Throw ->
                            failwith $"Unrecognised ANSI control sequence: received char '%c{key.KeyChar}'"
                        | UnrecognisedEscapeCodeBehaviour.PassThrough -> emitBuffered ()
                | ValueSome (AnsiDecodeState.Csi (angleBracket, CsiDecodeState.ConsumingIdentifier)) ->

                    if '0' <= key.KeyChar && key.KeyChar <= '9' then
                        ()
                    else
                        let code =
                            let mutable result = 0
                            // The last element is the (non-numeric) character we're parsing right now, so skip it;
                            // the first character is [, so skip that too.
                            for i = 1 to this._DequeueState.Processed.Count - 2 do
                                result <- result * 10 + int this._DequeueState.Processed.[i].KeyChar - int '0'

                            result

                        match code, key.KeyChar with
                        | 200, '~' ->
                            result.Add (WorldStateChange.KeyboardEvent KeyboardEvent.BeginBracketedPaste)
                            this._DequeueState.Clear ()
                        | 201, '~' ->
                            result.Add (WorldStateChange.KeyboardEvent KeyboardEvent.EndBracketedPaste)
                            this._DequeueState.Clear ()
                        | _ ->
                            match this._Behaviour with
                            | UnrecognisedEscapeCodeBehaviour.Throw ->
                                failwith $"don't know how to handle CSI code %i{code} with key %c{key.KeyChar}"
                            | UnrecognisedEscapeCodeBehaviour.PassThrough -> emitBuffered ()
                | ValueSome (AnsiDecodeState.Csi (angleBracket, CsiDecodeState.SgrTracking state)) ->
                    match key.KeyChar with
                    | ';' ->
                        match state.Mode with
                        | None ->
                            let code =
                                let mutable result = 0
                                // The last element is the semicolon we're parsing right now, so skip it.
                                for i = state.Cursor to this._DequeueState.Processed.Count - 2 do
                                    result <- result * 10 + int this._DequeueState.Processed.[i].KeyChar - int '0'

                                state.Cursor <- this._DequeueState.Processed.Count
                                result

                            let button =
                                if code &&& 64 > 0 then
                                    if code &&& 1 = 0 then
                                        Some MouseButton.ScrollUp
                                    else
                                        Some MouseButton.ScrollDown
                                else
                                    match code &&& 3 with
                                    | 0 -> Some MouseButton.Left
                                    | 1 -> Some MouseButton.Middle
                                    | 2 -> Some MouseButton.Right
                                    | _ -> None

                            match button with
                            | None ->
                                match this._Behaviour with
                                | UnrecognisedEscapeCodeBehaviour.Throw ->
                                    failwith $"unexpected mouse specifier: %i{code}"
                                | UnrecognisedEscapeCodeBehaviour.PassThrough -> emitBuffered ()

                            | Some button ->

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

                            state.Mode <- Some (button, modifiers)
                            state.Cursor <- this._DequeueState.Processed.Count
                        | Some button ->
                            let paramValue =
                                let mutable result = 0
                                // The last element is the semicolon we're parsing right now, so skip it.
                                for i = state.Cursor to this._DequeueState.Processed.Count - 2 do
                                    result <- result * 10 + int this._DequeueState.Processed.[i].KeyChar - int '0'

                                state.Cursor <- this._DequeueState.Processed.Count
                                result

                            state.Parameters.Add paramValue
                    | x when '0' <= x && x <= '9' -> ()
                    | 'm'
                    | 'M' ->
                        match state.Mode with
                        | None ->
                            match this._Behaviour with
                            | UnrecognisedEscapeCodeBehaviour.Throw ->
                                failwith $"Expected mouse button specifier; got %c{key.KeyChar}"
                            | UnrecognisedEscapeCodeBehaviour.PassThrough -> emitBuffered ()

                        | Some (button, modifiers) ->
                            if state.Parameters.Count = 1 then
                                let x = state.Parameters.[0]

                                let code =
                                    let mutable result = 0
                                    // The last element is the semicolon we're parsing right now, so skip it.
                                    for i = state.Cursor to this._DequeueState.Processed.Count - 2 do
                                        result <- result * 10 + int this._DequeueState.Processed.[i].KeyChar - int '0'

                                    result

                                let coordinates =
                                    {
                                        X = x
                                        Y = code
                                    }

                                if key.KeyChar = 'M' then
                                    WorldStateChange.MouseEvent (MouseEvent.Press (button, modifiers, coordinates))
                                    |> result.Add
                                else
                                    assert (key.KeyChar = 'm')

                                    MouseEvent.Release (button, modifiers, coordinates)
                                    |> WorldStateChange.MouseEvent
                                    |> result.Add

                                this._DequeueState.Clear ()
                            else
                                match this._Behaviour with
                                | UnrecognisedEscapeCodeBehaviour.Throw ->
                                    let s = state.Parameters |> Seq.map string<int> |> String.concat ";"
                                    failwith $"expected exactly one parameter already parsed; got %s{s}"
                                | UnrecognisedEscapeCodeBehaviour.PassThrough -> emitBuffered ()
                    | c ->
                        match this._Behaviour with
                        | UnrecognisedEscapeCodeBehaviour.Throw ->
                            failwith $"Unrecognised char '%c{c}' in ANSI SGR mouse-handling escape code"
                        | UnrecognisedEscapeCodeBehaviour.PassThrough -> emitBuffered ()

            match this._DequeueState.ReemitIfTime this._Stopwatch with
            | Some expiredKeys ->
                expiredKeys |> Seq.map WorldStateChange.Keystroke |> result.AddRange
                this._DequeueState.Clear ()
            | None -> ()

            if result.Count = 0 then
                // This can happen if we are awaiting more keystrokes in an ANSI escape sequence.
                ValueNone
            else
                result.ToArray () |> ValueSome

    member this.PostAppEvent a = this._Post a

    interface IAsyncDisposable with
        member this.DisposeAsync () =
            (this._Nursery :> IAsyncDisposable).DisposeAsync ()

[<RequireQualifiedAccess>]
module WorldFreezer =
    /// Pass `fun () -> Console.KeyAvailable` for `keyAvailable`, `Stopwatch.system` for `stopwatch`,
    /// and `fun () -> Console.ReadKey true` for `readKey`.
    let listen'<'appEvent>
        (behaviour : UnrecognisedEscapeCodeBehaviour)
        (stopwatch : IStopwatch)
        (keyAvailable : unit -> bool)
        (readKey : unit -> ConsoleKeyInfo)
        : WorldFreezer<'appEvent>
        =
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
            _Stopwatch = stopwatch
            _DequeueState = DequeueState.Empty ()
            _Behaviour = behaviour
        }

    let listen<'appEvent> () : WorldFreezer<'appEvent> =
        listen'
            UnrecognisedEscapeCodeBehaviour.PassThrough
            Stopwatch.system
            (fun () -> Console.KeyAvailable)
            (fun () -> Console.ReadKey true)
