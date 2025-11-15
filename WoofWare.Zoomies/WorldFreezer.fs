namespace WoofWare.Zoomies

open System
open System.Collections.Concurrent
open System.Runtime.InteropServices
open System.Threading
open System.Threading.Tasks

/// The mouse is modelled as a device with one of five buttons, like "left" or "right".
type MouseButton =
    /// Left mouse button; usually the main one people use to interact with stuff.
    | Left
    /// The middle mouse button, usually a clicking motion of a scroll wheel. There's not really a universal convention
    /// for what this does.
    | Middle
    /// Right mouse button; usually used to summon some kind of contextual options.
    | Right
    /// For example, using macOS's "natural scrolling", the "flick your fingers downward" motion on a trackpad to
    /// move the content down the page (or, in old-school language, move the view portal up) results in a sequence
    /// of ScrollUp clicks.
    | ScrollUp
    /// For example, using macOS's "natural scrolling", the "flick your fingers upwards" motion on a trackpad to
    /// move the content up the page (or, in old-school language, move the view portal down) results in a sequence
    /// of ScrollDown clicks.
    | ScrollDown

    /// Human-readable string representation.
    override this.ToString () =
        match this with
        | MouseButton.Left -> "Left"
        | MouseButton.Middle -> "Middle"
        | MouseButton.Right -> "Right"
        | MouseButton.ScrollUp -> "ScrollUp"
        | MouseButton.ScrollDown -> "ScrollDown"

/// Represents the variety of modifier keyboard keys that may have been pressed, simultaneously or distinctly,
/// during a mouse event.
type MouseModifiers =
    /// No modifier keys were pressed; it was just the mouse click.
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

    /// Human-readable string representation of these coordinates: "(x, y)".
    override this.ToString () = $"(%i{this.X}, %i{this.Y})"

type MouseEvent =
    | Press of MouseButton * MouseModifiers * MouseCoordinates
    | Release of MouseButton * MouseModifiers * MouseCoordinates

    /// Human-readable string representation of this event. Unrelated e.g. to ANSI codes.
    override this.ToString () =
        match this with
        | MouseEvent.Press (button, modifiers, coords) -> $"Press %O{button} (%O{modifiers}) at %O{coords}"
        | MouseEvent.Release (button, modifiers, coords) -> $"Release %O{button} (%O{modifiers}) at %O{coords}"

type KeyboardEvent =
    | BeginBracketedPaste
    | EndBracketedPaste

type internal RawWorldStateChange<'appEvent> =
    | Keystroke of ConsoleKeyInfo
    | ApplicationEvent of 'appEvent
    | ApplicationEventException of exn

/// WoofWare.Zoomies presents to you a very narrow view of the world: a stream of WorldStateChange events, presented
/// in a linear order, buffered with each batch processed on-demand.
/// See `WorldFreezer` for the type that funnels the real world into a stream of these events.
type WorldStateChange<'appEvent> =
    /// Most interaction with a Zoomies TUI app is in the form of keystrokes. We pass you a nearly-unfiltered stream
    /// of the keystrokes the user supplies. Certain sequences of keystroke are ANSI escape codes, which we surface
    /// as e.g. the WorldStateChange.MouseEvent case instead.
    | Keystroke of ConsoleKeyInfo
    /// WoofWare.Zoomies automatically interprets certain ANSI escape codes as indicating keyboard events like
    /// "begin bracketed paste".
    | KeyboardEvent of KeyboardEvent
    /// WoofWare.Zoomies automatically interprets certain ANSI escape codes as indicating mouse events like "mouse
    /// down".
    | MouseEvent of MouseEvent
    /// When you use an IWorldBridge to insert events into the WoofWare.Zoomies-supplied event stream, they flow through
    /// as instances of ApplicationEvent.
    | ApplicationEvent of 'appEvent
    /// When you use an IWorldBridge's SubscribeEvent to insert a response to an external event into the
    /// WoofWare.Zoomies-supplied event stream, but the conversion function you supplied throws, the failure manifests
    /// in the event stream as an ApplicationEventException.
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

/// An IWorldBridge allows you to funnel arbitrary events into the WoofWare.Zoomies world model.
/// WoofWare.Zoomies supplies you with a long-lived one of these for each WorldFreezer.
type IWorldBridge<'appEvent> =
    /// Post an application event to the queue of events that forms the world.
    ///
    /// After this method returns, it's guaranteed that `WorldProcessor.ProcessWorld` will see the event "soon" (or has
    /// already seen it, if the render loop won the race with the `ret` instruction in `PostEvent`): the
    /// framework is allowed to break up batches of events, so you might not see it on the *next* render loop if there
    /// are other events ahead of this one in the queue, but the event is guaranteed to have been inserted into a
    /// defined place in the queue.
    abstract PostEvent : 'appEvent -> unit

    /// Subscribe to a synchronous event source. When the event fires, the converter
    /// function transforms the event data into an application event visible to the WorldFreezer's `Changes` queue.
    /// Returns an IDisposable to unsubscribe (though all subscriptions are also auto-cleaned
    /// when the associated WorldFreezer is disposed).
    ///
    /// If `toAppEvent` throws, you will receive an ApplicationEventException in the WoofWare.Zoomies event stream.
    abstract SubscribeEvent<'a> : IEvent<'a> -> toAppEvent : ('a -> 'appEvent) -> IDisposable

/// WoofWare.Zoomies presents to you a very narrow view of the world: a stream of WorldStateChange events, presented
/// in a linear order, buffered with each batch processed on-demand.
/// The `WorldFreezer` is what translates the real world into that buffered stream of linear events.
/// `WorldFreezer` is also an `IWorldBridge`, allowing the application programmer to insert events into the linear
/// stream for batched processing just like a keystroke would be; such events manifest as
/// `WorldStateChange.ApplicationEvent`s.
type WorldFreezer<'appEvent> =
    private
        {
            /// This is populated character-by-character of input keystroke.
            /// Note, for example, that this may contain partially-read sequences of ANSI control characters!
            _Changes : ConcurrentQueue<RawWorldStateChange<'appEvent>>
            /// Invariant: this is only mutated within `this.Changes()`.
            mutable _DequeueState : DequeueState
            _Stopwatch : IStopwatch
            _RefreshExternal : unit -> unit
            _Behaviour : UnrecognisedEscapeCodeBehaviour
            _Subscriptions : ConcurrentBag<IDisposable>
            _IsDisposing : int ref
            _HasDisposed : TaskCompletionSource<unit>
            _ActiveSubscriptionRequests : int ref
            /// Incremented when the terminal is resized. Checked by the render loop.
            _TerminalResizeGeneration : int ref
        }

    /// Load pending changes from the external world, like keystrokes, into the change list.
    member this.RefreshExternal () = this._RefreshExternal ()

    /// Increment the terminal resize generation. This is intended to be called by signal handlers (e.g., SIGWINCH).
    /// The render loop will detect the change and refresh terminal bounds.
    member internal this.NotifyTerminalResize () =
        Interlocked.Increment this._TerminalResizeGeneration |> ignore<int>

    /// Get the current terminal resize generation. Used by the render loop to detect resizes.
    member internal this.TerminalResizeGeneration = Volatile.Read &this._TerminalResizeGeneration.contents

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

    interface IWorldBridge<'appEvent> with
        member this.PostEvent evt =
            this._Changes.Enqueue (RawWorldStateChange.ApplicationEvent evt)

        member this.SubscribeEvent evt toAppEvent =
            if this._IsDisposing.Value > 0 then
                raise (ObjectDisposedException "WorldFreezer")
            else
                Interlocked.Increment this._ActiveSubscriptionRequests |> ignore<int>

                let handler =
                    Handler<'a> (fun _ args ->
                        let appEvent =
                            try
                                toAppEvent args |> Ok
                            with e ->
                                Error e

                        match appEvent with
                        | Ok appEvent -> this._Changes.Enqueue (RawWorldStateChange.ApplicationEvent appEvent)
                        | Error exc -> this._Changes.Enqueue (RawWorldStateChange.ApplicationEventException exc)
                    )

                evt.AddHandler handler

                let mutable disposed = 0

                let subscription =
                    { new IDisposable with
                        member _.Dispose () =
                            if Interlocked.CompareExchange (&disposed, 1, 0) = 0 then
                                evt.RemoveHandler handler
                    }

                this._Subscriptions.Add subscription

                Interlocked.Decrement this._ActiveSubscriptionRequests |> ignore<int>
                subscription

    interface IAsyncDisposable with
        member this.DisposeAsync () =
            if Interlocked.Increment this._IsDisposing = 1 then
                task {
                    // Wait for ActiveSubscriptionRequests to hit 0; then we know no more subscriptions are incoming
                    while this._ActiveSubscriptionRequests.Value > 0 do
                        // TODO: do this without sleeping
                        // #TimeIsNotASynchronizationPrimitive
                        do! Task.Delay (TimeSpan.FromMilliseconds 10.0)

                    for sub in this._Subscriptions do
                        try
                            sub.Dispose ()
                        with _ ->
                            ()

                    this._HasDisposed.SetResult ()
                }
                |> ValueTask
            else
                this._HasDisposed.Task |> ValueTask

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

        let refreshExternal () =
            while keyAvailable () do
                let key = readKey ()
                RawWorldStateChange.Keystroke key |> worldChanges.Enqueue

        {
            _Changes = worldChanges
            _RefreshExternal = refreshExternal
            _Stopwatch = stopwatch
            _DequeueState = DequeueState.Empty ()
            _Behaviour = behaviour
            _Subscriptions = ConcurrentBag ()
            _IsDisposing = ref 0
            _ActiveSubscriptionRequests = ref 0
            _HasDisposed = TaskCompletionSource<_> ()
            _TerminalResizeGeneration = ref 0
        }

    let listen<'appEvent> () : WorldFreezer<'appEvent> =
        listen'
            UnrecognisedEscapeCodeBehaviour.PassThrough
            Stopwatch.system
            (fun () -> Console.KeyAvailable)
            (fun () -> Console.ReadKey true)
