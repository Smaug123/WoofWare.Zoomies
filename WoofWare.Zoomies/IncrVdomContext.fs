namespace WoofWare.Zoomies

open System
open System.Collections.Generic
open WoofWare.Incremental

/// VdomContext implementation backed by Incremental nodes.
/// Terminal bounds, focused key, and time are read from incremental sources.
/// Activation tracking and post-layout events remain mutable (they occur during render).
type IncrVdomContext<'postLayoutEvent> =
    internal
        {
            _TerminalBoundsVar : Rectangle Var
            _FocusedKeyVar : NodeKey option Var
            _Clock : Clock
            _Incr : Incremental
            mutable _IsDirty : bool
            _LastActivationTimes : Dictionary<NodeKey, DateTime>
            _PostLayoutEvents : ResizeArray<'postLayoutEvent>
            /// Function to get the current time (for testability).
            _GetUtcNow : unit -> DateTime
        }

    /// Get the current time.
    member private this.GetUtcNowInternal () : DateTime = this._GetUtcNow ()

    interface IVdomContext<'postLayoutEvent> with
        member this.TerminalBounds = this._Incr.Var.Value this._TerminalBoundsVar

        member this.FocusedKey = this._Incr.Var.Value this._FocusedKeyVar

        member this.GetUtcNow () = this.GetUtcNowInternal ()

        member this.WasRecentlyActivated key =
            match this._LastActivationTimes.TryGetValue key with
            | true, time ->
                let elapsed = (this.GetUtcNowInternal () - time).TotalMilliseconds
                elapsed < VdomContextConstants.RECENT_ACTIVATION_TIMEOUT_MS
            | false, _ -> false

        member this.PostLayoutEvent event =
            this._PostLayoutEvents.Add event
            this._IsDirty <- true

[<RequireQualifiedAccess>]
module IncrVdomContext =

    /// Create a new IncrVdomContext from an IncrementalState.
    /// The getUtcNow function is used for activation time tracking (visual feedback).
    let make'<'userState, 'postLayoutEvent>
        (getUtcNow : unit -> DateTime)
        (incrState : IncrementalState<'userState>)
        : IncrVdomContext<'postLayoutEvent>
        =
        {
            _TerminalBoundsVar = incrState.TerminalBoundsVar
            _FocusedKeyVar = incrState.FocusedKeyVar
            _Clock = incrState.Clock
            _Incr = incrState.Incr
            _IsDirty = true
            _LastActivationTimes = Dictionary<NodeKey, DateTime> ()
            _PostLayoutEvents = ResizeArray ()
            _GetUtcNow = getUtcNow
        }

    /// Create a new IncrVdomContext from an IncrementalState.
    /// Uses DateTime.UtcNow for activation time tracking.
    let make<'userState, 'postLayoutEvent>
        (incrState : IncrementalState<'userState>)
        : IncrVdomContext<'postLayoutEvent>
        =
        make'<'userState, 'postLayoutEvent> (fun () -> DateTime.UtcNow) incrState

    /// Get the terminal bounds.
    let terminalBounds<'postLayoutEvent> (ctx : IncrVdomContext<'postLayoutEvent>) : Rectangle =
        ctx._Incr.Var.Value ctx._TerminalBoundsVar

    /// Get the focused key.
    let focusedKey<'postLayoutEvent> (ctx : IncrVdomContext<'postLayoutEvent>) : NodeKey option =
        ctx._Incr.Var.Value ctx._FocusedKeyVar

    /// Set the terminal bounds.
    let internal setTerminalBounds<'postLayoutEvent>
        (bounds : Rectangle)
        (ctx : IncrVdomContext<'postLayoutEvent>)
        : unit
        =
        let current = ctx._Incr.Var.Value ctx._TerminalBoundsVar

        if current <> bounds then
            ctx._Incr.Var.Set ctx._TerminalBoundsVar bounds
            ctx._IsDirty <- true

    /// Set the focused key.
    let internal setFocusedKey<'postLayoutEvent>
        (key : NodeKey option)
        (ctx : IncrVdomContext<'postLayoutEvent>)
        : unit
        =
        let current = ctx._Incr.Var.Value ctx._FocusedKeyVar

        if current <> key then
            ctx._Incr.Var.Set ctx._FocusedKeyVar key
            ctx._IsDirty <- true

    /// Get the current UTC time.
    let getUtcNow<'postLayoutEvent> (ctx : IncrVdomContext<'postLayoutEvent>) : DateTime = ctx._GetUtcNow ()

    /// Record that a node was just activated.
    let internal recordActivation<'postLayoutEvent> (key : NodeKey) (ctx : IncrVdomContext<'postLayoutEvent>) : unit =
        ctx._LastActivationTimes.[key] <- getUtcNow ctx
        ctx._IsDirty <- true

    /// Clear activation state for a key.
    let internal clearActivation<'postLayoutEvent> (key : NodeKey) (ctx : IncrVdomContext<'postLayoutEvent>) : unit =
        if ctx._LastActivationTimes.Remove key then
            ctx._IsDirty <- true

    /// Remove any activation records that have expired.
    let internal pruneExpiredActivations<'postLayoutEvent> (ctx : IncrVdomContext<'postLayoutEvent>) : unit =
        let now = getUtcNow ctx
        let mutable removed = false

        for KeyValue (key, time) in ctx._LastActivationTimes do
            if
                (now - time).TotalMilliseconds
                >= VdomContextConstants.RECENT_ACTIVATION_TIMEOUT_MS
            then
                ctx._LastActivationTimes.Remove key |> ignore<bool>
                removed <- true

        if removed then
            ctx._IsDirty <- true

    /// Returns true if the node with the given key was activated within the visual feedback window.
    let wasRecentlyActivated<'postLayoutEvent> (key : NodeKey) (ctx : IncrVdomContext<'postLayoutEvent>) : bool =
        match ctx._LastActivationTimes.TryGetValue key with
        | true, time -> (getUtcNow ctx - time).TotalMilliseconds < VdomContextConstants.RECENT_ACTIVATION_TIMEOUT_MS
        | false, _ -> false

    /// Mark the context as dirty.
    let internal markDirty<'postLayoutEvent> (ctx : IncrVdomContext<'postLayoutEvent>) : unit = ctx._IsDirty <- true

    /// Mark the context as clean.
    let internal markClean<'postLayoutEvent> (ctx : IncrVdomContext<'postLayoutEvent>) : unit = ctx._IsDirty <- false

    /// Check if the context is dirty.
    let internal isDirty<'postLayoutEvent> (ctx : IncrVdomContext<'postLayoutEvent>) : bool = ctx._IsDirty

    /// Drain all post-layout events, returning them and clearing the internal list.
    let internal drainPostLayoutEvents<'postLayoutEvent>
        (ctx : IncrVdomContext<'postLayoutEvent>)
        : 'postLayoutEvent[]
        =
        if ctx._PostLayoutEvents.Count = 0 then
            Array.empty
        else
            let events = ctx._PostLayoutEvents.ToArray ()
            ctx._PostLayoutEvents.Clear ()
            events

    /// Get a typed IVdomContext<'postLayoutEvent> view of this context.
    let internal asTyped<'postLayoutEvent> (ctx : IncrVdomContext<'postLayoutEvent>) : IVdomContext<'postLayoutEvent> =
        ctx

    /// Get a base IVdomContext view of this context.
    let internal asBase<'postLayoutEvent> (ctx : IncrVdomContext<'postLayoutEvent>) : IVdomContext = ctx

    /// Get the underlying Incremental instance.
    let incr<'postLayoutEvent> (ctx : IncrVdomContext<'postLayoutEvent>) : Incremental = ctx._Incr

    /// Get the clock for time-based reactivity.
    let clock<'postLayoutEvent> (ctx : IncrVdomContext<'postLayoutEvent>) : Clock = ctx._Clock

    /// Get the terminal bounds as a Node for incremental computations.
    let boundsNode<'postLayoutEvent> (ctx : IncrVdomContext<'postLayoutEvent>) : Rectangle Node =
        ctx._Incr.Var.Watch ctx._TerminalBoundsVar

    /// Get the focused key as a Node for incremental computations.
    let focusedKeyNode<'postLayoutEvent> (ctx : IncrVdomContext<'postLayoutEvent>) : NodeKey option Node =
        ctx._Incr.Var.Watch ctx._FocusedKeyVar

    /// Get the clock time as an incremental Node (nanoseconds since epoch).
    let clockTimeNode<'postLayoutEvent>
        (ctx : IncrVdomContext<'postLayoutEvent>)
        : int64<WoofWare.TimingWheel.timeNs> Node
        =
        ctx._Incr.Clock.WatchNow ctx._Clock

    /// Get the clock time as a DateTime Node for convenience.
    let clockDateTimeNode<'postLayoutEvent> (ctx : IncrVdomContext<'postLayoutEvent>) : DateTime Node =
        ctx._Incr.Map TimeConversion.nsToDateTime (clockTimeNode ctx)
