namespace WoofWare.Zoomies

open System
open System.Collections.Generic
open WoofWare.Incremental

/// VdomContext implementation backed by Incremental nodes.
type VdomContext<'postLayoutEvent> =
    internal
        {
            _TerminalBoundsVar : Rectangle Var
            _FocusedKeyVar : NodeKey option Var
            _Clock : Clock
            _Incr : Incremental
            _IncrView : IncrView
            /// Cached clock DateTime node.
            _ClockDateTimeNode : DateTime Node
            mutable _IsDirty : bool
            _LastActivationTimes : Dictionary<NodeKey, DateTime>
            _PostLayoutEvents : ResizeArray<'postLayoutEvent>
            /// Incremented whenever activation state changes, so incremental nodes can depend on it.
            _ActivationGenerationVar : int Var
        }

    interface IVdomContext<'postLayoutEvent> with
        member this.TerminalBounds = this._Incr.Var.Value this._TerminalBoundsVar

        member this.FocusedKey = this._Incr.Var.Watch this._FocusedKeyVar

        member this.Incr = this._IncrView

        member this.UnsafeIncr = this._Incr

        member this.Builder = IncrementalBuilder.create this._Incr

        member this.WasRecentlyActivated key =
            let activationGenNode = this._Incr.Var.Watch this._ActivationGenerationVar

            this._Incr.Map2
                (fun _gen (now : DateTime) ->
                    match this._LastActivationTimes.TryGetValue key with
                    | true, time ->
                        let elapsed = (now - time).TotalMilliseconds
                        elapsed < VdomContextConstants.RECENT_ACTIVATION_TIMEOUT_MS
                    | false, _ -> false
                )
                activationGenNode
                this._ClockDateTimeNode

        member this.PostLayoutEvent event =
            this._PostLayoutEvents.Add event
            this._IsDirty <- true

[<RequireQualifiedAccess>]
module VdomContext =

    /// Create a new VdomContext from an IncrementalState.
    let make<'postLayoutEvent> (incrState : IncrementalState) : VdomContext<'postLayoutEvent> =
        {
            _TerminalBoundsVar = incrState.TerminalBoundsVar
            _FocusedKeyVar = incrState.FocusedKeyVar
            _Clock = incrState.Clock
            _Incr = incrState.Incr
            _IncrView = IncrView incrState.Incr
            _ClockDateTimeNode = incrState.ClockDateTimeNode
            _IsDirty = true
            _LastActivationTimes = Dictionary<NodeKey, DateTime> ()
            _PostLayoutEvents = ResizeArray ()
            _ActivationGenerationVar = incrState.Incr.Var.Create 0
        }

    /// Get the terminal bounds.
    let terminalBounds<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : Rectangle =
        ctx._Incr.Var.Value ctx._TerminalBoundsVar

    /// Get the focused key.
    let focusedKey<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : NodeKey option =
        ctx._Incr.Var.Value ctx._FocusedKeyVar

    /// Set the terminal bounds.
    let internal setTerminalBounds<'postLayoutEvent> (bounds : Rectangle) (ctx : VdomContext<'postLayoutEvent>) : unit =
        let current = ctx._Incr.Var.Value ctx._TerminalBoundsVar

        if current <> bounds then
            ctx._Incr.Var.Set ctx._TerminalBoundsVar bounds
            ctx._IsDirty <- true

    /// Set the focused key.
    let internal setFocusedKey<'postLayoutEvent> (key : NodeKey option) (ctx : VdomContext<'postLayoutEvent>) : unit =
        let current = ctx._Incr.Var.Value ctx._FocusedKeyVar

        if current <> key then
            ctx._Incr.Var.Set ctx._FocusedKeyVar key
            ctx._IsDirty <- true

    /// Record that a node was just activated.
    let internal recordActivation<'postLayoutEvent>
        (now : DateTime)
        (key : NodeKey)
        (ctx : VdomContext<'postLayoutEvent>)
        : unit
        =
        ctx._LastActivationTimes.[key] <- now
        let gen = ctx._Incr.Var.Value ctx._ActivationGenerationVar
        ctx._Incr.Var.Set ctx._ActivationGenerationVar (gen + 1)
        ctx._IsDirty <- true

    /// Clear activation state for a key.
    let internal clearActivation<'postLayoutEvent> (key : NodeKey) (ctx : VdomContext<'postLayoutEvent>) : unit =
        if ctx._LastActivationTimes.Remove key then
            let gen = ctx._Incr.Var.Value ctx._ActivationGenerationVar
            ctx._Incr.Var.Set ctx._ActivationGenerationVar (gen + 1)
            ctx._IsDirty <- true

    /// Remove expired activation records.
    let internal pruneExpiredActivations<'postLayoutEvent>
        (now : DateTime)
        (ctx : VdomContext<'postLayoutEvent>)
        : unit
        =
        // .NET Core 3.0+: Remove does not invalidate enumerators. Tested in TestVdomContext.fs.
        let mutable removed = false

        for KeyValue (key, time) in ctx._LastActivationTimes do
            if
                (now - time).TotalMilliseconds
                >= VdomContextConstants.RECENT_ACTIVATION_TIMEOUT_MS
            then
                ctx._LastActivationTimes.Remove key |> ignore<bool>
                removed <- true

        if removed then
            let gen = ctx._Incr.Var.Value ctx._ActivationGenerationVar
            ctx._Incr.Var.Set ctx._ActivationGenerationVar (gen + 1)
            ctx._IsDirty <- true

    /// Returns a Node that is true if the given key was activated within the visual feedback window.
    let wasRecentlyActivated<'postLayoutEvent> (key : NodeKey) (ctx : VdomContext<'postLayoutEvent>) : bool Node =
        let activationGenNode = ctx._Incr.Var.Watch ctx._ActivationGenerationVar

        ctx._Incr.Map2
            (fun _gen (now : DateTime) ->
                match ctx._LastActivationTimes.TryGetValue key with
                | true, time ->
                    let elapsed = (now - time).TotalMilliseconds
                    elapsed < VdomContextConstants.RECENT_ACTIVATION_TIMEOUT_MS
                | false, _ -> false
            )
            activationGenNode
            ctx._ClockDateTimeNode

    /// Mark the context as dirty.
    let internal markDirty<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : unit = ctx._IsDirty <- true

    /// Mark the context as clean.
    let internal markClean<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : unit = ctx._IsDirty <- false

    /// Check if the context is dirty.
    let internal isDirty<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : bool = ctx._IsDirty

    /// Drain all post-layout events, returning them and clearing the internal list.
    let internal drainPostLayoutEvents<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : 'postLayoutEvent[] =
        if ctx._PostLayoutEvents.Count = 0 then
            Array.empty
        else
            let events = ctx._PostLayoutEvents.ToArray ()
            ctx._PostLayoutEvents.Clear ()
            events

    /// Get a typed IVdomContext<'postLayoutEvent> view of this context.
    let internal asTyped<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : IVdomContext<'postLayoutEvent> = ctx

    /// Get a base IVdomContext view of this context.
    let internal asBase<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : IVdomContext = ctx

    /// Get the safe Incremental view for building nodes.
    let incr<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : IncrView = ctx._IncrView

    /// A computation expression for building incremental nodes using `VdomContext.incr`.
    let incrBuilder<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : IncrementalBuilder =
        IncrementalBuilder.create ctx._Incr

    /// Get the underlying Incremental instance. This is unsafe inside view functions.
    let unsafeIncr<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : Incremental = ctx._Incr

    /// Get the clock for time-based reactivity.
    let clock<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : Clock = ctx._Clock

    /// Get the terminal bounds as a Node for incremental computations.
    let boundsNode<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : Rectangle Node =
        ctx._Incr.Var.Watch ctx._TerminalBoundsVar

    /// Get the focused key as a Node for incremental computations.
    let focusedKeyNode<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : NodeKey option Node =
        ctx._Incr.Var.Watch ctx._FocusedKeyVar

    /// Get the clock time as an incremental Node (nanoseconds since epoch).
    let clockTimeNode<'postLayoutEvent>
        (ctx : VdomContext<'postLayoutEvent>)
        : int64<WoofWare.TimingWheel.timeNs> Node
        =
        ctx._Incr.Clock.WatchNow ctx._Clock

    /// Get the clock time as a DateTime Node.
    let clockDateTimeNode<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : DateTime Node =
        ctx._ClockDateTimeNode
