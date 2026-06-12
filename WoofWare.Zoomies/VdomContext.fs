namespace WoofWare.Zoomies

open System
open WoofWare.Incremental
open WoofWare.TimingWheel

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
            _PostLayoutEvents : ResizeArray<'postLayoutEvent>
            /// Last activation time per key, newest first. An association list rather than a Map
            /// because NodeKey has no comparison; it stays tiny because recordActivation prunes
            /// entries that have already expired.
            _ActivationsVar : (NodeKey * TimeNs) list Var
        }

    interface IVdomContext<'postLayoutEvent> with
        member this.TerminalBounds = this._Incr.Var.Value this._TerminalBoundsVar

        member this.FocusedKey = this._Incr.Var.Watch this._FocusedKeyVar

        member this.Incr = this._IncrView

        member this.UnsafeIncr = this._Incr

        member this.Builder = IncrementalBuilder.create this._Incr

        member this.WasRecentlyActivated key =
            let activatedAtNode =
                this._Incr.Var.Watch this._ActivationsVar
                |> this._Incr.Map (fun entries ->
                    entries |> List.tryPick (fun (k, t) -> if k = key then Some t else None)
                )

            // Bind so that each activation gets its own clock alarm: the node flips to false
            // exactly when the activation window closes, and the alarm is what tells the event
            // loop to wake up and repaint.
            activatedAtNode
            |> this._Incr.Bind (fun activatedAt ->
                match activatedAt with
                | None -> this._Incr.Return false
                | Some activatedAt ->
                    this._Incr.Clock.At
                        this._Clock
                        (TimeNs.add activatedAt VdomContextConstants.recentActivationTimeout)
                    |> this._Incr.Map (fun ba -> ba = BeforeOrAfter.Before)
            )

        member this.PostLayoutEvent event = this._PostLayoutEvents.Add event

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
            _PostLayoutEvents = ResizeArray ()
            _ActivationsVar = incrState.Incr.Var.Create []
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

    /// Set the focused key.
    let internal setFocusedKey<'postLayoutEvent> (key : NodeKey option) (ctx : VdomContext<'postLayoutEvent>) : unit =
        let current = ctx._Incr.Var.Value ctx._FocusedKeyVar

        if current <> key then
            ctx._Incr.Var.Set ctx._FocusedKeyVar key

    /// Record that a node was just activated.
    let internal recordActivation<'postLayoutEvent>
        (now : DateTime)
        (key : NodeKey)
        (ctx : VdomContext<'postLayoutEvent>)
        : unit
        =
        let nowNs = TimeConversion.dateTimeToNs now

        let live =
            ctx._Incr.Var.Value ctx._ActivationsVar
            // Drop the key being re-recorded, and prune entries whose window has already
            // closed: their alarms have fired, so nothing depends on them any more.
            |> List.filter (fun (k, t) -> k <> key && TimeNs.add t VdomContextConstants.recentActivationTimeout > nowNs)

        ctx._Incr.Var.Set ctx._ActivationsVar ((key, nowNs) :: live)

    /// Clear activation state for a key.
    let internal clearActivation<'postLayoutEvent> (key : NodeKey) (ctx : VdomContext<'postLayoutEvent>) : unit =
        let current = ctx._Incr.Var.Value ctx._ActivationsVar
        let remaining = current |> List.filter (fun (k, _) -> k <> key)

        if remaining.Length <> current.Length then
            ctx._Incr.Var.Set ctx._ActivationsVar remaining

    /// Returns a Node that is true if the given key was activated within the visual feedback window.
    /// The node owns a clock alarm for the end of the window, so expiry both flips the node and
    /// wakes the event loop.
    let wasRecentlyActivated<'postLayoutEvent> (key : NodeKey) (ctx : VdomContext<'postLayoutEvent>) : bool Node =
        (ctx :> IVdomContext).WasRecentlyActivated key

    /// True if post-layout events have been posted and not yet drained. The render loop
    /// must keep rendering (not sleep) while any are pending.
    let internal hasPendingPostLayoutEvents<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : bool =
        ctx._PostLayoutEvents.Count > 0

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
