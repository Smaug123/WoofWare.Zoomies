namespace WoofWare.Zoomies

open System
open System.Collections.Generic

/// Context provided to vdom construction, containing information about the layout of the previous render cycle.
/// This is mutable (although you aren't given the tools to mutate it), so don't persist it.
type VdomContext<'postLayoutEvent> =
    internal
        {
            mutable _FocusedKey : NodeKey option
            mutable _TerminalBounds : Rectangle
            mutable _IsDirty : bool
            _LastActivationTimes : Dictionary<NodeKey, DateTime>
            _GetUtcNow : unit -> DateTime
            /// Events posted by components during rendering, to be processed after layout is complete.
            _PostLayoutEvents : ResizeArray<'postLayoutEvent>
        }

    interface IVdomContext<'postLayoutEvent> with
        member this.TerminalBounds = this._TerminalBounds
        member this.FocusedKey = this._FocusedKey
        member this.GetUtcNow () = this._GetUtcNow ()

        member this.WasRecentlyActivated key =
            match this._LastActivationTimes.TryGetValue key with
            | true, time ->
                (this._GetUtcNow () - time).TotalMilliseconds < VdomContextConstants.RECENT_ACTIVATION_TIMEOUT_MS
            | false, _ -> false

        member this.PostLayoutEvent event =
            this._PostLayoutEvents.Add event
            this._IsDirty <- true

[<RequireQualifiedAccess>]
module VdomContext =
    let internal empty<'postLayoutEvent>
        (getUtcNow : unit -> DateTime)
        (terminalBounds : Rectangle)
        : VdomContext<'postLayoutEvent>
        =
        {
            _TerminalBounds = terminalBounds
            _FocusedKey = None
            _IsDirty = true
            _LastActivationTimes = Dictionary<NodeKey, DateTime> ()
            _GetUtcNow = getUtcNow
            _PostLayoutEvents = ResizeArray ()
        }

    let internal setFocusedKey<'postLayoutEvent> (key : NodeKey option) (v : VdomContext<'postLayoutEvent>) =
        if v._FocusedKey <> key then
            v._IsDirty <- true
            v._FocusedKey <- key

    let internal setTerminalBounds<'postLayoutEvent> (tb : Rectangle) (v : VdomContext<'postLayoutEvent>) =
        if v._TerminalBounds <> tb then
            v._IsDirty <- true
            v._TerminalBounds <- tb

    let internal markClean<'postLayoutEvent> (v : VdomContext<'postLayoutEvent>) = v._IsDirty <- false

    let internal markDirty<'postLayoutEvent> (v : VdomContext<'postLayoutEvent>) = v._IsDirty <- true

    let internal isDirty<'postLayoutEvent> (v : VdomContext<'postLayoutEvent>) = v._IsDirty

    /// Get the dimensions of the terminal (on the previous render).
    let terminalBounds<'postLayoutEvent> (v : VdomContext<'postLayoutEvent>) : Rectangle = v._TerminalBounds

    /// Get the NodeKey of the Vdom element, if any, which was focused in the last render.
    /// If you're not using the automatic focus handling mechanism, this is always None.
    let focusedKey<'postLayoutEvent> (v : VdomContext<'postLayoutEvent>) : NodeKey option = v._FocusedKey

    /// Note that this time does *not* participate in dirtiness tracking. Hopefully we get Bonsai eventually so we can
    /// do that.
    let getUtcNow<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) = ctx._GetUtcNow ()

    /// Returns true if the node with the given key was activated within the
    /// visual feedback window (approximately 500ms).
    let wasRecentlyActivated<'postLayoutEvent> (key : NodeKey) (ctx : VdomContext<'postLayoutEvent>) : bool =
        match ctx._LastActivationTimes.TryGetValue key with
        | true, time -> (getUtcNow ctx - time).TotalMilliseconds < VdomContextConstants.RECENT_ACTIVATION_TIMEOUT_MS
        | false, _ -> false

    /// Record that a node was just activated.
    let internal recordActivation<'postLayoutEvent> (key : NodeKey) (ctx : VdomContext<'postLayoutEvent>) : unit =
        ctx._LastActivationTimes.[key] <- getUtcNow ctx
        ctx._IsDirty <- true

    /// Clear activation state for a key.
    let internal clearActivation<'postLayoutEvent> (key : NodeKey) (ctx : VdomContext<'postLayoutEvent>) : unit =
        if ctx._LastActivationTimes.Remove key then
            ctx._IsDirty <- true

    /// Remove any activation records that have expired, marking the context dirty if anything changes.
    let internal pruneExpiredActivations<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : unit =
        let now = getUtcNow ctx

        // The docs are very explicit.
        // https://learn.microsoft.com/en-us/dotnet/api/system.collections.generic.dictionary-2.getenumerator?view=net-6.0)
        // > .NET Core 3.0+ only: The only mutating methods which do not invalidate enumerators are Remove and Clear.
        // https://learn.microsoft.com/en-us/dotnet/api/system.collections.generic.dictionary-2.remove?view=net-6.0
        // > .NET Core 3.0+ only: this mutating method may be safely called without invalidating active enumerators on the Dictionary<TKey,TValue> instance. This does not imply thread safety.
        // We also explicitly test this safety property in TestVdomContext.fs.
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

    /// Drain all post-layout events, returning them and clearing the internal list.
    /// Returns an empty array if no events were posted.
    let internal drainPostLayoutEvents<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : 'postLayoutEvent[] =
        if ctx._PostLayoutEvents.Count = 0 then
            Array.empty
        else
            let events = ctx._PostLayoutEvents.ToArray ()
            ctx._PostLayoutEvents.Clear ()
            events

    /// Get a typed IVdomContext<'postLayoutEvent> view of this VdomContext.
    let internal asTyped<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : IVdomContext<'postLayoutEvent> = ctx

    /// Get a base IVdomContext view of this VdomContext.
    let internal asBase<'postLayoutEvent> (ctx : VdomContext<'postLayoutEvent>) : IVdomContext = ctx
