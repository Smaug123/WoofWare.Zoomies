namespace WoofWare.Zoomies

open System
open System.Collections.Generic

/// Context provided to vdom construction, containing information about the layout of the previous render cycle.
/// This is mutable (although you aren't given the tools to mutate it), so don't persist it.
type VdomContext =
    private
        {
            mutable _FocusedKey : NodeKey option
            mutable _TerminalBounds : Rectangle
            mutable IsDirty : bool
            _LastActivationTimes : Dictionary<NodeKey, DateTime>
            GetUtcNow : unit -> DateTime
        }

[<RequireQualifiedAccess>]
module VdomContext =
    let internal empty (terminalBounds : Rectangle) =
        {
            _TerminalBounds = terminalBounds
            _FocusedKey = None
            IsDirty = true
            _LastActivationTimes = Dictionary<NodeKey, DateTime> ()
            GetUtcNow = (fun () -> DateTime.UtcNow)
        }

    let internal setFocusedKey (key : NodeKey option) (v : VdomContext) =
        if v._FocusedKey <> key then
            v.IsDirty <- true
            v._FocusedKey <- key

    let internal setTerminalBounds (tb : Rectangle) (v : VdomContext) =
        if v._TerminalBounds <> tb then
            v.IsDirty <- true
            v._TerminalBounds <- tb

    let internal markClean (v : VdomContext) = v.IsDirty <- false

    let internal markDirty (v : VdomContext) = v.IsDirty <- true

    /// Get the dimensions of the terminal (on the previous render).
    let terminalBounds (v : VdomContext) : Rectangle = v._TerminalBounds
    /// Get the NodeKey of the Vdom element, if any, which was focused in the last render.
    /// If you're not using the automatic focus handling mechanism, this is always None.
    let focusedKey (v : VdomContext) : NodeKey option = v._FocusedKey

    /// Returns true if the node with the given key was activated within the
    /// visual feedback window (approximately 500ms).
    let wasRecentlyActivated (key : NodeKey) (ctx : VdomContext) : bool =
        match ctx._LastActivationTimes.TryGetValue key with
        | true, time -> (ctx.GetUtcNow () - time).TotalMilliseconds < 500.0
        | false, _ -> false

    /// Record that a node was just activated. Internal use only.
    let internal recordActivation (key : NodeKey) (ctx : VdomContext) : unit =
        ctx._LastActivationTimes.[key] <- ctx.GetUtcNow ()
        ctx.IsDirty <- true

    /// Clear activation state for a key. Internal use only.
    let internal clearActivation (key : NodeKey) (ctx : VdomContext) : unit =
        if ctx._LastActivationTimes.Remove key then
            ctx.IsDirty <- true
