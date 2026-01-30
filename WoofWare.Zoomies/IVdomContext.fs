namespace WoofWare.Zoomies

open System
open WoofWare.Incremental

[<Sealed>]
/// Restricted Incremental API for use in view functions.
/// This exposes only safe combinators that build nodes without forcing stabilization.
type IncrView internal (incr : Incremental) =
    member _.Map (f : 'a -> 'b) (node : 'a Node) : 'b Node = incr.Map f node
    member _.Map2 (f : 'a -> 'b -> 'c) (node1 : 'a Node) (node2 : 'b Node) : 'c Node = incr.Map2 f node1 node2
    member _.Bind (f : 'a -> 'b Node) (node : 'a Node) : 'b Node = incr.Bind f node
    member _.Both (node1 : 'a Node) (node2 : 'b Node) : ('a * 'b) Node = incr.Both node1 node2
    member _.Return (value : 'a) : 'a Node = incr.Return value

[<RequireQualifiedAccess>]
module VdomContextConstants =
    /// Number of milliseconds you get after activation of an activatable component like Button, before which the
    /// framework considers `VdomContext.wasRecentlyActivated` to expire.
    [<Literal>]
    let RECENT_ACTIVATION_TIMEOUT_MS = 500.0

/// Base interface for VdomContext - used by components that don't need to post layout events.
/// This provides read-only access to context information like terminal bounds and focus state.
type IVdomContext =
    /// Get the dimensions of the terminal (on the previous render).
    abstract TerminalBounds : Rectangle

    /// Get the NodeKey of the Vdom element, if any, which was focused in the last render.
    /// If you're not using the automatic focus handling mechanism, this is always None.
    abstract FocusedKey : NodeKey option

    /// Get the focused key as an incremental Node for reactive computations.
    /// Components should use this instead of FocusedKey when building incremental views
    /// so that focus changes trigger re-computation.
    abstract FocusedKeyNode : NodeKey option Node

    /// Get the safe Incremental view for building incremental computations.
    abstract Incr : IncrView

    /// Get the underlying Incremental instance. This is unsafe inside view functions.
    abstract UnsafeIncr : Incremental

    /// Returns a Node that is true if the node with the given key was activated within the
    /// visual feedback window (approximately 500ms). The Node depends on the clock, so it
    /// will automatically update as time passes.
    abstract WasRecentlyActivated : NodeKey -> bool Node

/// Extended interface for components that need to post layout events.
/// Layout events are processed after the render is complete, allowing components
/// to communicate state changes based on layout information (e.g., viewport-aware scrolling).
///
/// Since this interface inherits from IVdomContext, a typed context can be passed
/// anywhere an untyped IVdomContext is expected.
type IVdomContext<'postLayoutEvent> =
    inherit IVdomContext

    /// Post an event to be processed after layout is complete.
    /// This is useful for components that need to communicate state changes based on layout information
    /// (e.g., viewport-aware scrolling). The event will be fed to ProcessPostLayoutEvents after the current render.
    abstract PostLayoutEvent : 'postLayoutEvent -> unit
