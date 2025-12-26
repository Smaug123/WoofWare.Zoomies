namespace WoofWare.Zoomies

open System

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

    /// Returns true if the node with the given key was activated within the
    /// visual feedback window (approximately 500ms).
    abstract WasRecentlyActivated : NodeKey -> bool

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
