namespace WoofWare.Zoomies.Test

open WoofWare.Incremental
open WoofWare.Zoomies

/// Helpers for creating AppConfig for tests.
[<RequireQualifiedAccess>]
module TestConfig =

    /// Create a passthrough AppConfig with unit state that ignores all inputs.
    /// Use for tests that just need to render and don't care about state changes.
    let passthrough<'postLayoutEvent>
        (view : IVdomContext<'postLayoutEvent> -> unit -> Vdom<DesiredBounds>)
        : AppConfig<unit, unit, 'postLayoutEvent>
        =
        AppConfig.make () (fun _s _ev -> ()) (App.pureView view)
        |> AppConfig.withFocusHandling FocusHandling.FrameworkManaged

    /// Create a passthrough AppConfig with unit state that ignores all inputs, for incremental views.
    /// Use for tests that just need to render and don't care about state changes.
    let passthroughIncr<'postLayoutEvent>
        (view : IVdomContext<'postLayoutEvent> -> unit -> Vdom<DesiredBounds> Node)
        : AppConfig<unit, unit, 'postLayoutEvent>
        =
        AppConfig.make () (fun _s _ev -> ()) (App.pureViewIncr view)
        |> AppConfig.withFocusHandling FocusHandling.FrameworkManaged

    /// Create an AppConfig with custom state handling using a pure view.
    /// The handleInput function converts WorldStateChange to optional app events.
    let withState<'state, 'appEvent, 'postLayoutEvent when 'state : equality>
        (initial : 'state)
        (transition : 'state -> 'appEvent -> 'state)
        (handleInput : WorldStateChange<'appEvent> -> 'appEvent option)
        (view : IVdomContext<'postLayoutEvent> -> 'state -> Vdom<DesiredBounds>)
        : AppConfig<'state, 'appEvent, 'postLayoutEvent>
        =
        AppConfig.make initial transition (App.pureView view)
        |> AppConfig.withHandleInput handleInput
        |> AppConfig.withFocusHandling FocusHandling.FrameworkManaged

type MockWorld =
    {
        /// Deliver a key into the attached WorldFreezer, as the platform input thread would.
        /// Synchronous: when this returns, the key is visible to the freezer's Changes.
        SendKey : System.ConsoleKeyInfo -> unit
    }

[<RequireQualifiedAccess>]
module MockWorld =

    /// Create a MockWorld that delivers keystrokes synchronously into the given freezer.
    let attach (freezer : WorldFreezer<'appEvent>) : MockWorld =
        {
            SendKey = freezer.DeliverKeystroke
        }
