namespace WoofWare.Zoomies.Test

open System.Collections.Concurrent
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
        KeyAvailable : unit -> bool
        ReadKey : unit -> System.ConsoleKeyInfo
        SendKey : System.ConsoleKeyInfo -> unit
    }

[<RequireQualifiedAccess>]
module MockWorld =

    let make () : MockWorld =
        let queue = ConcurrentQueue ()

        let isReady () = queue.Count > 0

        let rec getLatest () =
            match queue.TryDequeue () with
            | false, _ -> getLatest ()
            | true, v -> v

        let send k = queue.Enqueue k

        {
            KeyAvailable = isReady
            ReadKey = getLatest
            SendKey = send
        }
