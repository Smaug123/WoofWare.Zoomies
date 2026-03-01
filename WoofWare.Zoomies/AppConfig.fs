namespace WoofWare.Zoomies

open System.Runtime.ExceptionServices
open WoofWare.Incremental

/// How the framework should handle focus cycling (Tab/Shift+Tab)
[<RequireQualifiedAccess>]
type FocusHandling =
    /// Framework handles Tab/Shift+Tab internally.
    /// Tab cycles focus forward, Shift+Tab cycles backward.
    /// These keystrokes are consumed by the framework and not passed to user code.
    | FrameworkManaged
    /// User handles focus. Tab keystrokes are passed through as regular input
    /// (they are not consumed by the framework for focus cycling).
    /// The user is responsible for managing focus state entirely in their own
    /// application state; the framework's internal focus-tracking APIs are not
    /// exposed publicly in this mode.
    | UserManaged

/// Configuration for running an app with StateMachine-based event handling.
type AppConfig<'state, 'appEvent, 'postLayoutEvent> =
    {
        /// Initial application state.
        Initial : 'state

        /// Pure transition function: state -> event -> state.
        Transition : 'state -> 'appEvent -> 'state

        /// Build the Vdom from context and state node.
        View : VdomContext<'postLayoutEvent> -> 'state Node -> Vdom<DesiredBounds> Node

        /// Convert raw input to app events.
        /// Return None to ignore or let the framework handle (e.g. Tab for focus).
        HandleInput : WorldStateChange<'appEvent> -> 'appEvent option

        /// Process a post-layout event. Return the updated state.
        HandlePostLayout : 'postLayoutEvent -> 'state -> 'state

        /// How to handle focus cycling.
        FocusHandling : FocusHandling

        /// Activation resolver for focused-element keystrokes.
        ActivationResolver : ActivationResolver<'appEvent, 'state>

        /// Called once at startup with the world bridge.
        OnSetup : IWorldBridge<'appEvent> -> unit
    }

[<RequireQualifiedAccess>]
module AppConfig =

    /// Create a minimal config with framework-managed focus and no post-layout handling.
    let simple<'state, 'appEvent>
        (initial : 'state)
        (transition : 'state -> 'appEvent -> 'state)
        (view : VdomContext<unit> -> 'state Node -> Vdom<DesiredBounds> Node)
        (activationResolver : ActivationResolver<'appEvent, 'state>)
        : AppConfig<'state, 'appEvent, unit>
        =
        {
            Initial = initial
            Transition = transition
            View = view
            HandleInput =
                function
                | WorldStateChange.ApplicationEvent ev -> Some ev
                | WorldStateChange.ApplicationEventException exc ->
                    ExceptionDispatchInfo.Throw exc
                    failwith "unreachable"
                | _ -> None
            HandlePostLayout = fun _ state -> state
            FocusHandling = FocusHandling.FrameworkManaged
            ActivationResolver = activationResolver
            OnSetup = fun _ -> ()
        }

    /// Create a config with custom input handling but framework-managed focus.
    let withInputHandler<'state, 'appEvent, 'postLayoutEvent>
        (initial : 'state)
        (transition : 'state -> 'appEvent -> 'state)
        (view : VdomContext<'postLayoutEvent> -> 'state Node -> Vdom<DesiredBounds> Node)
        (handleInput : WorldStateChange<'appEvent> -> 'appEvent option)
        (handlePostLayout : 'postLayoutEvent -> 'state -> 'state)
        (activationResolver : ActivationResolver<'appEvent, 'state>)
        : AppConfig<'state, 'appEvent, 'postLayoutEvent>
        =
        {
            Initial = initial
            Transition = transition
            View = view
            HandleInput = handleInput
            HandlePostLayout = handlePostLayout
            FocusHandling = FocusHandling.FrameworkManaged
            ActivationResolver = activationResolver
            OnSetup = fun _ -> ()
        }

    /// Create a config with default HandleInput and HandlePostLayout.
    /// Use the withXxx functions to customize.
    let make<'state, 'appEvent, 'postLayoutEvent>
        (initial : 'state)
        (transition : 'state -> 'appEvent -> 'state)
        (view : VdomContext<'postLayoutEvent> -> 'state Node -> Vdom<DesiredBounds> Node)
        : AppConfig<'state, 'appEvent, 'postLayoutEvent>
        =
        {
            Initial = initial
            Transition = transition
            View = view
            HandleInput =
                function
                | WorldStateChange.ApplicationEvent ev -> Some ev
                | WorldStateChange.ApplicationEventException exc ->
                    ExceptionDispatchInfo.Throw exc
                    failwith "unreachable"
                | _ -> None
            HandlePostLayout = fun _ state -> state
            FocusHandling = FocusHandling.FrameworkManaged
            ActivationResolver = ActivationResolver.none
            OnSetup = fun _ -> ()
        }

    /// Replace the HandleInput function on a config.
    let withHandleInput<'state, 'appEvent, 'postLayoutEvent>
        (handleInput : WorldStateChange<'appEvent> -> 'appEvent option)
        (config : AppConfig<'state, 'appEvent, 'postLayoutEvent>)
        : AppConfig<'state, 'appEvent, 'postLayoutEvent>
        =
        { config with
            HandleInput = handleInput
        }

    /// Replace the FocusHandling on a config.
    let withFocusHandling<'state, 'appEvent, 'postLayoutEvent>
        (focusHandling : FocusHandling)
        (config : AppConfig<'state, 'appEvent, 'postLayoutEvent>)
        : AppConfig<'state, 'appEvent, 'postLayoutEvent>
        =
        { config with
            FocusHandling = focusHandling
        }

    /// Replace the HandlePostLayout function on a config.
    let withHandlePostLayout<'state, 'appEvent, 'postLayoutEvent>
        (handlePostLayout : 'postLayoutEvent -> 'state -> 'state)
        (config : AppConfig<'state, 'appEvent, 'postLayoutEvent>)
        : AppConfig<'state, 'appEvent, 'postLayoutEvent>
        =
        { config with
            HandlePostLayout = handlePostLayout
        }

    /// Replace the ActivationResolver on a config.
    let withActivationResolver<'state, 'appEvent, 'postLayoutEvent>
        (resolver : ActivationResolver<'appEvent, 'state>)
        (config : AppConfig<'state, 'appEvent, 'postLayoutEvent>)
        : AppConfig<'state, 'appEvent, 'postLayoutEvent>
        =
        { config with
            ActivationResolver = resolver
        }
