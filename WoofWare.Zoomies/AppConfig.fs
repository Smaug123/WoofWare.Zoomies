namespace WoofWare.Zoomies

open WoofWare.Incremental

/// How the framework should handle focus cycling (Tab/Shift+Tab)
[<RequireQualifiedAccess>]
type FocusHandling =
    /// Framework handles Tab/Shift+Tab internally.
    /// Tab cycles focus forward, Shift+Tab cycles backward.
    /// These keystrokes are consumed by the framework and not passed to user code.
    | FrameworkManaged
    /// User handles focus. Tab keystrokes are passed through as regular input.
    /// The user is responsible for managing focus state and calling the appropriate
    /// focus management functions.
    | UserManaged

/// Configuration for running an app with the new StateMachine-based event handling.
///
/// This replaces the WorldProcessor interface with a more declarative approach:
/// - State transitions are expressed as a pure function
/// - Events flow through the Incremental graph
/// - Focus handling is configurable
type AppConfig<'state, 'appEvent, 'postLayoutEvent> =
    {
        /// Initial application state.
        Initial : 'state

        /// Pure transition function: state -> event -> state.
        /// Called for each app event to produce the new state.
        /// Events are batched between stabilizations, so multiple events may be folded in sequence.
        Transition : 'state -> 'appEvent -> 'state

        /// Build the Vdom from context and state node.
        /// This is called when the Incremental graph stabilizes and the state has changed.
        View : VdomContext<'postLayoutEvent> -> 'state Node -> Vdom<DesiredBounds> Node

        /// Convert raw input to app events.
        /// Return Some to inject the event into the state machine.
        /// Return None to ignore the input (or let the framework handle it, e.g., Tab for focus).
        HandleInput : WorldStateChange<'appEvent> -> 'appEvent option

        /// Process post-layout events (generated during render).
        /// These events are processed synchronously after each render.
        /// Return the new state; if it differs from the current state, a re-render will occur.
        HandlePostLayout : 'postLayoutEvent -> 'state -> 'state

        /// How to handle focus cycling.
        /// FrameworkManaged: Tab/Shift+Tab handled automatically.
        /// UserManaged: Tab passed through as input, user manages focus.
        FocusHandling : FocusHandling

        /// Activation resolver for focused-element keystrokes.
        /// When a focusable element is active and receives a keystroke,
        /// this resolver determines if it should be converted to an app event.
        ActivationResolver : ActivationResolver<'appEvent, 'state>

        /// Called once at startup with the world bridge.
        /// Use this to capture the bridge for posting async events.
        /// For example, store it in a ref cell or pass it to async operations.
        OnSetup : IWorldBridge<'appEvent> -> unit
    }

[<RequireQualifiedAccess>]
module AppConfig =

    /// Create a minimal config with framework-managed focus and no post-layout handling.
    ///
    /// This is suitable for simple apps that:
    /// - Want automatic Tab focus cycling
    /// - Don't need post-layout events
    /// - Only care about ApplicationEvent from WorldStateChange
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
