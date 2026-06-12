namespace WoofWare.Zoomies.Test

open System
open WoofWare.Incremental
open WoofWare.Zoomies

/// Test harness wrapping IncrementalState with time control.
type MockTimer =
    {
        /// The underlying IncrementalState.
        IncrState : IncrementalState
        /// Advance time by the given amount and stabilize.
        Advance : TimeSpan -> DateTime
        /// Set the time to a specific DateTime and stabilize.
        SetTimeUtc : DateTime -> unit
        /// Get the current time.
        CurrentTime : unit -> DateTime
    }

[<RequireQualifiedAccess>]
module MockTime =
    let defaultStartTime =
        // Eddie Hall's 500kg deadlift.
        // (https://www.youtube.com/watch?v=_DX2L4Pp8S0 took place on 2016-07-09 in Leeds, UK.
        // Eddie Hall hits the lift at timestamp 1:12:45. There are various watches displayed throughout the stream;
        // the clearest is the 19:15 local time displayed at timestamp 0:48:03.)
        DateTime (2016, 07, 09, 18, 39, 00, DateTimeKind.Utc)

    /// Create a MockTimer with the given initial time and bounds.
    let makeWithTime (startTime : DateTime) (bounds : Rectangle) : MockTimer =
        let incrState = IncrementalState.make bounds None
        // Advance the clock to the specified start time
        IncrementalState.advanceClockAndStabilize startTime incrState
        let mutable currentTime = startTime

        {
            IncrState = incrState
            Advance =
                fun ts ->
                    currentTime <- currentTime + ts
                    IncrementalState.advanceClockAndStabilize currentTime incrState
                    currentTime
            SetTimeUtc =
                fun dt ->
                    currentTime <- dt
                    IncrementalState.advanceClockAndStabilize currentTime incrState
            CurrentTime = fun () -> currentTime
        }

    /// Create a MockTimer with default start time and bounds.
    let make () =
        makeWithTime
            defaultStartTime
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = 80
                Height = 24
            }

    /// Create a MockTimer from console dimensions with the given start time.
    let makeFromConsole' (startTime : DateTime) (console : IConsole) : MockTimer =
        makeWithTime
            startTime
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = console.WindowWidth ()
                Height = console.WindowHeight ()
            }

    /// Create a MockTimer from console dimensions with default start time.
    let makeFromConsole (console : IConsole) : MockTimer =
        makeFromConsole' defaultStartTime console

    /// Create a VdomContext for testing purposes with the given bounds.
    /// Uses a static mock time.
    let makeVdomContext<'postLayoutEvent> (bounds : Rectangle) : VdomContext<'postLayoutEvent> =
        let incrState = IncrementalState.make bounds None
        IncrementalState.advanceClockAndStabilize defaultStartTime incrState
        VdomContext.make incrState

    /// Create a VdomContext with default 80x24 terminal bounds for testing.
    let makeDefaultVdomContext<'postLayoutEvent> () : VdomContext<'postLayoutEvent> =
        makeVdomContext
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = 80
                Height = 24
            }

    /// Create a VdomContext from an IConsole for testing.
    /// Uses a static mock time.
    let makeVdomContextFromConsole<'postLayoutEvent> (console : IConsole) : VdomContext<'postLayoutEvent> =
        makeVdomContext
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = console.WindowWidth ()
                Height = console.WindowHeight ()
            }

    /// Create a RenderState for testing from a MockTimer, with a function to advance time.
    let makeRenderStateFromTimer<'postLayoutEvent>
        (console : IConsole)
        (timer : MockTimer)
        (debugWriter : System.IO.StreamWriter option)
        : RenderState<'postLayoutEvent> * (TimeSpan -> DateTime)
        =
        let vdomContext = VdomContext.make<'postLayoutEvent> timer.IncrState
        let renderState = RenderState.make console vdomContext debugWriter

        let advanceWithContext ts = timer.Advance ts

        renderState, advanceWithContext

    /// Create a RenderState for testing with a static mock time.
    let makeRenderStateStatic<'postLayoutEvent>
        (console : IConsole)
        (debugWriter : System.IO.StreamWriter option)
        : RenderState<'postLayoutEvent>
        =
        let vdomContext = makeVdomContextFromConsole<'postLayoutEvent> console
        RenderState.make console vdomContext debugWriter

    /// Create a VdomContext from a MockTimer, with a function to advance time.
    let makeVdomContextFromTimer<'postLayoutEvent>
        (timer : MockTimer)
        : VdomContext<'postLayoutEvent> * (TimeSpan -> DateTime)
        =
        let ctx = VdomContext.make<'postLayoutEvent> timer.IncrState

        let advanceWithContext ts = timer.Advance ts

        ctx, advanceWithContext

/// Test infrastructure for running incremental pump cycles.
type IncrTestContext<'state, 'appEvent, 'postLayoutEvent> =
    {
        /// The incremental state holder (clock, bounds, focus).
        IncrState : IncrementalState
        /// The variable holding the user state.
        StateVar : 'state Var
        /// The render state for terminal output.
        RenderState : RenderState<'postLayoutEvent>
        /// Observer for the Vdom node.
        VdomObserver : Vdom<DesiredBounds> Observer
        /// Tracks the previous Vdom for detecting time-based changes.
        PreviousVdom : Vdom<DesiredBounds> ref
        /// Mutable time tracker for getUtcNow.
        mutable CurrentTime : DateTime
    }

    interface IDisposable with
        member this.Dispose () =
            (this.RenderState :> IDisposable).Dispose ()

[<RequireQualifiedAccess>]
module IncrTestContext =
    /// Create test context from an AppConfig and console.
    let make<'state, 'appEvent, 'postLayoutEvent when 'state : equality>
        (console : IConsole)
        (config : AppConfig<'state, 'appEvent, 'postLayoutEvent>)
        (debugWriter : System.IO.StreamWriter option)
        : IncrTestContext<'state, 'appEvent, 'postLayoutEvent>
        =
        let initialBounds =
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = console.WindowWidth ()
                Height = console.WindowHeight ()
            }

        let incrState = IncrementalState.make initialBounds None
        let vdomContext = VdomContext.make incrState

        let stateVar = incrState.Incr.Var.Create config.Initial

        let vdomNode = config.View vdomContext (incrState.Incr.Var.Watch stateVar)
        let vdomObserver = incrState.Incr.Observe vdomNode
        let startTime = MockTime.defaultStartTime

        let renderState = RenderState.make console vdomContext debugWriter

        {
            IncrState = incrState
            StateVar = stateVar
            RenderState = renderState
            VdomObserver = vdomObserver
            PreviousVdom = ref Vdom.empty
            CurrentTime = startTime
        }

    /// Get the current state from the state variable.
    let currentState (ctx : IncrTestContext<'state, 'appEvent, 'postLayoutEvent>) : 'state =
        ctx.IncrState.Incr.Var.Value ctx.StateVar

    /// Advance time by the given amount and return a getUtcNow function.
    let advanceTime (ts : TimeSpan) (ctx : IncrTestContext<'state, 'appEvent, 'postLayoutEvent>) : unit =
        ctx.CurrentTime <- ctx.CurrentTime + ts

    /// Get a getUtcNow function that returns the context's current time.
    let getUtcNow (ctx : IncrTestContext<'state, 'appEvent, 'postLayoutEvent>) : unit -> DateTime =
        fun () -> ctx.CurrentTime

    /// Run one pump cycle using the incremental pipeline.
    /// This calls App.pumpOnce with the context's infrastructure.
    let pumpOnce<'state, 'appEvent, 'postLayoutEvent when 'state : equality>
        (listener : WorldFreezer<'appEvent>)
        (config : AppConfig<'state, 'appEvent, 'postLayoutEvent>)
        (ctx : IncrTestContext<'state, 'appEvent, 'postLayoutEvent>)
        : 'state
        =
        App.pumpOnce
            (getUtcNow ctx)
            listener
            ctx.IncrState
            ctx.StateVar
            ctx.RenderState
            ctx.VdomObserver
            config
            ctx.PreviousVdom
            (fun () -> false)
