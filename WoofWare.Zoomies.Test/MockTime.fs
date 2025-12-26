namespace WoofWare.Zoomies.Test

open System
open WoofWare.Zoomies

/// A test harness that wraps an IncrementalState and provides time control.
type MockTimer =
    {
        /// The underlying IncrementalState.
        IncrState : IncrementalState<unit>
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
        let incrState = IncrementalState.make () bounds None
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
        let incrState = IncrementalState.make () bounds None
        IncrementalState.advanceClockAndStabilize defaultStartTime incrState
        let ctx = VdomContext.make incrState
        // Set the stabilization time so getUtcNow returns the correct time
        VdomContext.setCurrentStabilizationTime defaultStartTime ctx
        ctx

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

    /// Create a RenderState for testing from a MockTimer.
    /// Returns both the RenderState and a function to advance time that updates both the clock and the VdomContext.
    let makeRenderStateFromTimer<'postLayoutEvent>
        (console : IConsole)
        (timer : MockTimer)
        (debugWriter : System.IO.StreamWriter option)
        : RenderState<'postLayoutEvent> * (TimeSpan -> DateTime)
        =
        let vdomContext = VdomContext.make<unit, 'postLayoutEvent> timer.IncrState
        // Set the stabilization time so getUtcNow returns the correct time
        VdomContext.setCurrentStabilizationTime (timer.CurrentTime ()) vdomContext
        let renderState = RenderState.make console vdomContext debugWriter

        let advanceWithContext ts =
            let newTime = timer.Advance ts
            VdomContext.setCurrentStabilizationTime newTime vdomContext
            newTime

        renderState, advanceWithContext

    /// Create a RenderState for testing with a static mock time.
    /// This is for tests that don't need to control time.
    let makeRenderStateStatic<'postLayoutEvent>
        (console : IConsole)
        (debugWriter : System.IO.StreamWriter option)
        : RenderState<'postLayoutEvent>
        =
        let vdomContext = makeVdomContextFromConsole<'postLayoutEvent> console
        RenderState.make console vdomContext debugWriter

    /// Create a VdomContext from a MockTimer.
    /// Returns both the context and a function to advance time that updates both.
    /// Use this when you need to control time and test VdomContext together.
    let makeVdomContextFromTimer<'postLayoutEvent>
        (timer : MockTimer)
        : VdomContext<'postLayoutEvent> * (TimeSpan -> DateTime)
        =
        let ctx = VdomContext.make<unit, 'postLayoutEvent> timer.IncrState
        VdomContext.setCurrentStabilizationTime (timer.CurrentTime ()) ctx

        let advanceWithContext ts =
            let newTime = timer.Advance ts
            VdomContext.setCurrentStabilizationTime newTime ctx
            newTime

        ctx, advanceWithContext
