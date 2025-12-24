namespace WoofWare.Zoomies.Test

open System
open WoofWare.Zoomies

type MockTimer =
    {
        GetUtcNow : unit -> DateTime
        /// Returns the resulting DateTime.
        Advance : TimeSpan -> DateTime
        SetTimeUtc : DateTime -> unit
    }

[<RequireQualifiedAccess>]
module MockTime =
    let getStaticUtcNow () =
        DateTime (2025, 11, 25, 13, 33, 00, DateTimeKind.Utc)

    let make () =
        // Eddie Hall's 500kg deadlift.
        // (https://www.youtube.com/watch?v=_DX2L4Pp8S0 took place on 2016-07-09 in Leeds, UK.
        // Eddie Hall hits the lift at timestamp 1:12:45. There are various watches displayed throughout the stream;
        // the clearest is the 19:15 local time displayed at timestamp 0:48:03.)
        let mutable now = DateTime (2016, 07, 09, 18, 39, 00, DateTimeKind.Utc)
        let lockObj = obj ()

        {
            GetUtcNow = fun () -> now
            Advance =
                fun ts ->
                    lock
                        lockObj
                        (fun () ->
                            now <- now + ts
                            now
                        )
            SetTimeUtc = fun dt -> lock lockObj (fun () -> now <- dt)
        }

    /// Create an IncrVdomContext for testing purposes with the given bounds and time function.
    let makeVdomContext'<'postLayoutEvent>
        (getUtcNow : unit -> DateTime)
        (bounds : Rectangle)
        : IncrVdomContext<'postLayoutEvent>
        =
        // Create an IncrementalState with a unit state (tests don't need state)
        let incrState = IncrementalState.make () bounds None
        IncrVdomContext.make' getUtcNow incrState

    /// Create an IncrVdomContext for testing purposes with the given bounds.
    /// Uses a static mock time.
    let makeVdomContext<'postLayoutEvent> (bounds : Rectangle) : IncrVdomContext<'postLayoutEvent> =
        makeVdomContext' getStaticUtcNow bounds

    /// Create an IncrVdomContext with default 80x24 terminal bounds for testing.
    let makeDefaultVdomContext<'postLayoutEvent> () : IncrVdomContext<'postLayoutEvent> =
        makeVdomContext
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = 80
                Height = 24
            }

    /// Create an IncrVdomContext from an IConsole for testing with a custom time function.
    let makeVdomContextFromConsole'<'postLayoutEvent>
        (getUtcNow : unit -> DateTime)
        (console : IConsole)
        : IncrVdomContext<'postLayoutEvent>
        =
        makeVdomContext'
            getUtcNow
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = console.WindowWidth ()
                Height = console.WindowHeight ()
            }

    /// Create an IncrVdomContext from an IConsole for testing.
    /// This is a helper to make migrating tests from the old API easier.
    /// Uses a static mock time.
    let makeVdomContextFromConsole<'postLayoutEvent> (console : IConsole) : IncrVdomContext<'postLayoutEvent> =
        makeVdomContextFromConsole' getStaticUtcNow console

    /// Create a RenderState for testing - backward-compatible helper.
    /// This replaces the old pattern of `RenderState.make console getUtcNow debugWriter`.
    let makeRenderState<'postLayoutEvent>
        (console : IConsole)
        (getUtcNow : unit -> DateTime)
        (debugWriter : System.IO.StreamWriter option)
        : RenderState<'postLayoutEvent>
        =
        let vdomContext = makeVdomContextFromConsole'<'postLayoutEvent> getUtcNow console
        RenderState.make console vdomContext debugWriter
