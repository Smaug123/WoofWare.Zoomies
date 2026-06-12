namespace WoofWare.Zoomies.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental
open WoofWare.TimingWheel
open WoofWare.Zoomies

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestIncrTime =

    // ============================================================
    // spinnerFrameNode tests
    // ============================================================

    [<Test>]
    let ``spinnerFrameNode starts at frame 0 when clock is at epoch`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // 10 fps means each frame lasts 100ms = 100_000_000 ns
            let frameNode = IncrTime.spinnerFrameNode incr clock 10 10.0

            let observer = incr.Observe frameNode
            incr.Stabilize ()

            let frame = Observer.value observer
            frame |> shouldEqual 0
        }

    [<Test>]
    let ``spinnerFrameNode advances frame when clock advances`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // 10 fps means each frame lasts 100ms = 100_000_000 ns
            let frameNode = IncrTime.spinnerFrameNode incr clock 10 10.0

            let observer = incr.Observe frameNode
            incr.Stabilize ()

            // Initially at frame 0
            Observer.value observer |> shouldEqual 0

            // Advance clock by 100ms (one frame at 10fps)
            let nsPerFrame = 100_000_000L // 100ms in ns
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch nsPerFrame)
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 1

            // Advance by another frame
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch (2L * nsPerFrame))
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 2
        }

    [<Test>]
    let ``spinnerFrameNode wraps around after all frames`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            let frameCount = 10

            // 10 fps means each frame lasts 100ms = 100_000_000 ns
            let frameNode = IncrTime.spinnerFrameNode incr clock frameCount 10.0

            let observer = incr.Observe frameNode
            incr.Stabilize ()

            // Advance clock by exactly frameCount frames (full cycle)
            let nsPerFrame = 100_000_000L
            let fullCycleNs = int64 frameCount * nsPerFrame

            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch fullCycleNs)
            incr.Stabilize ()

            // Should be back at frame 0
            Observer.value observer |> shouldEqual 0
        }

    [<Test>]
    let ``spinnerFrameNode at different fps values`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // 5 fps means each frame lasts 200ms = 200_000_000 ns
            let frameNode = IncrTime.spinnerFrameNode incr clock 10 5.0

            let observer = incr.Observe frameNode
            incr.Stabilize ()

            // At 100ms (half a frame at 5fps), should still be at frame 0
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 100_000_000L)
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 0

            // At 200ms (one full frame at 5fps), should be at frame 1
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 200_000_000L)
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 1
        }

    [<Test>]
    let ``spinnerFrameNode only propagates when frame changes`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // 10 fps means each frame lasts 100ms
            let frameNode = IncrTime.spinnerFrameNode incr clock 10 10.0

            // Track how many times the frame value changes
            let mutable previousFrame = -1
            let mutable changeCount = 0

            let observer = incr.Observe frameNode
            incr.Stabilize ()

            let checkAndCount () =
                let current = Observer.value observer

                if current <> previousFrame then
                    changeCount <- changeCount + 1
                    previousFrame <- current

            checkAndCount () // Initial
            changeCount |> shouldEqual 1

            // Advance by 10ms - still in frame 0
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 10_000_000L)
            incr.Stabilize ()
            checkAndCount ()
            changeCount |> shouldEqual 1 // No change

            // Advance to frame 1 (100ms total)
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 100_000_000L)
            incr.Stabilize ()
            checkAndCount ()
            changeCount |> shouldEqual 2 // Changed to frame 1
        }

    // ============================================================
    // spinnerFrameNode graceful fallback tests
    // ============================================================

    [<Test>]
    let ``spinnerFrameNode handles zero frameCount gracefully by treating as 1`` () =
        task {
            let incr = Incremental.make ()
            let clock = incr.Clock.Create (TimeNs.ofInt64NsSinceEpoch 0L)

            let frameNode = IncrTime.spinnerFrameNode incr clock 0 10.0

            let observer = incr.Observe frameNode
            incr.Stabilize ()

            // With frameCount treated as 1, frame index is always 0
            Observer.value observer |> shouldEqual 0

            // Even after advancing clock, still 0
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 1_000_000_000L)
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 0
        }

    [<Test>]
    let ``spinnerFrameNode handles negative frameCount gracefully by treating as 1`` () =
        task {
            let incr = Incremental.make ()
            let clock = incr.Clock.Create (TimeNs.ofInt64NsSinceEpoch 0L)

            let frameNode = IncrTime.spinnerFrameNode incr clock -5 10.0

            let observer = incr.Observe frameNode
            incr.Stabilize ()

            // With frameCount treated as 1, frame index is always 0
            Observer.value observer |> shouldEqual 0

            // Even after advancing clock, still 0
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 1_000_000_000L)
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 0
        }

    [<Test>]
    let ``spinnerFrameNode handles zero fps gracefully by returning frame 0`` () =
        task {
            let incr = Incremental.make ()
            let clock = incr.Clock.Create (TimeNs.ofInt64NsSinceEpoch 0L)

            let frameNode = IncrTime.spinnerFrameNode incr clock 10 0.0

            let observer = incr.Observe frameNode
            incr.Stabilize ()

            // With zero fps, frame index is always 0
            Observer.value observer |> shouldEqual 0

            // Even after advancing clock, still 0
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 1_000_000_000L)
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 0
        }

    [<Test>]
    let ``spinnerFrameNode handles negative fps gracefully by returning frame 0`` () =
        task {
            let incr = Incremental.make ()
            let clock = incr.Clock.Create (TimeNs.ofInt64NsSinceEpoch 0L)

            let frameNode = IncrTime.spinnerFrameNode incr clock 10 -5.0

            let observer = incr.Observe frameNode
            incr.Stabilize ()

            // With negative fps, frame index is always 0
            Observer.value observer |> shouldEqual 0

            // Even after advancing clock, still 0
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 1_000_000_000L)
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 0
        }

    // ============================================================
    // periodicTickNode tests
    // ============================================================

    [<Test>]
    let ``periodicTickNode starts at tick 0`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            let tickNode = IncrTime.periodicTickNode incr clock (TimeSpan.FromSeconds 1.0)

            let observer = incr.Observe tickNode
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 0L
        }

    [<Test>]
    let ``periodicTickNode increments when interval elapses`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // 1 second interval = 1_000_000_000 ns
            let tickNode = IncrTime.periodicTickNode incr clock (TimeSpan.FromSeconds 1.0)

            let observer = incr.Observe tickNode
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 0L

            // Advance by 1 second
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 1_000_000_000L)
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 1L

            // Advance by another second (2 seconds total)
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 2_000_000_000L)
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 2L
        }

    [<Test>]
    let ``periodicTickNode does not increment within interval`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // 1 second interval
            let tickNode = IncrTime.periodicTickNode incr clock (TimeSpan.FromSeconds 1.0)

            let observer = incr.Observe tickNode
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 0L

            // Advance by 500ms (half an interval)
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 500_000_000L)
            incr.Stabilize ()

            // Still at tick 0
            Observer.value observer |> shouldEqual 0L

            // Advance by another 500ms (1 second total)
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 1_000_000_000L)
            incr.Stabilize ()

            // Now at tick 1
            Observer.value observer |> shouldEqual 1L
        }

    [<Test>]
    let ``periodicTickNode handles millisecond intervals`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // 100ms interval = 100_000_000 ns
            let tickNode =
                IncrTime.periodicTickNode incr clock (TimeSpan.FromMilliseconds 100.0)

            let observer = incr.Observe tickNode
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 0L

            // Advance by 100ms
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 100_000_000L)
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 1L

            // Advance by 1 second (10 more ticks at 100ms interval)
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 1_100_000_000L)
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 11L
        }

    [<Test>]
    let ``periodicTickNode only propagates when tick changes`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // 1 second interval
            let tickNode = IncrTime.periodicTickNode incr clock (TimeSpan.FromSeconds 1.0)

            let mutable previousTick = -1L
            let mutable changeCount = 0

            let observer = incr.Observe tickNode
            incr.Stabilize ()

            let checkAndCount () =
                let current = Observer.value observer

                if current <> previousTick then
                    changeCount <- changeCount + 1
                    previousTick <- current

            checkAndCount () // Initial
            changeCount |> shouldEqual 1

            // Advance by 100ms - still in tick 0
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 100_000_000L)
            incr.Stabilize ()
            checkAndCount ()
            changeCount |> shouldEqual 1 // No change

            // Advance to tick 1 (1 second total)
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 1_000_000_000L)
            incr.Stabilize ()
            checkAndCount ()
            changeCount |> shouldEqual 2 // Changed to tick 1
        }

    // ============================================================
    // Alarm-scheduling tests: the clock-based variants keep an alarm
    // in the timing wheel so the event loop knows when to wake up.
    // ============================================================

    [<Test>]
    let ``spinnerFrameNode keeps a wake-up alarm scheduled while alive`` () =
        let incr = Incremental.make ()
        let clock = incr.Clock.Create (TimeNs.ofInt64NsSinceEpoch 0L)

        let fps = 10.0
        let intervalNs = int64 (1_000_000_000.0 / fps)

        let frameNode = IncrTime.spinnerFrameNode incr clock 10 fps
        let observer = incr.Observe frameNode
        incr.Stabilize ()

        let check () =
            match incr.Clock.NextAlarmFiresAt clock with
            | ValueNone -> failwith "expected a frame alarm to be scheduled"
            | ValueSome alarmAt ->
                let precision = TimeNs.Span.toInt64Ns (incr.Clock.AlarmPrecision clock)

                // The next alarm is never more than one frame (plus wheel precision) away.
                (TimeNs.toInt64NsSinceEpoch alarmAt <= intervalNs * 2L + precision)
                |> shouldEqual true

        check ()

        // After advancing past several frames, an alarm is still scheduled for the next one.
        incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch (intervalNs * 5L))
        incr.Stabilize ()

        match incr.Clock.NextAlarmFiresAt clock with
        | ValueNone -> failwith "expected a frame alarm to remain scheduled after advancing"
        | ValueSome alarmAt ->
            let precision = TimeNs.Span.toInt64Ns (incr.Clock.AlarmPrecision clock)

            (TimeNs.toInt64NsSinceEpoch alarmAt <= intervalNs * 6L + precision)
            |> shouldEqual true

        Observer.disallowFutureUse observer
        incr.Stabilize ()

    [<Test>]
    let ``spinnerFrameNode alarm is removed when its Bind scope is invalidated`` () =
        let incr = Incremental.make ()
        let clock = incr.Clock.Create (TimeNs.ofInt64NsSinceEpoch 0L)

        let showVar = incr.Var.Create true

        // Components live inside Bind scopes (the vdom builder's let!); when the bind
        // switches away, the spinner and its alarm must die with it.
        let node =
            incr.Bind
                (fun show ->
                    if show then
                        IncrTime.spinnerFrameNode incr clock 10 10.0
                    else
                        incr.Return -1
                )
                (incr.Var.Watch showVar)

        let observer = incr.Observe node
        incr.Stabilize ()

        incr.Clock.NextAlarmFiresAt clock |> shouldNotEqual ValueNone

        incr.Var.Set showVar false
        incr.Stabilize ()
        Observer.value observer |> shouldEqual -1

        incr.Clock.NextAlarmFiresAt clock |> shouldEqual ValueNone

        Observer.disallowFutureUse observer
        incr.Stabilize ()

    [<Test>]
    let ``degenerate spinner and tick parameters schedule no alarms`` () =
        let incr = Incremental.make ()
        let clock = incr.Clock.Create (TimeNs.ofInt64NsSinceEpoch 0L)

        let frameNode = IncrTime.spinnerFrameNode incr clock 10 0.0
        let frameNode2 = IncrTime.spinnerFrameNode incr clock 10 -3.0
        let tickNode = IncrTime.periodicTickNode incr clock (TimeSpan.FromSeconds -1.0)
        let tickNode2 = IncrTime.periodicTickNode incr clock TimeSpan.Zero

        let observers =
            [
                incr.Observe frameNode |> ignore
                incr.Observe frameNode2 |> ignore
                incr.Observe tickNode |> ignore
                incr.Observe tickNode2 |> ignore
            ]

        ignore observers
        incr.Stabilize ()

        incr.Clock.NextAlarmFiresAt clock |> shouldEqual ValueNone

    [<Test>]
    let ``spinnerFrameNode clamps sub-precision fps rather than throwing`` () =
        let incr = Incremental.make ()
        let clock = incr.Clock.Create (TimeNs.ofInt64NsSinceEpoch 0L)

        // 10000 fps implies a 100us frame, below the wheel's ~1ms precision; the wheel
        // would throw if asked for that alarm interval, so the framework must clamp.
        let frameNode = IncrTime.spinnerFrameNode incr clock 10 10000.0
        let observer = incr.Observe frameNode
        incr.Stabilize ()

        // Values still follow the true (unclamped) interval.
        incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 250_000L)
        incr.Stabilize ()
        Observer.value observer |> shouldEqual 2

        incr.Clock.NextAlarmFiresAt clock |> shouldNotEqual ValueNone

        Observer.disallowFutureUse observer
        incr.Stabilize ()
