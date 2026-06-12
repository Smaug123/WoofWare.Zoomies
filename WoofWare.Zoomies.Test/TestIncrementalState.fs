namespace WoofWare.Zoomies.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental
open WoofWare.TimingWheel
open WoofWare.Zoomies

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestIncrementalState =

    /// Empty rectangle for tests
    let private emptyRect : Rectangle =
        {
            TopLeftX = 0
            TopLeftY = 0
            Width = 0
            Height = 0
        }

    // ============================================================
    // TimeConversion tests
    // ============================================================

    /// Generator for DateTimes that can be converted to nanoseconds without overflow.
    /// Unix epoch is 1970-01-01, and int64 can hold ~292 years of nanoseconds.
    let validDateTimeArb =
        // Nanoseconds per tick is 100, ticks per second is 10_000_000
        // Max int64 is ~9.2e18, so max nanoseconds from epoch is ~292 years
        // Use a safe range: 1970 to 2200
        let minDate = TimeConversion.unixEpoch
        let maxDate = DateTime (2200, 1, 1, 0, 0, 0, DateTimeKind.Utc)
        // Seconds in range is ~7.3e9, which overflows int, so choose over int64.
        let secondsRange = (maxDate.Ticks - minDate.Ticks) / 10_000_000L

        Arb.fromGen (
            Gen.choose64 (0L, secondsRange)
            |> Gen.map (fun seconds -> minDate.AddSeconds (float seconds))
        )

    [<Test>]
    let ``TimeConversion round-trip preserves DateTime within tick precision`` () =
        let prop (dt : DateTime) =
            // Convert to UTC for consistent handling
            let utcDt = dt.ToUniversalTime ()
            let ns = TimeConversion.dateTimeToNs utcDt
            let roundTripped = TimeConversion.nsToDateTime ns

            // Should be equal (within tick precision, which is 100ns)
            // Since we convert to UTC, the round-tripped value should match
            roundTripped |> shouldEqual utcDt

        Check.One (propConfig, Prop.forAll validDateTimeArb prop)

    [<Test>]
    let ``TimeConversion handles epoch correctly`` () =
        let epoch = TimeConversion.unixEpoch
        let ns = TimeConversion.dateTimeToNs epoch
        WoofWare.TimingWheel.TimeNs.toInt64NsSinceEpoch ns |> shouldEqual 0L

        let roundTripped = TimeConversion.nsToDateTime ns
        roundTripped |> shouldEqual epoch

    [<Test>]
    let ``TimeConversion handles dates after epoch`` () =
        let dt = TimeConversion.unixEpoch.AddSeconds 1.0
        let ns = TimeConversion.dateTimeToNs dt
        WoofWare.TimingWheel.TimeNs.toInt64NsSinceEpoch ns |> shouldEqual 1_000_000_000L

    [<Test>]
    let ``TimeConversion handles dates well after epoch`` () =
        // 2024-01-01 00:00:00 UTC
        let dt = DateTime (2024, 1, 1, 0, 0, 0, DateTimeKind.Utc)
        let ns = TimeConversion.dateTimeToNs dt
        let roundTripped = TimeConversion.nsToDateTime ns
        roundTripped |> shouldEqual dt

    // ============================================================
    // IncrementalState tests
    // ============================================================

    [<Test>]
    let ``IncrementalState.make creates state with correct initial values`` () =
        let initialBounds =
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = 80
                Height = 24
            }

        let initialFocus = Some (NodeKey.make "focused")

        let incrState = IncrementalState.make initialBounds initialFocus

        incrState.Incr.Var.Value incrState.TerminalBoundsVar
        |> shouldEqual initialBounds

        incrState.Incr.Var.Value incrState.FocusedKeyVar |> shouldEqual initialFocus

    [<Test>]
    let ``IncrementalState.boundsNode returns working node`` () =
        let bounds1 =
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = 80
                Height = 24
            }

        let incrState = IncrementalState.make bounds1 None
        let node = IncrementalState.boundsNode incrState
        let observer = incrState.Incr.Observe node
        incrState.Incr.Stabilize ()

        Observer.value observer |> shouldEqual bounds1

        // Update bounds and verify node reflects change
        let bounds2 =
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = 120
                Height = 40
            }

        IncrementalState.setBounds bounds2 incrState
        incrState.Incr.Stabilize ()

        Observer.value observer |> shouldEqual bounds2

    [<Test>]
    let ``IncrementalState.focusedKeyNode returns working node`` () =
        let key1 = NodeKey.make "key1"
        let incrState = IncrementalState.make emptyRect (Some key1)
        let node = IncrementalState.focusedKeyNode incrState
        let observer = incrState.Incr.Observe node
        incrState.Incr.Stabilize ()

        Observer.value observer |> shouldEqual (Some key1)

        // Update focus and verify node reflects change
        let key2 = NodeKey.make "key2"
        IncrementalState.setFocusedKey (Some key2) incrState
        incrState.Incr.Stabilize ()

        Observer.value observer |> shouldEqual (Some key2)

    [<Test>]
    let ``IncrementalState.clockTimeNode returns working node`` () =
        let incrState = IncrementalState.make emptyRect None
        let node = IncrementalState.clockTimeNode incrState
        let observer = incrState.Incr.Observe node
        incrState.Incr.Stabilize ()

        let time1 = Observer.value observer

        // Advance clock and verify node reflects change
        let futureTime = TimeConversion.unixEpoch.AddSeconds 10.0
        IncrementalState.advanceClockAndStabilize futureTime incrState

        let time2 = Observer.value observer

        (WoofWare.TimingWheel.TimeNs.toInt64NsSinceEpoch time2) > (WoofWare.TimingWheel.TimeNs.toInt64NsSinceEpoch time1)
        |> shouldEqual true

    // ============================================================
    // VdomContext dirty propagation tests
    // ============================================================

    [<Test>]
    let ``setting unchanged bounds or focus does not dirty the graph`` () =
        let bounds =
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = 80
                Height = 24
            }

        let key = NodeKey.make "key1"
        let incrState = IncrementalState.make bounds (Some key)
        let ctx = VdomContext.make<unit> incrState
        let incr = incrState.Incr

        let mutable boundsRecomputes = 0
        let mutable focusRecomputes = 0

        let boundsProbe =
            VdomContext.boundsNode ctx
            |> incr.Map (fun b ->
                boundsRecomputes <- boundsRecomputes + 1
                b
            )

        let focusProbe =
            VdomContext.focusedKeyNode ctx
            |> incr.Map (fun f ->
                focusRecomputes <- focusRecomputes + 1
                f
            )

        let _boundsObserver = incr.Observe boundsProbe
        let _focusObserver = incr.Observe focusProbe
        incr.Stabilize ()

        boundsRecomputes |> shouldEqual 1
        focusRecomputes |> shouldEqual 1

        // Setting identical values must not propagate.
        VdomContext.setTerminalBounds bounds ctx
        VdomContext.setFocusedKey (Some key) ctx
        incr.Stabilize ()

        boundsRecomputes |> shouldEqual 1
        focusRecomputes |> shouldEqual 1

        // Setting different values must propagate.
        VdomContext.setTerminalBounds
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = 120
                Height = 40
            }
            ctx

        VdomContext.setFocusedKey None ctx
        incr.Stabilize ()

        boundsRecomputes |> shouldEqual 2
        focusRecomputes |> shouldEqual 2

    [<Test>]
    let ``VdomContext node accessors return working nodes`` () =
        let bounds =
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = 80
                Height = 24
            }

        let key = NodeKey.make "test"
        let incrState = IncrementalState.make bounds (Some key)
        let ctx = VdomContext.make<unit> incrState

        // boundsNode
        let boundsNode = VdomContext.boundsNode ctx
        let boundsObserver = incrState.Incr.Observe boundsNode
        incrState.Incr.Stabilize ()
        Observer.value boundsObserver |> shouldEqual bounds

        // focusedKeyNode
        let focusNode = VdomContext.focusedKeyNode ctx
        let focusObserver = incrState.Incr.Observe focusNode
        incrState.Incr.Stabilize ()
        Observer.value focusObserver |> shouldEqual (Some key)

        // clockTimeNode
        let clockNode = VdomContext.clockTimeNode ctx
        let clockObserver = incrState.Incr.Observe clockNode
        incrState.Incr.Stabilize ()
        Observer.value clockObserver |> shouldEqual 0L<timeNs>

        IncrementalState.advanceClockAndStabilize MockTime.defaultStartTime incrState
        Observer.value clockObserver |> shouldEqual 1468089540000000000L<timeNs>

    [<Test>]
    let ``NextAlarmFiresAt is visible and returns ValueNone on a fresh clock`` () =
        let incrState = IncrementalState.make emptyRect None

        incrState.Incr.Clock.NextAlarmFiresAt incrState.Clock |> shouldEqual ValueNone

    // ============================================================
    // advanceClockAndStabilize honesty: graph time equals the last
    // observed wall-clock time; no fabricated advances.
    // ============================================================

    [<Test>]
    let ``advanceClockAndStabilize never fabricates time`` () =
        // Monotone-with-repeats sequences: a start time plus non-negative
        // second-granularity deltas (constructed, not filtered; zeros included).
        let arb =
            Arb.fromGen (
                gen {
                    let! start = validDateTimeArb.Generator
                    let! deltas = Gen.listOf (Gen.choose (0, 100))
                    return start, deltas
                }
            )

        let prop (start : DateTime, deltas : int list) =
            let incrState = IncrementalState.make emptyRect None
            let observer = incrState.Incr.Observe (IncrementalState.clockTimeNode incrState)

            IncrementalState.advanceClockAndStabilize start incrState
            Observer.value observer |> shouldEqual (TimeConversion.dateTimeToNs start)

            let mutable current = start

            for delta in deltas do
                current <- current.AddSeconds (float delta)
                IncrementalState.advanceClockAndStabilize current incrState
                // The observed clock is exactly the requested time: zero deltas must not creep.
                Observer.value observer |> shouldEqual (TimeConversion.dateTimeToNs current)

        Check.One (propConfig, Prop.forAll arb prop)

    [<Test>]
    let ``advanceClockAndStabilize to an earlier time leaves the clock unchanged`` () =
        let arb =
            Arb.fromGen (
                gen {
                    let! start = validDateTimeArb.Generator
                    let! backwards = Gen.choose (1, 1000)
                    return start, backwards
                }
            )

        let prop (start : DateTime, backwardsSeconds : int) =
            let incrState = IncrementalState.make emptyRect None
            let observer = incrState.Incr.Observe (IncrementalState.clockTimeNode incrState)

            IncrementalState.advanceClockAndStabilize start incrState

            IncrementalState.advanceClockAndStabilize (start.AddSeconds (float -backwardsSeconds)) incrState
            Observer.value observer |> shouldEqual (TimeConversion.dateTimeToNs start)

        Check.One (propConfig, Prop.forAll arb prop)

    [<Test>]
    let ``advanceClockAndStabilize with unchanged time still stabilizes pending var sets`` () =
        let incrState = IncrementalState.make emptyRect None
        let var = incrState.Incr.Var.Create 0
        let observer = incrState.Incr.Observe (incrState.Incr.Var.Watch var)

        IncrementalState.advanceClockAndStabilize MockTime.defaultStartTime incrState
        Observer.value observer |> shouldEqual 0

        incrState.Incr.Var.Set var 1
        // Time has not moved, but stabilization must still propagate the var set.
        IncrementalState.advanceClockAndStabilize MockTime.defaultStartTime incrState
        Observer.value observer |> shouldEqual 1
