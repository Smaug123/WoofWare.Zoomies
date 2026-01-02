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
        let tickRange = maxDate.Ticks - minDate.Ticks

        Arb.fromGen (
            Gen.choose (0, int (tickRange / 10_000_000L)) // seconds in range
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
        let initialState = "test state"

        let initialBounds =
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = 80
                Height = 24
            }

        let initialFocus = Some (NodeKey.make "focused")

        let incrState = IncrementalState.make initialState initialBounds initialFocus

        // Verify initial values can be read
        incrState.Incr.Var.Value incrState.StateVar |> shouldEqual initialState

        incrState.Incr.Var.Value incrState.TerminalBoundsVar
        |> shouldEqual initialBounds

        incrState.Incr.Var.Value incrState.FocusedKeyVar |> shouldEqual initialFocus

    [<Test>]
    let ``IncrementalState.stateNode returns working node`` () =
        let incrState = IncrementalState.make 42 emptyRect None
        let node = IncrementalState.stateNode incrState
        let observer = incrState.Incr.Observe node
        incrState.Incr.Stabilize ()

        Observer.value observer |> shouldEqual 42

        // Update state and verify node reflects change
        IncrementalState.setState 100 incrState
        incrState.Incr.Stabilize ()

        Observer.value observer |> shouldEqual 100

    [<Test>]
    let ``IncrementalState.boundsNode returns working node`` () =
        let bounds1 =
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = 80
                Height = 24
            }

        let incrState = IncrementalState.make () bounds1 None
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
        let incrState = IncrementalState.make () emptyRect (Some key1)
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
        let incrState = IncrementalState.make () emptyRect None
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
    let ``VdomContext.setTerminalBounds marks dirty when bounds change`` () =
        let bounds1 =
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = 80
                Height = 24
            }

        let incrState = IncrementalState.make () bounds1 None
        let ctx = VdomContext.make<unit, unit> incrState

        // Start clean
        VdomContext.markClean ctx
        VdomContext.isDirty ctx |> shouldEqual false

        // Change bounds
        let bounds2 =
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = 120
                Height = 40
            }

        VdomContext.setTerminalBounds bounds2 ctx

        // Should be dirty
        VdomContext.isDirty ctx |> shouldEqual true

    [<Test>]
    let ``VdomContext.setTerminalBounds does not mark dirty when bounds unchanged`` () =
        let bounds =
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = 80
                Height = 24
            }

        let incrState = IncrementalState.make () bounds None
        let ctx = VdomContext.make<unit, unit> incrState

        // Start clean
        VdomContext.markClean ctx
        VdomContext.isDirty ctx |> shouldEqual false

        // Set same bounds
        VdomContext.setTerminalBounds bounds ctx

        // Should still be clean
        VdomContext.isDirty ctx |> shouldEqual false

    [<Test>]
    let ``VdomContext.setFocusedKey marks dirty when focus changes`` () =
        let key1 = NodeKey.make "key1"
        let incrState = IncrementalState.make () emptyRect (Some key1)
        let ctx = VdomContext.make<unit, unit> incrState

        // Start clean
        VdomContext.markClean ctx
        VdomContext.isDirty ctx |> shouldEqual false

        // Change focus
        let key2 = NodeKey.make "key2"
        VdomContext.setFocusedKey (Some key2) ctx

        // Should be dirty
        VdomContext.isDirty ctx |> shouldEqual true

    [<Test>]
    let ``VdomContext.setFocusedKey does not mark dirty when focus unchanged`` () =
        let key = NodeKey.make "key1"
        let incrState = IncrementalState.make () emptyRect (Some key)
        let ctx = VdomContext.make<unit, unit> incrState

        // Start clean
        VdomContext.markClean ctx
        VdomContext.isDirty ctx |> shouldEqual false

        // Set same focus
        VdomContext.setFocusedKey (Some key) ctx

        // Should still be clean
        VdomContext.isDirty ctx |> shouldEqual false

    [<Test>]
    let ``VdomContext.setFocusedKey handles None to Some transition`` () =
        let incrState = IncrementalState.make () emptyRect None
        let ctx = VdomContext.make<unit, unit> incrState

        // Start clean
        VdomContext.markClean ctx
        VdomContext.isDirty ctx |> shouldEqual false

        // Set focus from None to Some
        let key = NodeKey.make "key1"
        VdomContext.setFocusedKey (Some key) ctx

        // Should be dirty
        VdomContext.isDirty ctx |> shouldEqual true

    [<Test>]
    let ``VdomContext.setFocusedKey handles Some to None transition`` () =
        let key = NodeKey.make "key1"
        let incrState = IncrementalState.make () emptyRect (Some key)
        let ctx = VdomContext.make<unit, unit> incrState

        // Start clean
        VdomContext.markClean ctx
        VdomContext.isDirty ctx |> shouldEqual false

        // Set focus from Some to None
        VdomContext.setFocusedKey None ctx

        // Should be dirty
        VdomContext.isDirty ctx |> shouldEqual true

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
        let incrState = IncrementalState.make () bounds (Some key)
        let ctx = VdomContext.make<unit, unit> incrState

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
