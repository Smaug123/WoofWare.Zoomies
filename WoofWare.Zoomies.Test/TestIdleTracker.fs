namespace WoofWare.Zoomies.Test

open System
open NUnit.Framework
open FsUnitTyped
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestIdleTracker =

    [<Test>]
    let ``Initial state has no last input time`` () =
        let state = IdleTracker.State.Initial
        state.LastInputTime |> shouldEqual ValueNone

    [<Test>]
    let ``RecordInput sets LastInputTime`` () =
        let now = DateTime (2024, 1, 15, 10, 30, 0, DateTimeKind.Utc)
        let state = IdleTracker.State.Initial.RecordInput now
        state.LastInputTime |> shouldEqual (ValueSome now)

    [<Test>]
    let ``IdleDuration returns Zero when no input recorded`` () =
        let now = DateTime (2024, 1, 15, 10, 30, 0, DateTimeKind.Utc)
        let state = IdleTracker.State.Initial
        state.IdleDuration now |> shouldEqual TimeSpan.Zero

    [<Test>]
    let ``IdleDuration returns correct duration after input`` () =
        let inputTime = DateTime (2024, 1, 15, 10, 30, 0, DateTimeKind.Utc)
        let checkTime = DateTime (2024, 1, 15, 10, 30, 5, DateTimeKind.Utc)
        let state = IdleTracker.State.Initial.RecordInput inputTime
        state.IdleDuration checkTime |> shouldEqual (TimeSpan.FromSeconds 5.0)

    [<Test>]
    let ``RecordInput overwrites previous input time`` () =
        let time1 = DateTime (2024, 1, 15, 10, 30, 0, DateTimeKind.Utc)
        let time2 = DateTime (2024, 1, 15, 10, 31, 0, DateTimeKind.Utc)
        let checkTime = DateTime (2024, 1, 15, 10, 31, 10, DateTimeKind.Utc)

        let state = IdleTracker.State.Initial.RecordInput(time1).RecordInput (time2)

        state.LastInputTime |> shouldEqual (ValueSome time2)
        state.IdleDuration checkTime |> shouldEqual (TimeSpan.FromSeconds 10.0)

    [<Test>]
    let ``isUserInput returns true for Keystroke`` () =
        let keyInfo = ConsoleKeyInfo ('a', ConsoleKey.A, false, false, false)
        let change : WorldStateChange<unit> = WorldStateChange.Keystroke keyInfo
        IdleTracker.isUserInput change |> shouldEqual true

    [<Test>]
    let ``isUserInput returns true for MouseEvent`` () =
        let coords =
            {
                X = 1
                Y = 1
            }

        let mouseEvent = MouseEvent.Press (MouseButton.Left, MouseModifiers.None, coords)
        let change : WorldStateChange<unit> = WorldStateChange.MouseEvent mouseEvent
        IdleTracker.isUserInput change |> shouldEqual true

    [<Test>]
    let ``isUserInput returns true for Paste`` () =
        let change : WorldStateChange<unit> = WorldStateChange.Paste "hello"
        IdleTracker.isUserInput change |> shouldEqual true

    [<Test>]
    let ``isUserInput returns false for ApplicationEvent`` () =
        let change : WorldStateChange<string> =
            WorldStateChange.ApplicationEvent "custom event"

        IdleTracker.isUserInput change |> shouldEqual false

    [<Test>]
    let ``isUserInput returns false for ApplicationEventException`` () =
        let change : WorldStateChange<unit> =
            WorldStateChange.ApplicationEventException (exn "test error")

        IdleTracker.isUserInput change |> shouldEqual false

    // AnimatedState tests

    [<Test>]
    let ``AnimatedState.Initial has no last input time and frame 0`` () =
        let state = IdleTracker.AnimatedState.Initial
        state.LastInputTime |> shouldEqual ValueNone
        state.LastPulseFrame |> shouldEqual 0

    [<Test>]
    let ``AnimatedState.RecordInput sets LastInputTime and resets frame`` () =
        let now = DateTime (2024, 1, 15, 10, 30, 0, DateTimeKind.Utc)

        let state =
            { IdleTracker.AnimatedState.Initial with
                LastPulseFrame = 5
            }
                .RecordInput
                now

        state.LastInputTime |> shouldEqual (ValueSome now)
        state.LastPulseFrame |> shouldEqual 0

    [<Test>]
    let ``AnimatedState.IdleDuration returns Zero when no input recorded`` () =
        let now = DateTime (2024, 1, 15, 10, 30, 0, DateTimeKind.Utc)
        let state = IdleTracker.AnimatedState.Initial
        state.IdleDuration now |> shouldEqual TimeSpan.Zero

    [<Test>]
    let ``AnimatedState.IdleDuration returns correct duration after input`` () =
        let inputTime = DateTime (2024, 1, 15, 10, 30, 0, DateTimeKind.Utc)
        let checkTime = DateTime (2024, 1, 15, 10, 30, 5, DateTimeKind.Utc)
        let state = IdleTracker.AnimatedState.Initial.RecordInput inputTime
        state.IdleDuration checkTime |> shouldEqual (TimeSpan.FromSeconds 5.0)

    [<Test>]
    let ``AdvancePulse returns false when idle under threshold`` () =
        let inputTime = DateTime (2024, 1, 15, 10, 30, 0, DateTimeKind.Utc)
        let checkTime = DateTime (2024, 1, 15, 10, 30, 2, DateTimeKind.Utc) // 2 seconds idle
        let state = IdleTracker.AnimatedState.Initial.RecordInput inputTime
        let struct (newState, shouldRerender) = state.AdvancePulse checkTime 3.0 5.0 10
        shouldRerender |> shouldEqual false
        newState |> shouldEqual state

    [<Test>]
    let ``AdvancePulse returns true when frame changes`` () =
        let inputTime = DateTime (2024, 1, 15, 10, 30, 0, DateTimeKind.Utc)
        let checkTime = DateTime (2024, 1, 15, 10, 30, 4, DateTimeKind.Utc) // 4 seconds idle (1s past threshold)
        let state = IdleTracker.AnimatedState.Initial.RecordInput inputTime
        // With 5 chars/sec and 1 second elapsed, we should be at frame 5
        let struct (newState, shouldRerender) = state.AdvancePulse checkTime 3.0 5.0 10
        shouldRerender |> shouldEqual true
        newState.LastPulseFrame |> shouldEqual 5

    [<Test>]
    let ``AdvancePulse returns false when frame unchanged`` () =
        let inputTime = DateTime (2024, 1, 15, 10, 30, 0, DateTimeKind.Utc)
        let checkTime = DateTime (2024, 1, 15, 10, 30, 4, DateTimeKind.Utc) // 4 seconds idle
        // Start with frame already at 5
        let state =
            { (IdleTracker.AnimatedState.Initial.RecordInput inputTime) with
                LastPulseFrame = 5
            }

        let struct (newState, shouldRerender) = state.AdvancePulse checkTime 3.0 5.0 10
        shouldRerender |> shouldEqual false
        newState.LastPulseFrame |> shouldEqual 5

    [<Test>]
    let ``AdvancePulse wraps frame around text length`` () =
        let inputTime = DateTime (2024, 1, 15, 10, 30, 0, DateTimeKind.Utc)
        // 5 seconds idle = 2s past threshold, at 5 chars/sec = 10 chars, but text is only 5 chars
        let checkTime = DateTime (2024, 1, 15, 10, 30, 5, DateTimeKind.Utc)
        let state = IdleTracker.AnimatedState.Initial.RecordInput inputTime
        let struct (newState, _) = state.AdvancePulse checkTime 3.0 5.0 5
        // 10 % 5 = 0
        newState.LastPulseFrame |> shouldEqual 0

    [<Test>]
    let ``AdvancePulse returns false for zero text length`` () =
        let inputTime = DateTime (2024, 1, 15, 10, 30, 0, DateTimeKind.Utc)
        let checkTime = DateTime (2024, 1, 15, 10, 30, 5, DateTimeKind.Utc)
        let state = IdleTracker.AnimatedState.Initial.RecordInput inputTime
        let struct (newState, shouldRerender) = state.AdvancePulse checkTime 3.0 5.0 0
        shouldRerender |> shouldEqual false
        newState |> shouldEqual state
