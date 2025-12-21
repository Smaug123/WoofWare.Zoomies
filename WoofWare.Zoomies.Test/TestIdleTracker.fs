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
