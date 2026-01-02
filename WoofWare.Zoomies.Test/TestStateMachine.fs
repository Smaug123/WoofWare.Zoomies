namespace WoofWare.Zoomies.Test

open System.Collections.Generic
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental
open WoofWare.Zoomies

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStateMachine =

    // ============================================================
    // Basic creation and initial state tests
    // ============================================================

    [<Test>]
    let ``StateMachine.create returns initial state`` () =
        let incr = Incremental.make ()
        let initial = 42

        let sm = StateMachine.create incr.State initial (fun s (_ : unit) -> s)

        let observer = incr.Observe sm.StateNode
        incr.Stabilize ()

        Observer.value observer |> shouldEqual initial

    [<Test>]
    let ``StateMachine.CurrentState returns initial state before stabilization`` () =
        let incr = Incremental.make ()
        let initial = "hello"

        let sm = StateMachine.create incr.State initial (fun s (_ : unit) -> s)

        sm.CurrentState () |> shouldEqual initial

    // ============================================================
    // Inject and transition tests
    // ============================================================

    [<Test>]
    let ``StateMachine.Inject followed by stabilize applies transition`` () =
        let incr = Incremental.make ()
        let initial = 0

        let transition state (event : int) = state + event

        let sm = StateMachine.create incr.State initial transition

        let observer = incr.Observe sm.StateNode
        incr.Stabilize ()

        Observer.value observer |> shouldEqual 0

        sm.Inject 5
        incr.Stabilize ()

        Observer.value observer |> shouldEqual 5

    [<Test>]
    let ``Multiple injected events are folded in order`` () =
        let incr = Incremental.make ()
        let initial : int list = []

        // Transition prepends event to state, so order matters
        let transition state event = event :: state

        let sm = StateMachine.create incr.State initial transition

        let observer = incr.Observe sm.StateNode
        incr.Stabilize ()

        // Inject events in order
        sm.Inject 1
        sm.Inject 2
        sm.Inject 3
        incr.Stabilize ()

        // Events should be folded left-to-right, so prepending gives [3; 2; 1]
        Observer.value observer |> shouldEqual [ 3 ; 2 ; 1 ]

    [<Test>]
    let ``Events injected between stabilizations are batched`` () =
        let incr = Incremental.make ()
        let transitionCount = ref 0
        let initial = 0

        let transition state (event : int) =
            transitionCount.Value <- transitionCount.Value + 1
            state + event

        let sm = StateMachine.create incr.State initial transition

        let observer = incr.Observe sm.StateNode
        incr.Stabilize ()

        // Reset count after initial stabilization
        transitionCount.Value <- 0

        // Inject multiple events before stabilizing
        sm.Inject 1
        sm.Inject 2
        sm.Inject 3

        // Should not have processed yet
        transitionCount.Value |> shouldEqual 0

        incr.Stabilize ()

        // All events processed in single stabilization
        transitionCount.Value |> shouldEqual 3
        Observer.value observer |> shouldEqual 6

    // ============================================================
    // SetState tests
    // ============================================================

    [<Test>]
    let ``SetState directly updates state after stabilization`` () =
        let incr = Incremental.make ()
        let initial = 0

        let sm = StateMachine.create incr.State initial (fun s (_ : unit) -> s)

        let observer = incr.Observe sm.StateNode
        incr.Stabilize ()

        Observer.value observer |> shouldEqual 0

        sm.SetState 100
        incr.Stabilize ()

        Observer.value observer |> shouldEqual 100

    [<Test>]
    let ``SetState updates CurrentState synchronously`` () =
        let incr = Incremental.make ()
        let initial = 0

        let sm = StateMachine.create incr.State initial (fun s (_ : unit) -> s)

        sm.CurrentState () |> shouldEqual 0

        sm.SetState 42

        // CurrentState should reflect the new value immediately
        sm.CurrentState () |> shouldEqual 42

    [<Test>]
    let ``SetState followed by Inject applies both`` () =
        let incr = Incremental.make ()
        let initial = 0

        let transition state (event : int) = state + event

        let sm = StateMachine.create incr.State initial transition

        let observer = incr.Observe sm.StateNode
        incr.Stabilize ()

        sm.SetState 100
        sm.Inject 5
        incr.Stabilize ()

        // SetState sets to 100, then Inject adds 5
        Observer.value observer |> shouldEqual 105

    // ============================================================
    // Property-based tests
    // ============================================================

    [<Test>]
    let ``Folding events via StateMachine equals direct fold`` () =
        let prop (initial : int) (events : int list) =
            let incr = Incremental.make ()
            let transition state event = state + event

            let sm = StateMachine.create incr.State initial transition

            let observer = incr.Observe sm.StateNode
            incr.Stabilize ()

            for ev in events do
                sm.Inject ev

            incr.Stabilize ()

            let expected = List.fold transition initial events
            let actual = Observer.value observer

            actual |> shouldEqual expected

        Check.One (propConfig, prop)

    [<Test>]
    let ``StateMachine preserves event order`` () =
        let prop (events : NonEmptyArray<int>) =
            let events = events.Get |> Array.toList
            let incr = Incremental.make ()
            let initial : int list = []

            // Collect events in order they're processed
            let transition state event = event :: state

            let sm = StateMachine.create incr.State initial transition

            let observer = incr.Observe sm.StateNode
            incr.Stabilize ()

            for ev in events do
                sm.Inject ev

            incr.Stabilize ()

            // Events should be in reverse order (since we prepend)
            let expected = List.rev events
            let actual = Observer.value observer

            actual |> shouldEqual expected

        Check.One (propConfig, prop)

    [<Test>]
    let ``Multiple stabilizations without new events don't change state`` () =
        let prop (initial : int) =
            let incr = Incremental.make ()
            let transitionCount = ref 0

            let transition state (event : int) =
                transitionCount.Value <- transitionCount.Value + 1
                state + event

            let sm = StateMachine.create incr.State initial transition

            let observer = incr.Observe sm.StateNode
            incr.Stabilize ()

            // Reset after initial stabilization
            transitionCount.Value <- 0
            let stateAfterInit = Observer.value observer

            // Stabilize multiple times without injecting
            incr.Stabilize ()
            incr.Stabilize ()
            incr.Stabilize ()

            // Transition should not have been called
            transitionCount.Value |> shouldEqual 0
            Observer.value observer |> shouldEqual stateAfterInit

        Check.One (propConfig, prop)

    [<Test>]
    let ``SetState then stabilize matches CurrentState`` () =
        let prop (initial : int) (newState : int) =
            let incr = Incremental.make ()

            let sm = StateMachine.create incr.State initial (fun s (_ : unit) -> s)

            let observer = incr.Observe sm.StateNode
            incr.Stabilize ()

            sm.SetState newState
            incr.Stabilize ()

            Observer.value observer |> shouldEqual newState
            sm.CurrentState () |> shouldEqual newState

        Check.One (propConfig, prop)

    // ============================================================
    // Edge cases
    // ============================================================

    [<Test>]
    let ``StateMachine works with unit state`` () =
        let incr = Incremental.make ()

        let sm = StateMachine.create incr.State () (fun () _ -> ())

        let observer = incr.Observe sm.StateNode
        incr.Stabilize ()

        sm.Inject "event"
        incr.Stabilize ()

        Observer.value observer |> shouldEqual ()

    [<Test>]
    let ``StateMachine works with reference type state`` () =
        let incr = Incremental.make ()
        let initial = ResizeArray<int> ()

        let transition (state : ResizeArray<int>) (event : int) =
            let newState = ResizeArray<int> (state)
            newState.Add event
            newState

        let sm = StateMachine.create incr.State initial transition

        let observer = incr.Observe sm.StateNode
        incr.Stabilize ()

        sm.Inject 1
        sm.Inject 2
        incr.Stabilize ()

        let result = Observer.value observer
        result |> Seq.toList |> shouldEqual [ 1 ; 2 ]

    [<Test>]
    let ``StateMachine handles empty event sequence`` () =
        let incr = Incremental.make ()
        let initial = 42

        let sm = StateMachine.create incr.State initial (fun s (_ : unit) -> s)

        let observer = incr.Observe sm.StateNode
        incr.Stabilize ()

        // No events injected
        incr.Stabilize ()

        Observer.value observer |> shouldEqual initial
