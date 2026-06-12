namespace WoofWare.Zoomies.Test

open System
open System.Threading.Tasks
open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies
open WoofWare.Expect

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestExternalEventSubscription =
    type TimerAppEvent =
        | TimerTick
        | StartTimer
        | StopTimer

    type TimerState =
        {
            TimerSubscription : IDisposable option
            Counter : int
        }

        static member Empty () =
            {
                Counter = 0
                TimerSubscription = None
            }

    type MockTimer (_ms : float) =
        let evt = Event<unit> ()

        let disposed =
            TaskCompletionSource<unit> TaskCreationOptions.RunContinuationsAsynchronously

        [<CLIEvent>]
        member _.Elapsed = evt.Publish

        member _.Trigger () =
            if disposed.Task.IsCompleted then
                raise (ObjectDisposedException "MockTimer")
            else
                evt.Trigger ()

        member _.Disposal = disposed.Task

        interface IDisposable with
            member _.Dispose () =
                disposed.TrySetResult () |> ignore<bool>

    /// Wrapper that tracks whether Dispose was called on the underlying IDisposable.
    type TrackingDisposable (inner : IDisposable) =
        let mutable wasDisposed = false
        member _.WasDisposed = wasDisposed

        interface IDisposable with
            member _.Dispose () =
                wasDisposed <- true
                inner.Dispose ()


    [<Test>]
    let ``Timer example`` () =
        task {
            /// So that the test harness can control the passage of time, we maintain a way to exfiltrate timers from the
            /// framework.
            let mutable globalTimer = None
            /// To verify that the subscription itself (not just the timer) is disposed
            let mutable globalSubscription : TrackingDisposable option = None
            /// Captured world bridge for posting events and setting up subscriptions
            let worldBridgeRef : IWorldBridge<TimerAppEvent> option ref = ref None

            /// Mutable state tracker for subscription management
            /// We need this because HandleInput doesn't have access to the current state
            let subscriptionStateRef : IDisposable option ref = ref None

            /// Pure transition function: state -> event -> state.
            /// Note: The timer subscription management happens in HandleInput as a side effect,
            /// since we need access to the world bridge for subscriptions.
            let transition (state : TimerState) (event : TimerAppEvent) : TimerState =
                match event with
                | StartTimer ->
                    // HandleInput created the subscription; record it in state for cleanup.
                    { state with
                        TimerSubscription = subscriptionStateRef.Value
                    }
                | StopTimer ->
                    // Dispose the subscription (side effect, but necessary for cleanup)
                    state.TimerSubscription |> Option.iter (fun s -> s.Dispose ())

                    globalTimer
                    |> Option.iter (fun (timer : MockTimer) -> (timer :> IDisposable).Dispose ())

                    { state with
                        TimerSubscription = None
                    }
                | TimerTick ->
                    { state with
                        Counter = state.Counter + 1
                    }

            /// Handle input events - converts WorldStateChange to app events.
            /// Also handles side effects for subscription management since we need the world bridge.
            let handleInput (change : WorldStateChange<TimerAppEvent>) : TimerAppEvent option =
                match change with
                | WorldStateChange.Keystroke c when c.KeyChar = ' ' ->
                    // Toggle timer on space
                    match subscriptionStateRef.Value with
                    | Some _ -> worldBridgeRef.Value |> Option.iter (fun bridge -> bridge.PostEvent StopTimer)
                    | None -> worldBridgeRef.Value |> Option.iter (fun bridge -> bridge.PostEvent StartTimer)

                    None // The event is posted async; we don't return it directly

                | WorldStateChange.ApplicationEvent StartTimer ->
                    // Set up the timer subscription
                    match worldBridgeRef.Value with
                    | Some bridge ->
                        let timer = new MockTimer (5000.0)

                        match globalTimer with
                        | None -> globalTimer <- Some timer
                        | Some _ -> failwith "only should have got one StartTimer"

                        let subscription = bridge.SubscribeEvent timer.Elapsed (fun _ -> TimerTick)
                        let trackedSubscription = new TrackingDisposable (subscription)
                        globalSubscription <- Some trackedSubscription
                        subscriptionStateRef.Value <- Some (trackedSubscription :> IDisposable)
                    | None -> ()

                    Some StartTimer

                | WorldStateChange.ApplicationEvent StopTimer ->
                    subscriptionStateRef.Value <- None
                    Some StopTimer

                | WorldStateChange.ApplicationEvent TimerTick -> Some TimerTick

                | _ -> None

            let vdom (_ : IVdomContext<_>) (state : TimerState) = Vdom.textContent $"%i{state.Counter}"

            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 1)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let config : AppConfig<TimerState, TimerAppEvent, unit> =
                {
                    Initial = TimerState.Empty ()
                    Transition = transition
                    View = App.pureView vdom
                    HandleInput = handleInput
                    HandlePostLayout = fun _ s -> s
                    FocusHandling = FocusHandling.FrameworkManaged
                    ActivationResolver = ActivationResolver.none
                    OnSetup = fun bridge -> worldBridgeRef.Value <- Some bridge
                }

            // Call OnSetup to capture the world bridge
            config.OnSetup (worldFreezer :> IWorldBridge<_>)

            use ctx = IncrTestContext.make console config None

            // Initial pump
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
0         |
"

                return ConsoleHarness.toString terminal
            }

            // Tell the app to start a timer
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
0         |
"

                return ConsoleHarness.toString terminal
            }

            // The `pumpOnce` enqueued the application event that will start the timer, but we're operating in a batch
            // that doesn't contain that enqueue.
            globalTimer.IsNone |> shouldEqual true
            // But after another pump, we'll process the timer-start.
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            match globalTimer with
            | Some timer -> timer.Trigger ()
            | None -> failwith "expected a timer to be running"

            // The timer has triggered an app event!
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
1         |
"

                return ConsoleHarness.toString terminal
            }

            match globalTimer with
            | Some timer -> timer.Trigger ()
            | None -> failwith "expected a timer to be running"

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
2         |
"

                return ConsoleHarness.toString terminal
            }

            // Tell the app to stop the timer
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
2         |
"

                return ConsoleHarness.toString terminal
            }

            // Again we need to pump again to actually process the "timer stop" request.
            match globalTimer with
            | Some timer ->
                timer.Disposal.IsCompleted |> shouldEqual false
                // Subscription should not be disposed yet
                match globalSubscription with
                | None -> failwith "should be subscribed"
                | Some globalSubscription -> globalSubscription.WasDisposed |> shouldEqual false

                IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

                do! timer.Disposal

                // Verify the subscription is torn down in state
                (IncrTestContext.currentState ctx).TimerSubscription |> shouldEqual None

                // Verify the subscription itself (not just the timer) was disposed
                match globalSubscription with
                | None -> failwith "should be subscribed"
                | Some globalSubscription -> globalSubscription.WasDisposed |> shouldEqual true

                // Verify that triggering after disposal throws, proving the timer is truly gone
                Assert.Throws<ObjectDisposedException> (fun () -> timer.Trigger ())
                |> ignore<ObjectDisposedException>

                // Pump once more and verify counter didn't increment from any stale tick
                IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

                (IncrTestContext.currentState ctx).Counter |> shouldEqual 2
            | None -> failwith "expected a timer to be running"
        }

    [<Test>]
    let ``events posted from a background task are folded into state in posting order`` () =
        task {
            // Regression test for the StateMachine deletion: the supported route for
            // cross-thread events is IWorldBridge.PostEvent, and posting order must be
            // preserved through the fold into the state var.
            let eventCount = 20

            let transition (state : int list) (event : int) : int list = event :: state

            let vdom (_ : IVdomContext<_>) (state : int list) =
                Vdom.textContent $"%i{List.length state}"

            let console, _terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 1)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let config : AppConfig<int list, int, unit> =
                {
                    Initial = []
                    Transition = transition
                    View = App.pureView vdom
                    HandleInput =
                        function
                        | WorldStateChange.ApplicationEvent ev -> Some ev
                        | _ -> None
                    HandlePostLayout = fun _ s -> s
                    FocusHandling = FocusHandling.FrameworkManaged
                    ActivationResolver = ActivationResolver.none
                    OnSetup = fun _ -> ()
                }

            use ctx = IncrTestContext.make console config None

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            let bridge = worldFreezer :> IWorldBridge<int>

            // Post from a background task; PostEvent guarantees enqueueing before it returns.
            do! Task.Run (fun () -> [ 1..eventCount ] |> List.iter bridge.PostEvent)

            // The framework may split batches, so pump until everything has arrived.
            let mutable state = IncrTestContext.pumpOnce worldFreezer config ctx
            let mutable pumps = 1

            while List.length state < eventCount && pumps < 100 do
                state <- IncrTestContext.pumpOnce worldFreezer config ctx
                pumps <- pumps + 1

            List.rev state |> shouldEqual [ 1..eventCount ]
        }
