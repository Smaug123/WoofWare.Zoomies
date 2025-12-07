namespace WoofWare.Zoomies.Test

open System
open System.Threading.Tasks
open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies
open WoofWare.Expect

[<TestFixture>]
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
            /// WorldProcessor.
            let mutable globalTimer = None
            /// To verify that the subscription itself (not just the timer) is disposed
            let mutable globalSubscription : TrackingDisposable option = None

            let processWorld (world : IWorldBridge<TimerAppEvent>) =
                { new WorldProcessor<TimerAppEvent, TimerState> with
                    member _.ProcessWorld (changes, _, state) =
                        let mutable newState = state

                        for change in changes do
                            match change with
                            | WorldStateChange.ApplicationEvent StartTimer ->
                                // Set up a timer that ticks every 5 seconds
                                let timer = new MockTimer (5000.0)

                                match globalTimer with
                                | None -> globalTimer <- Some timer
                                | Some _ -> failwith "only should have got one StartTimer"

                                let subscription = world.SubscribeEvent timer.Elapsed (fun _ -> TimerTick)
                                let trackedSubscription = new TrackingDisposable (subscription)
                                globalSubscription <- Some trackedSubscription

                                newState <-
                                    { newState with
                                        TimerSubscription = Some (trackedSubscription :> IDisposable)
                                    }

                            | WorldStateChange.ApplicationEvent StopTimer ->
                                newState.TimerSubscription |> Option.iter (fun s -> s.Dispose ())
                                globalTimer |> Option.get :> IDisposable |> _.Dispose()

                                newState <-
                                    { newState with
                                        TimerSubscription = None
                                    }

                            | WorldStateChange.ApplicationEvent TimerTick ->
                                newState <-
                                    { newState with
                                        Counter = newState.Counter + 1
                                    }

                            | WorldStateChange.Keystroke c when c.KeyChar = ' ' ->
                                // Toggle timer on space
                                match newState.TimerSubscription with
                                | Some _ -> world.PostEvent StopTimer
                                | None -> world.PostEvent StartTimer

                            | _ -> ()

                        ProcessWorldResult.make newState
                }

            let vdom (_ : VdomContext) (state : TimerState) = Vdom.textContent $"%i{state.Counter}"

            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 1)
            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let processWorld = processWorld worldFreezer

            let mutable state = TimerState.Empty ()

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            expect {
                snapshot
                    @"
0         |
"

                return ConsoleHarness.toString terminal
            }

            // Tell the app to start a timer
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

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
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            match globalTimer with
            | Some timer -> timer.Trigger ()
            | None -> failwith "expected a timer to be running"

            // The timer has triggered an app event!
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

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

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            expect {
                snapshot
                    @"
2         |
"

                return ConsoleHarness.toString terminal
            }

            // Tell the app to stop the timer
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

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
                globalSubscription.Value.WasDisposed |> shouldEqual false

                state <-
                    App.pumpOnce
                        worldFreezer
                        state
                        (fun _ -> true)
                        renderState
                        processWorld
                        vdom
                        ActivationResolver.none
                        (fun () -> false)

                do! timer.Disposal

                // Verify the subscription is torn down in state
                state.TimerSubscription |> shouldEqual None

                // Verify the subscription itself (not just the timer) was disposed
                globalSubscription.Value.WasDisposed |> shouldEqual true

                // Verify that triggering after disposal throws, proving the timer is truly gone
                Assert.Throws<ObjectDisposedException> (fun () -> timer.Trigger ())
                |> ignore<ObjectDisposedException>

                // Pump once more and verify counter didn't increment from any stale tick
                state <-
                    App.pumpOnce
                        worldFreezer
                        state
                        (fun _ -> true)
                        renderState
                        processWorld
                        vdom
                        ActivationResolver.none
                        (fun () -> false)

                state.Counter |> shouldEqual 2
            | None -> failwith "expected a timer to be running"
        }
