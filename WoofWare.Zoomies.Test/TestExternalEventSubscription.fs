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

        let mutable disposed =
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


    [<Test>]
    let ``Timer example`` () =
        task {
            /// So that the test harness can control the passage of time, we maintain a way to exfiltrate timers from the
            /// WorldProcessor.
            let mutable globalTimer = None

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

                                newState <-
                                    { newState with
                                        TimerSubscription = Some subscription
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
                                | Some _ -> world.PostEvent (fun _ -> Task.FromResult StopTimer) |> ignore<Task<_>>
                                | None -> world.PostEvent (fun _ -> Task.FromResult StartTimer) |> ignore<Task<_>>

                            | _ -> ()

                        newState
                }

            let vdom (_ : VdomContext) (state : TimerState) =
                Vdom.textContent false $"%i{state.Counter}"

            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 1)
            let renderState = RenderState.make' console

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let processWorld = processWorld worldFreezer

            let mutable state = TimerState.Empty ()
            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
0         |
"

                return ConsoleHarness.toString terminal
            }

            // Tell the app to start a timer
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

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
            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            match globalTimer with
            | Some timer -> timer.Trigger ()
            | None -> failwith "expected a timer to be running"

            // The timer has triggered an app event!
            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

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

            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
2         |
"

                return ConsoleHarness.toString terminal
            }

            // Tell the app to stop the timer
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

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
                state <- App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom
                do! timer.Disposal
                ()
            | None -> failwith "expected a timer to be running"
        }
