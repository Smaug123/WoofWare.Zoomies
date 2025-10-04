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
            mutable TimerSubscription : IDisposable option
            mutable Counter : int
        }

        static member Empty () =
            {
                Counter = 0
                TimerSubscription = None
            }

    type MockTimer (_ms : float) =
        let evt = Event<unit> ()
        let mutable disposed = false

        [<CLIEvent>]
        member _.Elapsed = evt.Publish

        member _.Trigger () =
            if disposed then
                raise (ObjectDisposedException "MockTimer")
            else
                evt.Trigger ()

        member _.IsDisposed = disposed

        interface IDisposable with
            member _.Dispose () = disposed <- true


    [<Test>]
    let ``Timer example`` () =
        task {
            /// So that the test harness can control the passage of time, we maintain a way to exfiltrate timers from the
            /// WorldProcessor.
            let mutable globalTimer = None

            let processWorld (world : IWorldBridge<TimerAppEvent>) =
                { new WorldProcessor<TimerAppEvent, TimerState> with
                    member _.ProcessWorld (changes, _, state) =
                        for change in changes do
                            match change with
                            | WorldStateChange.ApplicationEvent StartTimer ->
                                // Set up a timer that ticks every 5 seconds
                                let timer = new MockTimer (5000.0)

                                match globalTimer with
                                | None -> globalTimer <- Some timer
                                | Some _ -> failwith "only should have got one StartTimer"

                                let subscription = world.SubscribeEvent timer.Elapsed (fun _ -> TimerTick)
                                state.TimerSubscription <- Some subscription

                            | WorldStateChange.ApplicationEvent StopTimer ->
                                state.TimerSubscription |> Option.iter (fun s -> s.Dispose ())
                                globalTimer |> Option.get :> IDisposable |> _.Dispose()
                                state.TimerSubscription <- None

                            | WorldStateChange.ApplicationEvent TimerTick -> state.Counter <- state.Counter + 1

                            | WorldStateChange.Keystroke c when c.KeyChar = ' ' ->
                                // Toggle timer on space
                                match state.TimerSubscription with
                                | Some _ -> world.PostEvent (fun _ -> Task.FromResult StopTimer) |> ignore<Task<_>>
                                | None -> world.PostEvent (fun _ -> Task.FromResult StartTimer) |> ignore<Task<_>>

                            | _ -> ()
                }

            let vdom (_ : RenderState) (state : TimerState) =
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

            let state = TimerState.Empty ()
            App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
0         |
"

                return ConsoleHarness.toString terminal
            }

            // Tell the app to start a timer
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

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
            App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            match globalTimer with
            | Some timer -> timer.Trigger ()
            | None -> failwith "expected a timer to be running"

            // The timer has triggered an app event!
            App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

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

            App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

            expect {
                snapshot
                    @"
2         |
"

                return ConsoleHarness.toString terminal
            }

            // Tell the app to stop the timer
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

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
                timer.IsDisposed |> shouldEqual false
                App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom
                timer.IsDisposed |> shouldEqual true
            | None -> failwith "expected a timer to be running"
        }
