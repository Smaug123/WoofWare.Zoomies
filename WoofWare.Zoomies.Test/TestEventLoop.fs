namespace WoofWare.Zoomies.Test

open System
open System.Collections.Concurrent
open System.Reflection
open System.Threading
open System.Threading.Tasks
open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

/// The render loop sleeps until there is work to do.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEventLoop =

    /// A console which discards output, counting how many times it has been flushed.
    let private quietConsole (flushes : int ref) : IConsole =
        {
            WindowWidth = fun () -> 80
            WindowHeight = fun () -> 10
            ColorMode = ColorMode.Color
            Execute = ignore
            Flush = fun () -> Interlocked.Increment flushes |> ignore<int>
        }

    let private flushCount (flushes : int ref) = Volatile.Read &flushes.contents

    /// Await `condition` becoming true, polling, failing the test after a generous timeout.
    let private awaitCondition (description : string) (condition : unit -> bool) : Task =
        task {
            let sw = Diagnostics.Stopwatch.StartNew ()

            while not (condition ()) && sw.Elapsed < TimeSpan.FromSeconds 10.0 do
                do! Task.Delay 10

            if not (condition ()) then
                failwith $"timed out waiting for: %s{description}"
        }

    /// A WorldProcessor which records every keystroke it sees, and counts them in its state.
    let private recordingProcessor
        (seen : ConcurrentQueue<char>)
        (_ : IWorldBridge<'appEvent>)
        : WorldProcessor<'appEvent, unit, int>
        =
        { new WorldProcessor<'appEvent, unit, int> with
            member _.ProcessWorld (changes, _, state) =
                let mutable state = state

                for change in changes do
                    match change with
                    | WorldStateChange.Keystroke k ->
                        seen.Enqueue k.KeyChar
                        state <- state + 1
                    | _ -> ()

                ProcessWorldResult.make state

            member _.ProcessPostLayoutEvents (_, _, state) = state
        }

    let private countingView (_ : IVdomContext<unit>) (count : int) : Vdom<DesiredBounds> =
        Vdom.textContent $"count: %i{count}"

    /// Start an app with a capturable freezer; returns the handle and a way to get the freezer.
    let private start
        (cts : CancellationTokenSource)
        (console : IConsole)
        (getUtcNow : unit -> DateTime)
        (ctrlC : CtrlCHandler)
        (stopwatch : IStopwatch)
        (processWorld : IWorldBridge<unit> -> WorldProcessor<unit, unit, int>)
        (view : IVdomContext<unit> -> int -> Vdom<DesiredBounds>)
        (resolver : ActivationResolver<unit, int>)
        : AppHandle * (unit -> WorldFreezer<unit>)
        =
        let freezerRef = ref None

        let worldFreezer () =
            let f = WorldFreezer.listen'<unit> UnrecognisedEscapeCodeBehaviour.Throw stopwatch
            freezerRef.Value <- Some f
            f

        let handle =
            App.run' cts.Token console getUtcNow ctrlC worldFreezer 0 (fun _ -> true) processWorld view resolver None

        handle, (fun () -> freezerRef.Value |> Option.get)

    [<Test>]
    let ``an idle app does no work`` () =
        task {
            let ops = ref 0
            let ctrlC, _, _ = FakeCtrlCHandler.make ()

            // The loop reads the clock on every iteration, so this counts iterations.
            let clockReads = ref 0

            let getUtcNow () =
                Interlocked.Increment clockReads |> ignore<int>
                DateTime.UtcNow

            use cts = new CancellationTokenSource ()

            let handle, _ =
                start
                    cts
                    (quietConsole ops)
                    getUtcNow
                    ctrlC
                    StopwatchMock.Empty
                    (recordingProcessor (ConcurrentQueue ()))
                    countingView
                    ActivationResolver.none

            do! handle.Ready

            // Let any in-flight first iteration finish, then measure a quiet window.
            do! Task.Delay 100
            let readsBefore = clockReads.Value
            let flushesBefore = flushCount ops

            do! Task.Delay 250

            clockReads.Value |> shouldEqual readsBefore
            flushCount ops |> shouldEqual flushesBefore

            cts.Cancel ()
            do! handle.Finished
        }

    [<Test>]
    let ``a keystroke wakes the sleeping loop and is rendered`` () =
        task {
            let ops = ref 0
            let ctrlC, _, _ = FakeCtrlCHandler.make ()
            let seen = ConcurrentQueue<char> ()
            use cts = new CancellationTokenSource ()

            let handle, freezer =
                start
                    cts
                    (quietConsole ops)
                    (fun () -> DateTime.UtcNow)
                    ctrlC
                    StopwatchMock.Empty
                    (recordingProcessor seen)
                    countingView
                    ActivationResolver.none

            do! handle.Ready
            do! Task.Delay 50
            let flushesBefore = flushCount ops

            freezer().DeliverKeystroke (ConsoleKeyInfo ('x', ConsoleKey.X, false, false, false))

            do!
                awaitCondition
                    "keystroke processed and rendered"
                    (fun () -> seen |> Seq.contains 'x' && flushCount ops > flushesBefore)

            cts.Cancel ()
            do! handle.Finished
        }

    [<Test>]
    let ``a posted application event wakes the sleeping loop`` () =
        task {
            let ops = ref 0
            let ctrlC, _, _ = FakeCtrlCHandler.make ()
            let processed = ref 0
            use cts = new CancellationTokenSource ()

            let processWorld (_ : IWorldBridge<unit>) =
                { new WorldProcessor<unit, unit, int> with
                    member _.ProcessWorld (changes, _, state) =
                        for change in changes do
                            match change with
                            | WorldStateChange.ApplicationEvent () -> Interlocked.Increment processed |> ignore<int>
                            | _ -> ()

                        ProcessWorldResult.make state

                    member _.ProcessPostLayoutEvents (_, _, state) = state
                }

            let handle, freezer =
                start
                    cts
                    (quietConsole ops)
                    (fun () -> DateTime.UtcNow)
                    ctrlC
                    StopwatchMock.Empty
                    processWorld
                    countingView
                    ActivationResolver.none

            do! handle.Ready
            do! Task.Delay 50

            do! Task.Run (fun () -> (freezer () :> IWorldBridge<unit>).PostEvent ())

            do! awaitCondition "posted event processed" (fun () -> processed.Value = 1)

            cts.Cancel ()
            do! handle.Finished
        }

    [<Test>]
    let ``a button's pressed highlight expires with no further input`` () =
        task {
            let ops = ref 0
            let ctrlC, _, _ = FakeCtrlCHandler.make ()
            let buttonKey = NodeKey.make "button"
            use cts = new CancellationTokenSource ()

            // Every pressed-state the view has been asked to draw, in order.
            let pressedStates = ConcurrentQueue<bool> ()

            let view (ctx : IVdomContext<unit>) (_ : int) : Vdom<DesiredBounds> =
                pressedStates.Enqueue (ctx.WasRecentlyActivated buttonKey)
                Button.make (ctx, buttonKey, "Press me", isInitiallyFocused = true)

            let handle, freezer =
                start
                    cts
                    (quietConsole ops)
                    (fun () -> DateTime.UtcNow)
                    ctrlC
                    StopwatchMock.Empty
                    (recordingProcessor (ConcurrentQueue ()))
                    view
                    (ActivationResolver.button buttonKey ())

            do! handle.Ready

            freezer().DeliverKeystroke (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            do! awaitCondition "button drawn pressed" (fun () -> pressedStates |> Seq.contains true)

            // No more input: only the activation's expiry can cause the unpressed redraw.
            do!
                awaitCondition
                    "button drawn unpressed after the activation window"
                    (fun () -> pressedStates |> Seq.skipWhile not |> Seq.exists not)

            cts.Cancel ()
            do! handle.Finished
        }

    [<Test>]
    let ``a lone Esc is delivered after the disambiguation deadline, with no other wake source`` () =
        task {
            let ops = ref 0
            let ctrlC, _, _ = FakeCtrlCHandler.make ()
            let seen = ConcurrentQueue<char> ()
            use cts = new CancellationTokenSource ()

            let handle, freezer =
                start
                    cts
                    (quietConsole ops)
                    (fun () -> DateTime.UtcNow)
                    ctrlC
                    // A real stopwatch: the 10ms Esc deadline must elapse in real time.
                    Stopwatch.system
                    (recordingProcessor seen)
                    countingView
                    ActivationResolver.none

            do! handle.Ready

            // A lone Esc is swallowed pending disambiguation. Only the freezer's internal
            // deadline can cause it to be delivered.
            freezer().DeliverKeystroke (ConsoleKeyInfo ('\u001B', ConsoleKey.Escape, false, false, false))

            do! awaitCondition "lone Esc delivered after the deadline" (fun () -> seen |> Seq.contains '\u001B')

            cts.Cancel ()
            do! handle.Finished
        }

    [<Test>]
    let ``Ctrl-C wakes the sleeping loop and stops the app`` () =
        task {
            let ops = ref 0
            let registered : ConsoleCancelEventHandler option ref = ref None

            let ctrlC =
                {
                    Register = fun h -> registered.Value <- Some h
                    Unregister = fun _ -> ()
                }

            // Nothing else will ever cancel this.
            use cts = new CancellationTokenSource ()

            let handle, _ =
                start
                    cts
                    (quietConsole ops)
                    (fun () -> DateTime.UtcNow)
                    ctrlC
                    StopwatchMock.Empty
                    (recordingProcessor (ConcurrentQueue ()))
                    countingView
                    ActivationResolver.none

            do! handle.Ready
            do! Task.Delay 50

            // ConsoleCancelEventArgs has no public constructor.
            let args =
                Activator.CreateInstance (
                    typeof<ConsoleCancelEventArgs>,
                    BindingFlags.Instance ||| BindingFlags.NonPublic,
                    null,
                    [| box ConsoleSpecialKey.ControlC |],
                    null
                )
                :?> ConsoleCancelEventArgs

            registered.Value.Value.Invoke (Unchecked.defaultof<obj>, args)

            // The first Ctrl-C is swallowed, so the process isn't killed under us.
            args.Cancel |> shouldEqual true
            do! handle.Finished.WaitAsync (TimeSpan.FromSeconds 10.0)
        }

    [<Test>]
    let ``waitForWork returns immediately when the wake task is already complete`` () =
        EventLoop.waitForWork Task.CompletedTask ValueNone CancellationToken.None

    [<Test>]
    let ``waitForWork returns immediately on a zero or negative deadline`` () =
        let never = TaskCompletionSource ()
        EventLoop.waitForWork never.Task (ValueSome TimeSpan.Zero) CancellationToken.None
        EventLoop.waitForWork never.Task (ValueSome (TimeSpan.FromSeconds -1.0)) CancellationToken.None

    [<Test>]
    let ``waitForWork does not return before the deadline`` () =
        let never = TaskCompletionSource ()
        let sw = Diagnostics.Stopwatch.StartNew ()
        // A sub-millisecond deadline: Task.Delay would truncate this to zero.
        EventLoop.waitForWork never.Task (ValueSome (TimeSpan.FromTicks 5_000L)) CancellationToken.None
        (sw.Elapsed >= TimeSpan.FromTicks 5_000L) |> shouldEqual true

        let sw = Diagnostics.Stopwatch.StartNew ()
        EventLoop.waitForWork never.Task (ValueSome (TimeSpan.FromMilliseconds 30.0)) CancellationToken.None
        (sw.Elapsed >= TimeSpan.FromMilliseconds 30.0) |> shouldEqual true

    [<Test>]
    let ``waitForWork returns when the wake task completes mid-wait`` () =
        task {
            let wake = TaskCompletionSource ()

            let waiter =
                Task.Run (fun () -> EventLoop.waitForWork wake.Task ValueNone CancellationToken.None)

            do! Task.Delay 30
            waiter.IsCompleted |> shouldEqual false
            wake.SetResult ()
            do! waiter.WaitAsync (TimeSpan.FromSeconds 5.0)
        }

    [<Test>]
    let ``waitForWork returns on cancellation`` () =
        task {
            let wake = TaskCompletionSource ()
            use cts = new CancellationTokenSource ()

            let waiter =
                Task.Run (fun () -> EventLoop.waitForWork wake.Task ValueNone cts.Token)

            do! Task.Delay 30
            waiter.IsCompleted |> shouldEqual false
            cts.Cancel ()
            do! waiter.WaitAsync (TimeSpan.FromSeconds 5.0)
        }
