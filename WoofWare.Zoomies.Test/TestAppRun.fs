namespace WoofWare.Zoomies.Test

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAppRun =

    /// Marker to track when Flush is called (interleaved with terminal ops)
    type private ConsoleOp =
        | TerminalOp of TerminalOp
        | Flush

    [<Test>]
    let ``App.run registers bracketed paste on startup and unregisters on quit`` () =
        task {
            let ops = ConcurrentQueue<ConsoleOp> ()

            let console : IConsole =
                {
                    WindowWidth = fun () -> 80
                    WindowHeight = fun () -> 10
                    ColorMode = ColorMode.Color
                    Execute = fun op -> ops.Enqueue (TerminalOp op)
                    Flush = fun () -> ops.Enqueue Flush
                }

            let ctrlCHandler, _, _ = FakeCtrlCHandler.make ()

            let worldFreezer () =
                WorldFreezer.listen' UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

            let vdom (_ctx : IVdomContext<_>) (_state : unit) : Vdom<DesiredBounds> = Vdom.empty

            let config : AppConfig<unit, unit, unit> =
                {
                    Initial = ()
                    Transition = fun s _ -> s
                    View = App.pureView vdom
                    HandleInput = fun _ -> None
                    HandlePostLayout = fun _ s -> s
                    FocusHandling = FocusHandling.FrameworkManaged
                    ActivationResolver = ActivationResolver.none
                    OnSetup = fun _ -> ()
                }

            use cts = new CancellationTokenSource ()

            let appHandle =
                App.run cts.Token console (fun () -> TimeConversion.unixEpoch) ctrlCHandler worldFreezer config None

            do! appHandle.Ready
            cts.Cancel ()
            do! appHandle.Finished

            let opsList = ops.ToArray () |> Array.toList

            let findTerminalOp op =
                opsList |> List.tryFindIndex (fun consoleOp -> consoleOp = TerminalOp op)

            let findLastTerminalOp op =
                opsList
                |> List.mapi (fun i consoleOp -> i, consoleOp)
                |> List.filter (fun (_, consoleOp) -> consoleOp = TerminalOp op)
                |> List.tryLast
                |> Option.map fst

            let registerIndex = findTerminalOp TerminalOp.RegisterBracketedPaste
            let unregisterIndex = findTerminalOp TerminalOp.UnregisterBracketedPaste

            registerIndex.IsSome |> shouldEqual true
            unregisterIndex.IsSome |> shouldEqual true
            unregisterIndex.Value > registerIndex.Value |> shouldEqual true

            let enterAltScreenIndex = findTerminalOp TerminalOp.EnterAlternateScreen
            enterAltScreenIndex.IsSome |> shouldEqual true
            registerIndex.Value > enterAltScreenIndex.Value |> shouldEqual true

            let registerMouseModeIndex = findTerminalOp TerminalOp.RegisterMouseMode
            registerMouseModeIndex.IsSome |> shouldEqual true
            registerIndex.Value > registerMouseModeIndex.Value |> shouldEqual true

            let exitAltScreenIndex = findTerminalOp TerminalOp.ExitAlternateScreen
            exitAltScreenIndex.IsSome |> shouldEqual true
            unregisterIndex.Value < exitAltScreenIndex.Value |> shouldEqual true

            // Use findLastTerminalOp: SetCursorVisibility true may also be called during rendering.
            let setCursorVisibleIndex = findLastTerminalOp (TerminalOp.SetCursorVisibility true)
            setCursorVisibleIndex.IsSome |> shouldEqual true
            setCursorVisibleIndex.Value > registerIndex.Value |> shouldEqual true

            let unregisterMouseModeIndex = findTerminalOp TerminalOp.UnregisterMouseMode
            unregisterMouseModeIndex.IsSome |> shouldEqual true

            // Cleanup order: SetCursorVisible < UnregisterBracketedPaste < UnregisterMouseMode < ExitAlternateScreen
            setCursorVisibleIndex.Value < unregisterIndex.Value |> shouldEqual true
            unregisterIndex.Value < unregisterMouseModeIndex.Value |> shouldEqual true
            unregisterMouseModeIndex.Value < exitAltScreenIndex.Value |> shouldEqual true

            // Final flush comes after all cleanup ops and is the very last action
            let lastFlushIndex =
                opsList
                |> List.mapi (fun i op -> i, op)
                |> List.filter (fun (_, op) -> op = Flush)
                |> List.tryLast
                |> Option.map fst

            lastFlushIndex.IsSome |> shouldEqual true
            lastFlushIndex.Value > exitAltScreenIndex.Value |> shouldEqual true
            lastFlushIndex.Value |> shouldEqual (opsList.Length - 1)
        }

    // ============================================================
    // Time-based re-rendering tests
    // ============================================================

    [<Test>]
    let ``pumpOnce re-renders when time-based vdom changes`` () =
        let vdomCallCount = ref 0
        let capturedFrames = ResizeArray<int> ()

        let console : IConsole =
            {
                WindowWidth = fun () -> 80
                WindowHeight = fun () -> 10
                ColorMode = ColorMode.Color
                Execute = fun _ -> ()
                Flush = fun () -> ()
            }

        let listener =
            WorldFreezer.listen' UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

        let world = MockWorld.attach listener

        let incrVdom (ctx : VdomContext<unit>) (_stateNode : unit Node) : Vdom<DesiredBounds> Node =
            let incr = VdomContext.incr ctx
            let timeNode = VdomContext.clockTimeNode ctx
            // 10 fps = 100ms per frame
            let frameNode =
                IncrTime.spinnerFrameNodeFromTimeNode incr timeNode LoadingSpinner.FrameCount 10.0

            incr.Map
                (fun frame ->
                    vdomCallCount.Value <- vdomCallCount.Value + 1
                    capturedFrames.Add frame
                    LoadingSpinner.make frame
                )
                frameNode

        let config : AppConfig<unit, unit, unit> =
            {
                Initial = ()
                Transition = fun s _ -> s
                View = incrVdom
                HandleInput = fun _ -> None
                HandlePostLayout = fun _ s -> s
                FocusHandling = FocusHandling.FrameworkManaged
                ActivationResolver = ActivationResolver.none
                OnSetup = fun _ -> ()
            }

        use ctx = IncrTestContext.make console config None

        // Initial pump renders the first frame
        IncrTestContext.pumpOnce listener config ctx |> ignore
        vdomCallCount.Value |> shouldEqual 1

        // Advance past one 100ms frame boundary and pump again
        IncrTestContext.advanceTime (TimeSpan.FromMilliseconds 150.0) ctx
        IncrTestContext.pumpOnce listener config ctx |> ignore

        vdomCallCount.Value |> shouldEqual 2
        capturedFrames.Count |> shouldEqual 2
        capturedFrames.[0] <> capturedFrames.[1] |> shouldEqual true

    // ============================================================
    // App.pureView tests
    // ============================================================

    [<Test>]
    let ``App.pureView creates node that depends on state changes`` () =
        task {
            let bounds =
                {
                    TopLeftX = 0
                    TopLeftY = 0
                    Width = 80
                    Height = 24
                }

            let incrState = IncrementalState.make bounds None
            let incr = incrState.Incr
            let ctx = VdomContext.make<unit> incrState

            let stateVar = incr.Var.Create "initial"

            let mutable callCount = 0

            let pureVdom (_ctx : IVdomContext<unit>) (state : string) : Vdom<DesiredBounds> =
                callCount <- callCount + 1
                Vdom.textContent state

            let vdomNode = App.pureView pureVdom ctx (incr.Var.Watch stateVar)

            let observer = incr.Observe vdomNode
            incr.Stabilize ()

            let _ = Observer.value observer
            callCount |> shouldEqual 1

            incr.Var.Set stateVar "changed"
            incr.Stabilize ()

            let _ = Observer.value observer
            callCount |> shouldEqual 2
        }

    [<Test>]
    let ``App.pureView creates node that depends on bounds changes`` () =
        task {
            let bounds1 =
                {
                    TopLeftX = 0
                    TopLeftY = 0
                    Width = 80
                    Height = 24
                }

            let incrState = IncrementalState.make bounds1 None
            let incr = incrState.Incr
            let ctx = VdomContext.make<unit> incrState

            let stateVar = incr.Var.Create ()

            let mutable callCount = 0

            let pureVdom (ctx : IVdomContext<unit>) (_ : unit) : Vdom<DesiredBounds> =
                callCount <- callCount + 1
                Vdom.textContent $"Width: {ctx.TerminalBounds.Width}"

            let vdomNode = App.pureView pureVdom ctx (incr.Var.Watch stateVar)

            let observer = incr.Observe vdomNode
            incr.Stabilize ()

            let _ = Observer.value observer
            callCount |> shouldEqual 1

            let bounds2 =
                {
                    TopLeftX = 0
                    TopLeftY = 0
                    Width = 120
                    Height = 40
                }

            IncrementalState.setBounds bounds2 incrState
            incr.Stabilize ()

            let _ = Observer.value observer
            callCount |> shouldEqual 2
        }

    [<Test>]
    let ``App.pureView creates node that depends on focus changes`` () =
        task {
            let bounds =
                {
                    TopLeftX = 0
                    TopLeftY = 0
                    Width = 80
                    Height = 24
                }

            let key1 = NodeKey.make "key1"
            let incrState = IncrementalState.make bounds (Some key1)
            let incr = incrState.Incr
            let ctx = VdomContext.make<unit> incrState

            let stateVar = incr.Var.Create ()

            let mutable callCount = 0

            let pureVdom (ctx : IVdomContext<unit>) (_ : unit) : Vdom<DesiredBounds> Node =
                ctx.Incr.Map
                    (fun focusedKey ->
                        callCount <- callCount + 1

                        match focusedKey with
                        | Some key -> Vdom.textContent $"Focused: {key}"
                        | None -> Vdom.textContent "No focus"
                    )
                    ctx.FocusedKey

            let vdomNode = App.pureViewIncr pureVdom ctx (incr.Var.Watch stateVar)

            let observer = incr.Observe vdomNode
            incr.Stabilize ()

            let _ = Observer.value observer
            callCount |> shouldEqual 1

            let key2 = NodeKey.make "key2"
            IncrementalState.setFocusedKey (Some key2) incrState
            incr.Stabilize ()

            let _ = Observer.value observer
            callCount |> shouldEqual 2
        }

    [<Test>]
    let ``App.pureView does not trigger when nothing changes`` () =
        task {
            let bounds =
                {
                    TopLeftX = 0
                    TopLeftY = 0
                    Width = 80
                    Height = 24
                }

            let incrState = IncrementalState.make bounds None
            let incr = incrState.Incr
            let ctx = VdomContext.make<unit> incrState

            let stateVar = incr.Var.Create "state"

            let mutable callCount = 0

            let pureVdom (_ctx : IVdomContext<unit>) (state : string) : Vdom<DesiredBounds> =
                callCount <- callCount + 1
                Vdom.textContent state

            let vdomNode = App.pureView pureVdom ctx (incr.Var.Watch stateVar)

            let observer = incr.Observe vdomNode
            incr.Stabilize ()

            let _ = Observer.value observer
            callCount |> shouldEqual 1

            incr.Stabilize ()
            let _ = Observer.value observer
            callCount |> shouldEqual 1
        }

    // ============================================================
    // Resize forcing re-render tests
    // ============================================================

    [<Test>]
    let ``pumpOnce re-renders after terminal resize even when vdom does not depend on bounds`` () =
        let flushCount = ref 0

        let mutable consoleWidth = 80
        let mutable consoleHeight = 10

        let console : IConsole =
            {
                WindowWidth = fun () -> consoleWidth
                WindowHeight = fun () -> consoleHeight
                ColorMode = ColorMode.Color
                Execute = fun _ -> ()
                Flush = fun () -> flushCount.Value <- flushCount.Value + 1
            }

        let listener =
            WorldFreezer.listen' UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

        let world = MockWorld.attach listener

        // Static vdom: same reference on every stabilization
        let staticVdom = Vdom.textContent "Hello"

        let incrVdom (ctx : VdomContext<unit>) (_stateNode : unit Node) : Vdom<DesiredBounds> Node =
            let incr = VdomContext.incr ctx
            incr.Return staticVdom

        let config : AppConfig<unit, unit, unit> =
            {
                Initial = ()
                Transition = fun s _ -> s
                View = incrVdom
                HandleInput = fun _ -> None
                HandlePostLayout = fun _ s -> s
                FocusHandling = FocusHandling.FrameworkManaged
                ActivationResolver = ActivationResolver.none
                OnSetup = fun _ -> ()
            }

        use ctx = IncrTestContext.make console config None

        // Initial pump
        IncrTestContext.pumpOnce listener config ctx |> ignore
        let initialFlushCount = flushCount.Value
        initialFlushCount >= 1 |> shouldEqual true

        // Resize and notify
        consoleWidth <- 120
        consoleHeight <- 40
        listener.NotifyTerminalResize ()

        // Next pump picks up the resize and re-renders
        IncrTestContext.pumpOnce listener config ctx |> ignore

        flushCount.Value > initialFlushCount |> shouldEqual true

    // ============================================================
    // Event-driven loop tests: the loop sleeps until there is work.
    // ============================================================

    let private quietConsole (ops : ConcurrentQueue<ConsoleOp>) : IConsole =
        {
            WindowWidth = fun () -> 80
            WindowHeight = fun () -> 10
            ColorMode = ColorMode.Color
            Execute = fun op -> ops.Enqueue (TerminalOp op)
            Flush = fun () -> ops.Enqueue Flush
        }

    let private flushCount (ops : ConcurrentQueue<ConsoleOp>) =
        ops |> Seq.filter (fun op -> op = Flush) |> Seq.length

    /// Await `condition` becoming true, polling, failing the test after a generous timeout.
    let private awaitCondition (description : string) (condition : unit -> bool) : Task =
        task {
            let sw = System.Diagnostics.Stopwatch.StartNew ()

            while not (condition ()) && sw.Elapsed < TimeSpan.FromSeconds 10.0 do
                do! Task.Delay 10

            if not (condition ()) then
                failwith $"timed out waiting for: %s{description}"
        }

    [<Test>]
    let ``an idle app does no work`` () =
        task {
            let ops = ConcurrentQueue<ConsoleOp> ()
            let console = quietConsole ops
            let ctrlCHandler, _, _ = FakeCtrlCHandler.make ()

            let pumpCount = ref 0

            let getUtcNow () =
                Interlocked.Increment pumpCount |> ignore<int>
                TimeConversion.unixEpoch

            let worldFreezer () =
                WorldFreezer.listen'<unit> UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

            let config = TestConfig.passthrough<unit> (fun _ _ -> Vdom.textContent "static")

            use cts = new CancellationTokenSource ()

            let handle =
                App.run cts.Token console getUtcNow ctrlCHandler worldFreezer config None

            do! handle.Ready

            // Let any in-flight first iteration finish, then measure a quiet window.
            do! Task.Delay 100
            let pumpsBefore = pumpCount.Value
            let flushesBefore = flushCount ops

            do! Task.Delay 250

            pumpCount.Value |> shouldEqual pumpsBefore
            flushCount ops |> shouldEqual flushesBefore

            cts.Cancel ()
            do! handle.Finished
        }

    [<Test>]
    let ``a keystroke wakes the sleeping loop and is rendered`` () =
        task {
            let ops = ConcurrentQueue<ConsoleOp> ()
            let console = quietConsole ops
            let ctrlCHandler, _, _ = FakeCtrlCHandler.make ()

            let freezerRef = ref None

            let worldFreezer () =
                let f =
                    WorldFreezer.listen'<char> UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

                freezerRef.Value <- Some f
                f

            let seen = ConcurrentQueue<char> ()

            let config =
                TestConfig.withState<int, char, unit>
                    0
                    (fun s (c : char) ->
                        seen.Enqueue c
                        s + 1
                    )
                    (fun change ->
                        match change with
                        | WorldStateChange.Keystroke k -> Some k.KeyChar
                        | _ -> None
                    )
                    (fun _ count -> Vdom.textContent $"count: %i{count}")

            use cts = new CancellationTokenSource ()

            let handle =
                App.run cts.Token console (fun () -> TimeConversion.unixEpoch) ctrlCHandler worldFreezer config None

            do! handle.Ready

            let freezer = freezerRef.Value |> Option.get
            let flushesBefore = flushCount ops

            freezer.DeliverKeystroke (ConsoleKeyInfo ('x', ConsoleKey.X, false, false, false))

            do!
                awaitCondition
                    "keystroke processed and rendered"
                    (fun () -> seen |> Seq.contains 'x' && flushCount ops > flushesBefore)

            cts.Cancel ()
            do! handle.Finished
        }

    [<Test>]
    let ``clock alarms wake the loop: a spinner animates with no input at all`` () =
        task {
            let ops = ConcurrentQueue<ConsoleOp> ()
            let console = quietConsole ops
            let ctrlCHandler, _, _ = FakeCtrlCHandler.make ()

            let worldFreezer () =
                WorldFreezer.listen'<unit> UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

            // A spinner at 50fps: the timing wheel holds an alarm every 20ms.
            let incrVdom (ctx : VdomContext<unit>) (_state : unit Node) : Vdom<DesiredBounds> Node =
                let incr = VdomContext.unsafeIncr ctx
                let clock = VdomContext.clock ctx
                let frameNode = IncrTime.spinnerFrameNode incr clock LoadingSpinner.FrameCount 50.0
                incr.Map LoadingSpinner.make frameNode

            let config : AppConfig<unit, unit, unit> =
                {
                    Initial = ()
                    Transition = fun s _ -> s
                    View = incrVdom
                    HandleInput = fun _ -> None
                    HandlePostLayout = fun _ s -> s
                    FocusHandling = FocusHandling.FrameworkManaged
                    ActivationResolver = ActivationResolver.none
                    OnSetup = fun _ -> ()
                }

            use cts = new CancellationTokenSource ()

            let handle =
                App.run cts.Token console (fun () -> DateTime.UtcNow) ctrlCHandler worldFreezer config None

            do! handle.Ready

            let flushesBefore = flushCount ops

            // No input is ever delivered; only clock alarms can wake the loop.
            do! awaitCondition "spinner rendered several new frames" (fun () -> flushCount ops >= flushesBefore + 3)

            cts.Cancel ()
            do! handle.Finished
        }

    [<Test>]
    let ``a lone Esc is delivered after the disambiguation deadline, with no other wake source`` () =
        task {
            let ops = ConcurrentQueue<ConsoleOp> ()
            let console = quietConsole ops
            let ctrlCHandler, _, _ = FakeCtrlCHandler.make ()

            let freezerRef = ref None

            let worldFreezer () =
                // A real stopwatch: the 10ms Esc deadline must elapse in real time.
                let f =
                    WorldFreezer.listen'<char> UnrecognisedEscapeCodeBehaviour.Throw Stopwatch.system

                freezerRef.Value <- Some f
                f

            let seen = ConcurrentQueue<char> ()

            let config =
                TestConfig.withState<int, char, unit>
                    0
                    (fun s (c : char) ->
                        seen.Enqueue c
                        s + 1
                    )
                    (fun change ->
                        match change with
                        | WorldStateChange.Keystroke k -> Some k.KeyChar
                        | _ -> None
                    )
                    (fun _ count -> Vdom.textContent $"count: %i{count}")

            use cts = new CancellationTokenSource ()

            let handle =
                App.run cts.Token console (fun () -> TimeConversion.unixEpoch) ctrlCHandler worldFreezer config None

            do! handle.Ready

            let freezer = freezerRef.Value |> Option.get

            // A lone Esc: swallowed pending disambiguation. Only the freezer's internal
            // deadline can cause it to be delivered.
            freezer.DeliverKeystroke (ConsoleKeyInfo ('\u001B', ConsoleKey.Escape, false, false, false))

            do! awaitCondition "lone Esc delivered after the deadline" (fun () -> seen |> Seq.contains '\u001B')

            cts.Cancel ()
            do! handle.Finished
        }

    // ============================================================
    // EventLoop.waitForWork unit tests.
    // ============================================================

    [<Test>]
    let ``waitForWork returns immediately when the wake task is already complete`` () =
        EventLoop.waitForWork Task.CompletedTask ValueNone CancellationToken.None

    [<Test>]
    let ``waitForWork returns immediately on a zero or negative deadline`` () =
        let never = TaskCompletionSource ()
        EventLoop.waitForWork never.Task (ValueSome TimeSpan.Zero) CancellationToken.None
        EventLoop.waitForWork never.Task (ValueSome (TimeSpan.FromSeconds -1.0)) CancellationToken.None

    [<Test>]
    let ``waitForWork returns when the deadline elapses`` () =
        let never = TaskCompletionSource ()
        let sw = System.Diagnostics.Stopwatch.StartNew ()
        EventLoop.waitForWork never.Task (ValueSome (TimeSpan.FromMilliseconds 30.0)) CancellationToken.None
        (sw.Elapsed < TimeSpan.FromSeconds 5.0) |> shouldEqual true

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
