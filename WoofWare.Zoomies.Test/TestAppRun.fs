namespace WoofWare.Zoomies.Test

open System
open System.Collections.Concurrent
open System.Threading
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

            let world = MockWorld.make ()

            let worldFreezer () =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

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
                App.run cts.Token console (fun () -> TimeConversion.unixEpoch) ctrlCHandler worldFreezer config None 0

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

        let world = MockWorld.make ()

        let listener =
            WorldFreezer.listen'
                UnrecognisedEscapeCodeBehaviour.Throw
                StopwatchMock.Empty
                world.KeyAvailable
                world.ReadKey

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

        let world = MockWorld.make ()

        let listener =
            WorldFreezer.listen'
                UnrecognisedEscapeCodeBehaviour.Throw
                StopwatchMock.Empty
                world.KeyAvailable
                world.ReadKey

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
