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
    let ``App.run' registers bracketed paste on startup and unregisters on quit`` () =
        task {
            let ops = ConcurrentQueue<ConsoleOp> ()

            let console : IConsole =
                {
                    BackgroundColor = fun () -> ConsoleColor.Black
                    ForegroundColor = fun () -> ConsoleColor.White
                    WindowWidth = fun () -> 80
                    WindowHeight = fun () -> 10
                    ColorMode = ColorMode.Color
                    Execute = fun op -> ops.Enqueue (TerminalOp op)
                    Flush = fun () -> ops.Enqueue Flush
                }

            let clock = MockTime.make ()
            let ctrlCHandler, _, _ = FakeCtrlCHandler.make ()

            let world = MockWorld.make ()

            let worldFreezer () =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ctx : IVdomContext<_>) (_state : unit) : Vdom<DesiredBounds> = Vdom.empty

            let processWorld (_bridge : IWorldBridge<unit>) = WorldProcessor.passthrough

            let resolver = ActivationResolver.none

            use cts = new CancellationTokenSource ()

            let appHandle =
                App.run'
                    cts.Token
                    console
                    ctrlCHandler
                    worldFreezer
                    ()
                    (fun _ -> false)
                    processWorld
                    (App.pureView vdom)
                    resolver
                    None

            // Wait for the app to be ready (initial setup and first render complete)
            do! appHandle.Ready

            // Cancel to trigger quit
            cts.Cancel ()

            // Wait for the app to finish
            do! appHandle.Finished

            // Check that we got the expected operations
            let opsList = ops.ToArray () |> Array.toList

            // Helper to find first index of a terminal op
            let findTerminalOp op =
                opsList |> List.tryFindIndex (fun consoleOp -> consoleOp = TerminalOp op)

            // Helper to find last index of a terminal op
            let findLastTerminalOp op =
                opsList
                |> List.mapi (fun i consoleOp -> i, consoleOp)
                |> List.filter (fun (_, consoleOp) -> consoleOp = TerminalOp op)
                |> List.tryLast
                |> Option.map fst

            // RegisterBracketedPaste should appear early (after EnterAlternateScreen, RegisterMouseMode)
            let registerIndex = findTerminalOp TerminalOp.RegisterBracketedPaste
            let unregisterIndex = findTerminalOp TerminalOp.UnregisterBracketedPaste

            registerIndex.IsSome |> shouldEqual true
            unregisterIndex.IsSome |> shouldEqual true

            // Unregister should come after register
            unregisterIndex.Value > registerIndex.Value |> shouldEqual true

            // Verify order: Register should come after EnterAlternateScreen
            let enterAltScreenIndex = findTerminalOp TerminalOp.EnterAlternateScreen

            enterAltScreenIndex.IsSome |> shouldEqual true
            registerIndex.Value > enterAltScreenIndex.Value |> shouldEqual true

            // Verify order: Register should come after RegisterMouseMode
            let registerMouseModeIndex = findTerminalOp TerminalOp.RegisterMouseMode

            registerMouseModeIndex.IsSome |> shouldEqual true
            registerIndex.Value > registerMouseModeIndex.Value |> shouldEqual true

            // Verify order: Unregister should come before ExitAlternateScreen
            let exitAltScreenIndex = findTerminalOp TerminalOp.ExitAlternateScreen

            exitAltScreenIndex.IsSome |> shouldEqual true
            unregisterIndex.Value < exitAltScreenIndex.Value |> shouldEqual true

            // Verify SetCursorVisibility true is called during cleanup.
            // Use findLastTerminalOp because SetCursorVisibility true may also be called during rendering
            // (e.g., when showing a cursor in a text box). The cleanup SetCursorVisibility true is the last one.
            let setCursorVisibleIndex = findLastTerminalOp (TerminalOp.SetCursorVisibility true)
            setCursorVisibleIndex.IsSome |> shouldEqual true
            // The cleanup SetCursorVisibility true must come after startup ops (proving it's in teardown, not setup)
            setCursorVisibleIndex.Value > registerIndex.Value |> shouldEqual true

            // Verify UnregisterMouseMode is called during cleanup
            let unregisterMouseModeIndex = findTerminalOp TerminalOp.UnregisterMouseMode
            unregisterMouseModeIndex.IsSome |> shouldEqual true

            // Verify cleanup order: SetCursorVisibility true comes before UnregisterBracketedPaste
            setCursorVisibleIndex.Value < unregisterIndex.Value |> shouldEqual true
            // UnregisterBracketedPaste comes before UnregisterMouseMode
            unregisterIndex.Value < unregisterMouseModeIndex.Value |> shouldEqual true
            // UnregisterMouseMode comes before ExitAlternateScreen
            unregisterMouseModeIndex.Value < exitAltScreenIndex.Value |> shouldEqual true

            // Verify that a flush is called after all shutdown operations
            // This ensures buffered cleanup ops are written to the terminal
            let lastFlushIndex =
                opsList
                |> List.mapi (fun i op -> i, op)
                |> List.filter (fun (_, op) -> op = Flush)
                |> List.tryLast
                |> Option.map fst

            lastFlushIndex.IsSome |> shouldEqual true
            // The final flush should come after ExitAlternateScreen (the last cleanup operation)
            lastFlushIndex.Value > exitAltScreenIndex.Value |> shouldEqual true

            // Verify the final flush is the very last console action
            lastFlushIndex.Value |> shouldEqual (opsList.Length - 1)
        }

    // ============================================================
    // Time-based re-rendering tests
    // ============================================================

    [<Test>]
    let ``App.run' re-renders when time-based vdom changes`` () =
        task {
            // Track how many times vdom function is called
            let vdomCallCount = ref 0
            let capturedFrames = ConcurrentQueue<int> ()

            let console : IConsole =
                {
                    BackgroundColor = fun () -> ConsoleColor.Black
                    ForegroundColor = fun () -> ConsoleColor.White
                    WindowWidth = fun () -> 80
                    WindowHeight = fun () -> 10
                    ColorMode = ColorMode.Color
                    Execute = fun _ -> ()
                    Flush = fun () -> ()
                }

            let ctrlCHandler, _, _ = FakeCtrlCHandler.make ()
            let world = MockWorld.make ()

            let worldFreezer () =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            // Create an incremental vdom that depends on time (spinner)
            let incrVdom
                (incr : Incremental)
                (ctx : IncrVdomContext<unit>)
                (_stateNode : unit Node)
                : Vdom<DesiredBounds> Node
                =
                let clock = IncrVdomContext.clock ctx
                // 10 fps = 100ms per frame
                let frameNode = IncrTime.spinnerFrameNode incr clock LoadingSpinner.FrameCount 10.0

                incr.Map
                    (fun frame ->
                        vdomCallCount.Value <- vdomCallCount.Value + 1
                        capturedFrames.Enqueue frame
                        LoadingSpinner.make frame
                    )
                    frameNode

            let processWorld (_bridge : IWorldBridge<unit>) = WorldProcessor.passthrough

            let resolver = ActivationResolver.none

            use cts = new CancellationTokenSource ()

            let appHandle =
                App.run'
                    cts.Token
                    console
                    ctrlCHandler
                    worldFreezer
                    ()
                    (fun _ -> false)
                    processWorld
                    incrVdom
                    resolver
                    None

            // Wait for the app to be ready
            do! appHandle.Ready

            // At this point, the vdom should have been called at least once
            vdomCallCount.Value >= 1 |> shouldEqual true

            // Record initial frame count and wait a bit for time to pass
            let initialCallCount = vdomCallCount.Value

            // Wait for more than 100ms to allow at least one frame change
            do! System.Threading.Tasks.Task.Delay 150

            // The vdom should have been called again due to time advancement
            // (The main loop advances clock and marks dirty when vdom changes)
            vdomCallCount.Value > initialCallCount |> shouldEqual true

            // Verify we got different frames
            let frames = capturedFrames.ToArray ()
            frames.Length >= 2 |> shouldEqual true

            // Cancel to stop the app
            cts.Cancel ()

            do! appHandle.Finished
        }

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

            let incrState = IncrementalState.make "initial" bounds None
            let incr = incrState.Incr
            let ctx = IncrVdomContext.make<string, unit> incrState

            let mutable callCount = 0

            let pureVdom (_ctx : IVdomContext<unit>) (state : string) : Vdom<DesiredBounds> =
                callCount <- callCount + 1
                Vdom.textContent state

            let stateNode = IncrementalState.stateNode incrState
            let vdomNode = App.pureView pureVdom incr ctx stateNode

            let observer = incr.Observe vdomNode
            incr.Stabilize ()

            // Initial call
            let _ = Observer.value observer
            callCount |> shouldEqual 1

            // Change state
            IncrementalState.setState "changed" incrState
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

            let incrState = IncrementalState.make () bounds1 None
            let incr = incrState.Incr
            let ctx = IncrVdomContext.make<unit, unit> incrState

            let mutable callCount = 0

            let pureVdom (ctx : IVdomContext<unit>) (_ : unit) : Vdom<DesiredBounds> =
                callCount <- callCount + 1
                Vdom.textContent $"Width: {ctx.TerminalBounds.Width}"

            let stateNode = IncrementalState.stateNode incrState
            let vdomNode = App.pureView pureVdom incr ctx stateNode

            let observer = incr.Observe vdomNode
            incr.Stabilize ()

            // Initial call
            let _ = Observer.value observer
            callCount |> shouldEqual 1

            // Change bounds
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
            let incrState = IncrementalState.make () bounds (Some key1)
            let incr = incrState.Incr
            let ctx = IncrVdomContext.make<unit, unit> incrState

            let mutable callCount = 0

            let pureVdom (ctx : IVdomContext<unit>) (_ : unit) : Vdom<DesiredBounds> =
                callCount <- callCount + 1

                match ctx.FocusedKey with
                | Some key -> Vdom.textContent $"Focused: {key}"
                | None -> Vdom.textContent "No focus"

            let stateNode = IncrementalState.stateNode incrState
            let vdomNode = App.pureView pureVdom incr ctx stateNode

            let observer = incr.Observe vdomNode
            incr.Stabilize ()

            // Initial call
            let _ = Observer.value observer
            callCount |> shouldEqual 1

            // Change focus
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

            let incrState = IncrementalState.make "state" bounds None
            let incr = incrState.Incr
            let ctx = IncrVdomContext.make<string, unit> incrState

            let mutable callCount = 0

            let pureVdom (_ctx : IVdomContext<unit>) (state : string) : Vdom<DesiredBounds> =
                callCount <- callCount + 1
                Vdom.textContent state

            let stateNode = IncrementalState.stateNode incrState
            let vdomNode = App.pureView pureVdom incr ctx stateNode

            let observer = incr.Observe vdomNode
            incr.Stabilize ()

            // Initial call
            let _ = Observer.value observer
            callCount |> shouldEqual 1

            // Stabilize again without changing anything
            incr.Stabilize ()

            // Should not have been called again
            let _ = Observer.value observer
            callCount |> shouldEqual 1
        }
