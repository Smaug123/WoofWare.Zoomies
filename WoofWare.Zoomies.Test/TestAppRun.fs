namespace WoofWare.Zoomies.Test

open System
open System.Collections.Concurrent
open System.Threading
open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies

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
                    clock.GetUtcNow
                    ctrlCHandler
                    worldFreezer
                    ()
                    (fun _ -> false)
                    processWorld
                    vdom
                    resolver
                    None
                    0

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
