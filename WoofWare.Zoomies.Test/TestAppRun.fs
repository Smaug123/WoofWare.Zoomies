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

    [<Test>]
    let ``App.run' registers bracketed paste on startup and unregisters on quit`` () =
        task {
            let ops = ConcurrentQueue<TerminalOp> ()

            let console : IConsole =
                {
                    BackgroundColor = fun () -> ConsoleColor.Black
                    ForegroundColor = fun () -> ConsoleColor.White
                    WindowWidth = fun () -> 80
                    WindowHeight = fun () -> 10
                    ColorMode = ColorMode.Color
                    Execute = fun op -> ops.Enqueue op
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

            let vdom (_ctx : VdomContext) (_state : unit) : Vdom<DesiredBounds> = Vdom.empty

            let processWorld (_bridge : IWorldBridge<unit>) =
                { new WorldProcessor<unit, unit> with
                    member _.ProcessWorld (_inputs, _renderState, state) = ProcessWorldResult.make state
                }

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

            // Wait for the app to be ready (initial setup and first render complete)
            do! appHandle.Ready

            // Cancel to trigger quit
            cts.Cancel ()

            // Wait for the app to finish
            do! appHandle.Finished

            // Check that we got the expected operations
            let opsList = ops.ToArray () |> Array.toList

            // RegisterBracketedPaste should appear early (after EnterAlternateScreen, RegisterMouseMode)
            let registerIndex =
                opsList |> List.tryFindIndex (fun op -> op = TerminalOp.RegisterBracketedPaste)

            let unregisterIndex =
                opsList
                |> List.tryFindIndex (fun op -> op = TerminalOp.UnregisterBracketedPaste)

            registerIndex.IsSome |> shouldEqual true
            unregisterIndex.IsSome |> shouldEqual true

            // Unregister should come after register
            unregisterIndex.Value > registerIndex.Value |> shouldEqual true

            // Verify order: Register should come after EnterAlternateScreen
            let enterAltScreenIndex =
                opsList |> List.tryFindIndex (fun op -> op = TerminalOp.EnterAlternateScreen)

            enterAltScreenIndex.IsSome |> shouldEqual true
            registerIndex.Value > enterAltScreenIndex.Value |> shouldEqual true

            // Verify order: Unregister should come before ExitAlternateScreen
            let exitAltScreenIndex =
                opsList |> List.tryFindIndex (fun op -> op = TerminalOp.ExitAlternateScreen)

            exitAltScreenIndex.IsSome |> shouldEqual true
            unregisterIndex.Value < exitAltScreenIndex.Value |> shouldEqual true
        }
