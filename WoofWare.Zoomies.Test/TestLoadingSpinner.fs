namespace WoofWare.Zoomies.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Incremental
open WoofWare.TimingWheel
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestLoadingSpinner =
    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    type State = unit

    [<Test>]
    let ``spinner frame 0`` () =
        task {
            let vdom (_ : IVdomContext<_>) (_ : State) : Vdom<DesiredBounds> = LoadingSpinner.make 0

            let console, terminal = ConsoleHarness.make' (fun () -> 5) (fun () -> 1)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld = WorldProcessor.passthrough

            let renderState = MockTime.makeRenderStateStatic<unit> console None

            App.pumpOnce
                worldFreezer
                ()
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)

            expect {
                snapshot
                    @"
⠋    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``spinner frame 5`` () =
        task {
            let vdom (_ : IVdomContext<_>) (_ : State) : Vdom<DesiredBounds> = LoadingSpinner.make 5

            let console, terminal = ConsoleHarness.make' (fun () -> 5) (fun () -> 1)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld = WorldProcessor.passthrough

            let renderState = MockTime.makeRenderStateStatic<unit> console None

            App.pumpOnce
                worldFreezer
                ()
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)

            expect {
                snapshot
                    @"
⠴    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``spinner frame wraps at FrameCount`` () =
        task {
            // Frame 10 should equal Frame 0
            let vdom (_ : IVdomContext<_>) (_ : State) : Vdom<DesiredBounds> =
                LoadingSpinner.make LoadingSpinner.FrameCount

            let console, terminal = ConsoleHarness.make' (fun () -> 5) (fun () -> 1)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld = WorldProcessor.passthrough

            let renderState = MockTime.makeRenderStateStatic<unit> console None

            App.pumpOnce
                worldFreezer
                ()
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)

            expect {
                snapshot
                    @"
⠋    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``spinner handles negative frame`` () =
        task {
            // Frame -1 should equal Frame 9 (last frame)
            let vdom (_ : IVdomContext<_>) (_ : State) : Vdom<DesiredBounds> = LoadingSpinner.make -1

            let console, terminal = ConsoleHarness.make' (fun () -> 5) (fun () -> 1)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld = WorldProcessor.passthrough

            let renderState = MockTime.makeRenderStateStatic<unit> console None

            App.pumpOnce
                worldFreezer
                ()
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)

            expect {
                snapshot
                    @"
⠏    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``makeIncr produces vdom that updates with clock`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // Create the incremental spinner node
            let spinnerNode = LoadingSpinner.makeIncr incr clock 10.0

            let observer = incr.Observe spinnerNode
            incr.Stabilize ()

            // Get the initial vdom
            let vdom0 = Observer.value observer

            // Advance clock by 100ms (one frame at 10fps)
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 100_000_000L)
            incr.Stabilize ()

            let vdom1 = Observer.value observer

            // The vdom should have changed (different reference)
            Object.ReferenceEquals (vdom0, vdom1) |> shouldEqual false
        }

    [<Test>]
    let ``makeIncr renders different frames as clock advances`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 5) (fun () -> 1)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false
            let processWorld = WorldProcessor.passthrough

            // Create incremental state with clock starting at epoch
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // Create the spinner node (10 fps = 100ms per frame)
            let spinnerNode = LoadingSpinner.makeIncr incr clock 10.0

            // Create persistent observer
            let spinnerObserver = incr.Observe spinnerNode
            incr.Stabilize ()

            // Capture the vdoms we pass to rendering for diagnostic purposes
            let capturedVdoms = ResizeArray<Vdom<DesiredBounds>> ()

            let vdom (_ : IVdomContext<_>) (_ : State) : Vdom<DesiredBounds> =
                let v = Observer.value spinnerObserver
                capturedVdoms.Add v
                v

            let renderState = MockTime.makeRenderStateStatic<unit> console None

            // Render at frame 0
            App.pumpOnce
                worldFreezer
                ()
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore

            // Verify vdom function was called
            capturedVdoms.Count |> shouldEqual 1

            expect {
                snapshot
                    @"
⠋    |
"

                return ConsoleHarness.toString terminal
            }

            // Advance clock by 100ms (one frame at 10fps) and re-render
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 100_000_000L)
            incr.Stabilize ()

            // Verify observer has new value BEFORE calling pumpOnce
            let vdomBeforePump = Observer.value spinnerObserver
            Object.ReferenceEquals (capturedVdoms.[0], vdomBeforePump) |> shouldEqual false // Should be different vdom

            // IMPORTANT: In tests that bypass run' and use pumpOnce directly with external
            // incremental state, we must manually mark the context dirty after advancing the clock.
            // The real run' function handles this automatically by checking if the vdom observer
            // changed after stabilization.
            let ctx = RenderState.vdomContext renderState
            VdomContext.markDirty ctx

            App.pumpOnce
                worldFreezer
                ()
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore

            // Verify vdom function was called again
            capturedVdoms.Count |> shouldEqual 2

            // Verify the two vdoms are different
            Object.ReferenceEquals (capturedVdoms.[0], capturedVdoms.[1])
            |> shouldEqual false

            expect {
                snapshot
                    @"
⠙    |
"

                return ConsoleHarness.toString terminal
            }
        }
