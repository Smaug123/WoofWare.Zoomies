namespace WoofWare.Zoomies.Test

open NUnit.Framework
open WoofWare.Expect
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

            use worldFreezer =
                WorldFreezer.listen' UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

            let world = MockWorld.attach worldFreezer

            let haveFrameworkHandleFocus _ = false

            let processWorld = WorldProcessor.passthrough

            let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None

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

            use worldFreezer =
                WorldFreezer.listen' UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

            let world = MockWorld.attach worldFreezer

            let haveFrameworkHandleFocus _ = false

            let processWorld = WorldProcessor.passthrough

            let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None

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

            use worldFreezer =
                WorldFreezer.listen' UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

            let world = MockWorld.attach worldFreezer

            let haveFrameworkHandleFocus _ = false

            let processWorld = WorldProcessor.passthrough

            let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None

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

            use worldFreezer =
                WorldFreezer.listen' UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

            let world = MockWorld.attach worldFreezer

            let haveFrameworkHandleFocus _ = false

            let processWorld = WorldProcessor.passthrough

            let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None

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
