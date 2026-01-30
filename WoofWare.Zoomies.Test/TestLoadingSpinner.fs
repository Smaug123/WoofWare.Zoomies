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

    [<Test>]
    let ``spinner frame 0`` () =
        task {
            let vdom (_ : IVdomContext<_>) (_ : unit) : Vdom<DesiredBounds> = LoadingSpinner.make 0

            let console, terminal = ConsoleHarness.make' (fun () -> 5) (fun () -> 1)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let config = TestConfig.passthrough<unit> vdom

            use ctx = IncrTestContext.make console config None

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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
            let vdom (_ : IVdomContext<_>) (_ : unit) : Vdom<DesiredBounds> = LoadingSpinner.make 5

            let console, terminal = ConsoleHarness.make' (fun () -> 5) (fun () -> 1)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let config = TestConfig.passthrough<unit> vdom

            use ctx = IncrTestContext.make console config None

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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
            let vdom (_ : IVdomContext<_>) (_ : unit) : Vdom<DesiredBounds> =
                LoadingSpinner.make LoadingSpinner.FrameCount

            let console, terminal = ConsoleHarness.make' (fun () -> 5) (fun () -> 1)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let config = TestConfig.passthrough<unit> vdom

            use ctx = IncrTestContext.make console config None

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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
            let vdom (_ : IVdomContext<_>) (_ : unit) : Vdom<DesiredBounds> = LoadingSpinner.make -1

            let console, terminal = ConsoleHarness.make' (fun () -> 5) (fun () -> 1)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let config = TestConfig.passthrough<unit> vdom

            use ctx = IncrTestContext.make console config None

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            // Capture the vdoms we pass to rendering for diagnostic purposes
            let capturedVdoms = ResizeArray<Vdom<DesiredBounds>> ()

            let vdom (ctx : VdomContext<unit>) (_stateNode : unit Node) : Vdom<DesiredBounds> Node =
                let incr = VdomContext.incr ctx
                let timeNode = VdomContext.clockTimeNode ctx
                let spinnerNode = LoadingSpinner.makeIncrWithTimeNode incr timeNode 10.0

                incr.Map
                    (fun v ->
                        capturedVdoms.Add v
                        v
                    )
                    spinnerNode

            let config : AppConfig<unit, unit, unit> =
                AppConfig.simple () (fun s _ -> s) vdom ActivationResolver.none

            use ctx = IncrTestContext.make console config None

            // Render at frame 0
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // Verify vdom function was called
            capturedVdoms.Count |> shouldEqual 1

            expect {
                snapshot
                    @"
⠋    |
"

                return ConsoleHarness.toString terminal
            }

            // Advance time by 100ms (one frame at 10fps) and re-render
            // The pumpOnce will advance the clock and stabilize, which should cause
            // the spinner observer to have a new value.
            IncrTestContext.advanceTime (TimeSpan.FromMilliseconds 100.0) ctx

            // Verify observer has new value BEFORE calling pumpOnce
            // (after stabilization which happens inside pumpOnce)
            let vdom0 = capturedVdoms.[0]

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // Verify vdom function was called again
            capturedVdoms.Count |> shouldEqual 2

            // Verify the two vdoms are different
            Object.ReferenceEquals (vdom0, capturedVdoms.[1]) |> shouldEqual false

            expect {
                snapshot
                    @"
⠙    |
"

                return ConsoleHarness.toString terminal
            }
        }
