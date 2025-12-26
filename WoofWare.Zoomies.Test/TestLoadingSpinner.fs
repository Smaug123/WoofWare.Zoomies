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
    let ``spinnerFrameNode starts at frame 0 when clock is at epoch`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // 10 fps means each frame lasts 100ms = 100_000_000 ns
            let frameNode = IncrTime.spinnerFrameNode incr clock LoadingSpinner.FrameCount 10.0

            let observer = incr.Observe frameNode
            incr.Stabilize ()

            let frame = Observer.value observer
            frame |> shouldEqual 0
        }

    [<Test>]
    let ``spinnerFrameNode advances frame when clock advances`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // 10 fps means each frame lasts 100ms = 100_000_000 ns
            let frameNode = IncrTime.spinnerFrameNode incr clock LoadingSpinner.FrameCount 10.0

            let observer = incr.Observe frameNode
            incr.Stabilize ()

            // Initially at frame 0
            Observer.value observer |> shouldEqual 0

            // Advance clock by 100ms (one frame at 10fps)
            let nsPerFrame = 100_000_000L // 100ms in ns
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch nsPerFrame)
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 1

            // Advance by another frame
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch (2L * nsPerFrame))
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 2
        }

    [<Test>]
    let ``spinnerFrameNode wraps around after all frames`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // 10 fps means each frame lasts 100ms = 100_000_000 ns
            let frameNode = IncrTime.spinnerFrameNode incr clock LoadingSpinner.FrameCount 10.0

            let observer = incr.Observe frameNode
            incr.Stabilize ()

            // Advance clock by exactly FrameCount frames (full cycle)
            let nsPerFrame = 100_000_000L
            let fullCycleNs = int64 LoadingSpinner.FrameCount * nsPerFrame

            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch fullCycleNs)
            incr.Stabilize ()

            // Should be back at frame 0
            Observer.value observer |> shouldEqual 0
        }

    [<Test>]
    let ``spinnerFrameNode at different fps values`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // 5 fps means each frame lasts 200ms = 200_000_000 ns
            let frameNode = IncrTime.spinnerFrameNode incr clock LoadingSpinner.FrameCount 5.0

            let observer = incr.Observe frameNode
            incr.Stabilize ()

            // At 100ms (half a frame at 5fps), should still be at frame 0
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 100_000_000L)
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 0

            // At 200ms (one full frame at 5fps), should be at frame 1
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 200_000_000L)
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 1
        }

    [<Test>]
    let ``spinnerFrameNode only propagates when frame changes`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // 10 fps means each frame lasts 100ms
            let frameNode = IncrTime.spinnerFrameNode incr clock LoadingSpinner.FrameCount 10.0

            // Track how many times the frame value changes
            let mutable previousFrame = -1
            let mutable changeCount = 0

            let observer = incr.Observe frameNode
            incr.Stabilize ()

            let checkAndCount () =
                let current = Observer.value observer

                if current <> previousFrame then
                    changeCount <- changeCount + 1
                    previousFrame <- current

            checkAndCount () // Initial
            changeCount |> shouldEqual 1

            // Advance by 10ms - still in frame 0
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 10_000_000L)
            incr.Stabilize ()
            checkAndCount ()
            changeCount |> shouldEqual 1 // No change

            // Advance to frame 1 (100ms total)
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 100_000_000L)
            incr.Stabilize ()
            checkAndCount ()
            changeCount |> shouldEqual 2 // Changed to frame 1
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

    // ============================================================
    // IncrTime validation tests
    // ============================================================

    [<Test>]
    let ``spinnerFrameNode throws ArgumentException for zero frameCount`` () =
        let incr = Incremental.make ()
        let clock = incr.Clock.Create (TimeNs.ofInt64NsSinceEpoch 0L)

        Assert.Throws<System.ArgumentException> (fun () -> IncrTime.spinnerFrameNode incr clock 0 10.0 |> ignore)
        |> ignore

    [<Test>]
    let ``spinnerFrameNode throws ArgumentException for negative frameCount`` () =
        let incr = Incremental.make ()
        let clock = incr.Clock.Create (TimeNs.ofInt64NsSinceEpoch 0L)

        Assert.Throws<System.ArgumentException> (fun () -> IncrTime.spinnerFrameNode incr clock -5 10.0 |> ignore)
        |> ignore

    [<Test>]
    let ``spinnerFrameNode throws ArgumentException for zero fps`` () =
        let incr = Incremental.make ()
        let clock = incr.Clock.Create (TimeNs.ofInt64NsSinceEpoch 0L)

        Assert.Throws<System.ArgumentException> (fun () -> IncrTime.spinnerFrameNode incr clock 10 0.0 |> ignore)
        |> ignore

    [<Test>]
    let ``spinnerFrameNode throws ArgumentException for negative fps`` () =
        let incr = Incremental.make ()
        let clock = incr.Clock.Create (TimeNs.ofInt64NsSinceEpoch 0L)

        Assert.Throws<System.ArgumentException> (fun () -> IncrTime.spinnerFrameNode incr clock 10 -5.0 |> ignore)
        |> ignore

    // ============================================================
    // periodicTickNode tests
    // ============================================================

    [<Test>]
    let ``periodicTickNode starts at tick 0`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            let tickNode = IncrTime.periodicTickNode incr clock (TimeSpan.FromSeconds 1.0)

            let observer = incr.Observe tickNode
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 0L
        }

    [<Test>]
    let ``periodicTickNode increments when interval elapses`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // 1 second interval = 1_000_000_000 ns
            let tickNode = IncrTime.periodicTickNode incr clock (TimeSpan.FromSeconds 1.0)

            let observer = incr.Observe tickNode
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 0L

            // Advance by 1 second
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 1_000_000_000L)
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 1L

            // Advance by another second (2 seconds total)
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 2_000_000_000L)
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 2L
        }

    [<Test>]
    let ``periodicTickNode does not increment within interval`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // 1 second interval
            let tickNode = IncrTime.periodicTickNode incr clock (TimeSpan.FromSeconds 1.0)

            let observer = incr.Observe tickNode
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 0L

            // Advance by 500ms (half an interval)
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 500_000_000L)
            incr.Stabilize ()

            // Still at tick 0
            Observer.value observer |> shouldEqual 0L

            // Advance by another 500ms (1 second total)
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 1_000_000_000L)
            incr.Stabilize ()

            // Now at tick 1
            Observer.value observer |> shouldEqual 1L
        }

    [<Test>]
    let ``periodicTickNode handles millisecond intervals`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // 100ms interval = 100_000_000 ns
            let tickNode =
                IncrTime.periodicTickNode incr clock (TimeSpan.FromMilliseconds 100.0)

            let observer = incr.Observe tickNode
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 0L

            // Advance by 100ms
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 100_000_000L)
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 1L

            // Advance by 1 second (10 more ticks at 100ms interval)
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 1_100_000_000L)
            incr.Stabilize ()

            Observer.value observer |> shouldEqual 11L
        }

    [<Test>]
    let ``periodicTickNode only propagates when tick changes`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // 1 second interval
            let tickNode = IncrTime.periodicTickNode incr clock (TimeSpan.FromSeconds 1.0)

            let mutable previousTick = -1L
            let mutable changeCount = 0

            let observer = incr.Observe tickNode
            incr.Stabilize ()

            let checkAndCount () =
                let current = Observer.value observer

                if current <> previousTick then
                    changeCount <- changeCount + 1
                    previousTick <- current

            checkAndCount () // Initial
            changeCount |> shouldEqual 1

            // Advance by 100ms - still in tick 0
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 100_000_000L)
            incr.Stabilize ()
            checkAndCount ()
            changeCount |> shouldEqual 1 // No change

            // Advance to tick 1 (1 second total)
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 1_000_000_000L)
            incr.Stabilize ()
            checkAndCount ()
            changeCount |> shouldEqual 2 // Changed to tick 1
        }
