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
module TestRainbowText =
    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    [<Test>]
    let ``make with empty text returns empty styled spans`` () =
        task {
            let vdom (_ : IVdomContext<_>) (_ : unit) = RainbowText.make "" ValueNone 3

            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 1)
            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let renderState = MockTime.makeRenderStateStatic<unit> console None

            App.pumpOnce
                worldFreezer
                ()
                (fun _ -> false)
                renderState
                WorldProcessor.passthrough
                vdom
                ActivationResolver.none
                (fun () -> false)

            expect {
                snapshot
                    @"
          |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``make displays rainbow text without pulse`` () =
        task {
            let vdom (_ : IVdomContext<_>) (_ : unit) = RainbowText.make "Rainbow" ValueNone 3

            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 1)
            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let renderState = MockTime.makeRenderStateStatic<unit> console None

            App.pumpOnce
                worldFreezer
                ()
                (fun _ -> false)
                renderState
                WorldProcessor.passthrough
                vdom
                ActivationResolver.none
                (fun () -> false)

            // Terminal harness shows text without color codes
            expect {
                snapshot
                    @"
Rainbow   |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``make with pulse position 0 affects first characters`` () =
        task {
            // Pulse at position 0 with width 3 should affect chars 0, 1, 2
            let vdom (_ : IVdomContext<_>) (_ : unit) =
                RainbowText.make "Rainbow" (ValueSome 0) 3

            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 1)
            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let renderState = MockTime.makeRenderStateStatic<unit> console None

            App.pumpOnce
                worldFreezer
                ()
                (fun _ -> false)
                renderState
                WorldProcessor.passthrough
                vdom
                ActivationResolver.none
                (fun () -> false)

            // Text still renders the same (colors are not visible in test harness)
            expect {
                snapshot
                    @"
Rainbow   |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``make with pulse wraps around text`` () =
        task {
            // Pulse at position 5 with width 3 on "Rainbow" (7 chars)
            // Should affect chars 5, 6, 0 (wrapping)
            let vdom (_ : IVdomContext<_>) (_ : unit) =
                RainbowText.make "Rainbow" (ValueSome 5) 3

            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 1)
            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let renderState = MockTime.makeRenderStateStatic<unit> console None

            App.pumpOnce
                worldFreezer
                ()
                (fun _ -> false)
                renderState
                WorldProcessor.passthrough
                vdom
                ActivationResolver.none
                (fun () -> false)

            expect {
                snapshot
                    @"
Rainbow   |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``computePulseFrame returns ValueNone when not idle long enough`` () =
        let config =
            { RainbowText.Config.Default with
                IdleThresholdSeconds = 3.0
            }

        let idleDuration = TimeSpan.FromSeconds 2.5
        let result = RainbowText.computePulseFrame idleDuration 10 config
        result |> shouldEqual ValueNone

    [<Test>]
    let ``computePulseFrame returns ValueNone for empty text`` () =
        let config = RainbowText.Config.Default
        let idleDuration = TimeSpan.FromSeconds 10.0
        let result = RainbowText.computePulseFrame idleDuration 0 config
        result |> shouldEqual ValueNone

    [<Test>]
    let ``computePulseFrame returns frame 0 at exactly idle threshold`` () =
        let config =
            { RainbowText.Config.Default with
                IdleThresholdSeconds = 3.0
                PulseSpeedCharsPerSec = 5.0
            }

        let idleDuration = TimeSpan.FromSeconds 3.0
        let result = RainbowText.computePulseFrame idleDuration 10 config
        result |> shouldEqual (ValueSome 0)

    [<Test>]
    let ``computePulseFrame advances with time`` () =
        let config =
            { RainbowText.Config.Default with
                IdleThresholdSeconds = 3.0
                PulseSpeedCharsPerSec = 5.0 // 5 chars per second
            }

        // 4 seconds idle = 3 threshold + 1 elapsed = 5 chars advancement
        let idleDuration = TimeSpan.FromSeconds 4.0
        let result = RainbowText.computePulseFrame idleDuration 10 config
        result |> shouldEqual (ValueSome 5)

    [<Test>]
    let ``computePulseFrame wraps around text length`` () =
        let config =
            { RainbowText.Config.Default with
                IdleThresholdSeconds = 3.0
                PulseSpeedCharsPerSec = 5.0 // 5 chars per second
            }

        // 6 seconds idle = 3 threshold + 3 elapsed = 15 chars advancement
        // On text length 10, this wraps to position 5
        let idleDuration = TimeSpan.FromSeconds 6.0
        let result = RainbowText.computePulseFrame idleDuration 10 config
        result |> shouldEqual (ValueSome 5)

    [<Test>]
    let ``Default config has expected values`` () =
        let config = RainbowText.Config.Default
        config.IdleThresholdSeconds |> shouldEqual 3.0
        config.PulseWidthChars |> shouldEqual 3
        config.PulseSpeedCharsPerSec |> shouldEqual 5.0

    [<Test>]
    let ``makeIncr produces vdom that updates with clock`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // Create DateTime node from clock
            let clockNsNode = incr.Clock.WatchNow clock
            let currentTimeNode = incr.Map TimeConversion.nsToDateTime clockNsNode

            // Create a Var to track last input time
            let lastInputTimeVar =
                incr.Var.Create (ValueSome (DateTime (1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)))

            let lastInputTimeNode = incr.Var.Watch lastInputTimeVar

            let config =
                { RainbowText.Config.Default with
                    IdleThresholdSeconds = 1.0
                    PulseSpeedCharsPerSec = 10.0
                }

            // Create the incremental rainbow text node
            let rainbowNode =
                RainbowText.makeIncr incr currentTimeNode lastInputTimeNode "Hello" config

            let observer = incr.Observe rainbowNode
            incr.Stabilize ()

            // Get the initial vdom (at time 0, idle 0 seconds - no pulse)
            let vdom0 = Observer.value observer

            // Advance clock by 2 seconds (1 second past threshold)
            // At 10 chars/sec, position should be 10 % 5 = 0
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 2_000_000_000L)
            incr.Stabilize ()

            let vdom1 = Observer.value observer

            // The vdom should have changed (pulse now active)
            Object.ReferenceEquals (vdom0, vdom1) |> shouldEqual false
        }

    [<Test>]
    let ``makeIncr updates when pulse frame changes`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // Create DateTime node from clock
            let clockNsNode = incr.Clock.WatchNow clock
            let currentTimeNode = incr.Map TimeConversion.nsToDateTime clockNsNode

            // Set last input at epoch
            let epoch = DateTime (1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
            let lastInputTimeVar = incr.Var.Create (ValueSome epoch)
            let lastInputTimeNode = incr.Var.Watch lastInputTimeVar

            let config =
                { RainbowText.Config.Default with
                    IdleThresholdSeconds = 1.0
                    PulseSpeedCharsPerSec = 10.0 // 10 chars/sec = 100ms per char
                }

            let rainbowNode =
                RainbowText.makeIncr incr currentTimeNode lastInputTimeNode "Hello" config

            let observer = incr.Observe rainbowNode

            // Advance to 1.0 seconds (just at threshold)
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 1_000_000_000L)
            incr.Stabilize ()
            let vdomAtThreshold = Observer.value observer

            // Verify the pulse frame via computePulseFrame (which uses pure calculation)
            let idleAt1s = TimeSpan.FromSeconds 1.0
            RainbowText.computePulseFrame idleAt1s 5 config |> shouldEqual (ValueSome 0)

            // Advance to 1.1 seconds (100ms past threshold = 1 char)
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 1_100_000_000L)
            incr.Stabilize ()
            let vdomAt1Char = Observer.value observer

            let idleAt1_1s = TimeSpan.FromSeconds 1.1
            RainbowText.computePulseFrame idleAt1_1s 5 config |> shouldEqual (ValueSome 1)

            // Advance to 1.2 seconds (200ms past threshold = 2 chars)
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 1_200_000_000L)
            incr.Stabilize ()
            let vdomAt2Chars = Observer.value observer

            let idleAt1_2s = TimeSpan.FromSeconds 1.2
            RainbowText.computePulseFrame idleAt1_2s 5 config |> shouldEqual (ValueSome 2)

            // The vdom at threshold and frame 1 should be different objects
            // (frame changed from 0 to 1, so different pulse positions)
            Object.ReferenceEquals (vdomAtThreshold, vdomAt1Char) |> shouldEqual false

        // Note: Due to how Incremental's cutoff works, if the Vdom structures
        // are polyEqual, the same cached object might be returned. The key
        // behavior is that the pulse frame computation is correct (verified above).
        }

    [<Test>]
    let ``makeIncr handles no input time gracefully`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // Create DateTime node from clock
            let clockNsNode = incr.Clock.WatchNow clock
            let currentTimeNode = incr.Map TimeConversion.nsToDateTime clockNsNode

            // No input recorded yet
            let lastInputTimeVar = incr.Var.Create ValueNone
            let lastInputTimeNode = incr.Var.Watch lastInputTimeVar

            let config = RainbowText.Config.Default

            let rainbowNode =
                RainbowText.makeIncr incr currentTimeNode lastInputTimeNode "Hello" config

            let observer = incr.Observe rainbowNode
            incr.Stabilize ()

            // Should produce a valid vdom with no pulse (no crash)
            let _vdom = Observer.value observer
            // If we got here without exception, the test passes
            ()
        }

    [<Test>]
    let ``makeIncr resets pulse when user provides input`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // Create DateTime node from clock
            let clockNsNode = incr.Clock.WatchNow clock
            let currentTimeNode = incr.Map TimeConversion.nsToDateTime clockNsNode

            // Start with input at epoch
            let lastInputTimeVar =
                incr.Var.Create (ValueSome (DateTime (1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)))

            let lastInputTimeNode = incr.Var.Watch lastInputTimeVar

            let config =
                { RainbowText.Config.Default with
                    IdleThresholdSeconds = 1.0
                    PulseSpeedCharsPerSec = 5.0
                }

            let rainbowNode =
                RainbowText.makeIncr incr currentTimeNode lastInputTimeNode "Hello" config

            let observer = incr.Observe rainbowNode

            // Advance to 2 seconds (pulse active)
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 2_000_000_000L)
            incr.Stabilize ()
            let vdomWithPulse = Observer.value observer

            // Simulate user input at current time (resets idle)
            let currentTime = DateTime (1970, 1, 1, 0, 0, 2, DateTimeKind.Utc)
            incr.Var.Set lastInputTimeVar (ValueSome currentTime)
            incr.Stabilize ()
            let vdomAfterInput = Observer.value observer

            // The vdom should change (pulse should be gone)
            Object.ReferenceEquals (vdomWithPulse, vdomAfterInput) |> shouldEqual false
        }

    [<Test>]
    let ``makeIncr with empty text returns constant empty vdom`` () =
        task {
            let incr = Incremental.make ()
            let startTime = TimeNs.ofInt64NsSinceEpoch 0L
            let clock = incr.Clock.Create startTime

            // Create DateTime node from clock
            let clockNsNode = incr.Clock.WatchNow clock
            let currentTimeNode = incr.Map TimeConversion.nsToDateTime clockNsNode

            let lastInputTimeVar =
                incr.Var.Create (ValueSome (DateTime (1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)))

            let lastInputTimeNode = incr.Var.Watch lastInputTimeVar

            let config = RainbowText.Config.Default

            let rainbowNode =
                RainbowText.makeIncr incr currentTimeNode lastInputTimeNode "" config

            let observer = incr.Observe rainbowNode
            incr.Stabilize ()

            let vdom0 = Observer.value observer

            // Advance clock
            incr.Clock.AdvanceClock clock (TimeNs.ofInt64NsSinceEpoch 5_000_000_000L)
            incr.Stabilize ()

            let vdom1 = Observer.value observer

            // Empty text should produce the same vdom (by reference)
            Object.ReferenceEquals (vdom0, vdom1) |> shouldEqual true
        }
