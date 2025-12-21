namespace WoofWare.Zoomies.Test

open System
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies
open WoofWare.Zoomies.Components
open FsUnitTyped

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

    /// Catamorphism to extract StyledSpan list from a Vdom
    let extractSpans : VdomCata<StyledSpan list voption> =
        {
            Vdom =
                { new VdomCataCase<_, _, _> with
                    member _.Keyed x = x
                    member _.Unkeyed x = x
                }
            KeyedVdom =
                { new KeyedVdomCataCase<_, _, _> with
                    member _.WithKey _ inner = inner
                }
            UnkeyedVdom =
                { new UnkeyedVdomCataCase<_, _, _> with
                    member _.Bordered _ = ValueNone
                    member _.PanelSplit _ _ _ _ = ValueNone
                    member _.TextContent _ _ _ _ _ = ValueNone
                    member _.StyledSpans spans _ _ _ = ValueSome spans
                    member _.Focusable _ _ inner = inner
                    member _.Empty = ValueNone
                    member _.FlexibleContent _ _ = ValueNone
                    member _.Tag _ inner = inner
                }
        }

    let getSpans (vdom : Vdom<DesiredBounds>) : StyledSpan list =
        match VdomCata.run extractSpans vdom with
        | ValueSome spans -> spans
        | ValueNone -> failwith "Expected StyledSpans but got something else"

    [<Test>]
    let ``Config.Default has expected values`` () =
        let config = RainbowText.Config.Default
        config.IdleThresholdSeconds |> shouldEqual 3.0
        config.PulseWidthChars |> shouldEqual 3
        config.PulseSpeedCharsPerSec |> shouldEqual 5.0

    [<Test>]
    let ``empty string returns empty StyledSpans`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 1)

            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                RainbowText.make TimeSpan.Zero RainbowText.Config.Default ""

            let processWorld = WorldProcessor.passthrough
            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
          |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``short text renders without pulse when idle under threshold`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 1)
            let idleDuration = TimeSpan.FromSeconds 1.0 // Under 3s threshold

            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                RainbowText.make idleDuration RainbowText.Config.Default "Hello"

            let processWorld = WorldProcessor.passthrough
            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<FakeUnit>

            // Terminal harness captures just characters; colors applied via escape codes
            expect {
                snapshot
                    @"
Hello     |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``text renders with pulse when idle over threshold`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 1)
            let idleDuration = TimeSpan.FromSeconds 4.0 // Over 3s threshold

            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                RainbowText.make idleDuration RainbowText.Config.Default "Hello"

            let processWorld = WorldProcessor.passthrough
            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<FakeUnit>

            // Text still renders correctly; pulse is in colors
            expect {
                snapshot
                    @"
Hello     |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``colors cycle through ROYGBIV pattern`` () =
        // Test the actual spans generated
        let text = "ROYGBIV1"

        let spans =
            getSpans (RainbowText.make TimeSpan.Zero RainbowText.Config.Default text)

        spans.Length |> shouldEqual 8

        // Each character should have the corresponding ROYGBIV color
        let expectedColors =
            [|
                ConsoleColor.Red // R - index 0
                ConsoleColor.DarkYellow // O - index 1 (Orange)
                ConsoleColor.Yellow // Y - index 2
                ConsoleColor.Green // G - index 3
                ConsoleColor.Blue // B - index 4
                ConsoleColor.DarkBlue // I - index 5 (Indigo)
                ConsoleColor.Magenta // V - index 6 (Violet)
                ConsoleColor.Red // 1 - index 7 (wraps to 0)
            |]

        for i in 0 .. spans.Length - 1 do
            spans.[i].Style.Foreground |> shouldEqual (ValueSome expectedColors.[i])
            spans.[i].Text |> shouldEqual (string text.[i])

    [<Test>]
    let ``pulse position advances over time`` () =
        let config =
            { RainbowText.Config.Default with
                PulseSpeedCharsPerSec = 10.0 // 10 chars/sec for easy math
                PulseWidthChars = 1
            }

        // At t=3.0s (threshold), pulse at position 0
        let spans0 = getSpans (RainbowText.make (TimeSpan.FromSeconds 3.0) config "ABCDE")

        // At t=3.1s, pulse at position 1 (0.1s * 10 chars/s = 1)
        let spans1 = getSpans (RainbowText.make (TimeSpan.FromSeconds 3.1) config "ABCDE")

        // Position 0 should have light variant at t=3.0, normal at t=3.1
        // Position 1 should have normal at t=3.0, light variant at t=3.1

        // Expected ROYGBIV light variants for positions 0 and 1
        let lightRed = ConsoleColor.Red // Light variant of Red
        let lightOrange = ConsoleColor.Yellow // Light variant of DarkYellow (Orange)
        let normalRed = ConsoleColor.Red
        let normalOrange = ConsoleColor.DarkYellow

        // At t=3.0s: position 0 is pulsed
        spans0.[0].Style.Foreground |> shouldEqual (ValueSome lightRed)
        spans0.[1].Style.Foreground |> shouldEqual (ValueSome normalOrange)

        // At t=3.1s: position 1 is pulsed
        spans1.[0].Style.Foreground |> shouldEqual (ValueSome normalRed)
        spans1.[1].Style.Foreground |> shouldEqual (ValueSome lightOrange)

    [<Test>]
    let ``pulse wraps around text`` () =
        let config =
            { RainbowText.Config.Default with
                PulseSpeedCharsPerSec = 1.0
                PulseWidthChars = 1
            }

        // With 5 chars and 1 char/sec, at t=3+5=8s the pulse should be back at position 0
        let spansStart =
            getSpans (RainbowText.make (TimeSpan.FromSeconds 3.0) config "ABCDE")

        let spansWrapped =
            getSpans (RainbowText.make (TimeSpan.FromSeconds 8.0) config "ABCDE")

        // Both should have pulse at position 0
        spansStart.[0].Style.Foreground |> shouldEqual spansWrapped.[0].Style.Foreground
        spansStart.[1].Style.Foreground |> shouldEqual spansWrapped.[1].Style.Foreground

    [<Test>]
    let ``pulse width covers multiple characters`` () =
        let config =
            { RainbowText.Config.Default with
                PulseSpeedCharsPerSec = 1.0
                PulseWidthChars = 3
            }

        // At t=3.0s, pulse covers positions 0, 1, 2
        let spans = getSpans (RainbowText.make (TimeSpan.FromSeconds 3.0) config "ABCDEFG")

        // Light variants for first 3 characters
        // Expected: positions 0,1,2 have light variants; 3,4,5,6 have normal
        let lightColors =
            [|
                ConsoleColor.Red
                ConsoleColor.Yellow // Light Orange
                ConsoleColor.Yellow
            |]

        let normalColors =
            [|
                ConsoleColor.Green
                ConsoleColor.Blue
                ConsoleColor.DarkBlue
                ConsoleColor.Magenta
            |]

        for i in 0..2 do
            spans.[i].Style.Foreground |> shouldEqual (ValueSome lightColors.[i])

        for i in 3..6 do
            spans.[i].Style.Foreground |> shouldEqual (ValueSome normalColors.[i - 3])
