namespace WoofWare.Zoomies.Test

open System
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTextRendering =
    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    [<Test>]
    let ``Text rendering handles zero-width bounds without error`` () =
        task {
            // Regression test for: "Text rendering does not handle zero-size bounds"
            // With Width=0 (from a proportion split), rendering can write off-bounds and throw
            // Using width=1, a 0.5 split gives left=floor(0.5*1)=0 width, right=1 width
            let console, terminal = ConsoleHarness.make' (fun () -> 1) (fun () -> 5)

            // Create a vdom where text content has Width=0
            // With a terminal width of 1 and a 50/50 split, left gets 0 width, right gets 1 width
            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                let leftText = Vdom.textContent "some text content"
                let rightText = Vdom.textContent "other text"
                // Split with 0.5 proportion, terminal has width 1, so left gets 0 width
                Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, leftText, rightText)

            let processWorld = WorldProcessor.passthrough

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let renderState = MockTime.makeRenderState console MockTime.getStaticUtcNow None

            // This should not throw an IndexOutOfRangeException
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

            // Assert: only right side content is visible (proving left has 0 width)
            // If layout ever enforced a minimum width, this snapshot would change
            expect {
                snapshot
                    @"
o|
t|
h|
e|
r|
"

                return ConsoleHarness.toString terminal
            }
        }

    [<TestCase true>]
    [<TestCase false>]
    let ``Keyed text rendering handles zero-width bounds without error`` (leftIsKeyed : bool) =
        task {
            // Regression test for: "Text rendering does not handle zero-size bounds"
            // Test the keyed branch of text rendering
            // Using width=1, a 0.5 split gives left=floor(0.5*1)=0 width, right=1 width
            let console, terminal = ConsoleHarness.make' (fun () -> 1) (fun () -> 5)

            let textKey = NodeKey.make "text"

            // Create a vdom where keyed text content has Width=0
            // With a terminal width of 1 and a 50/50 split, left gets 0 width, right gets 1 width
            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                let leftText = Vdom.textContent "some text content"
                let rightText = Vdom.textContent "other text"

                // Split with 0.5 proportion so that left side gets 0 width
                if leftIsKeyed then
                    Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, Vdom.withKey textKey leftText, rightText)
                else
                    Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, leftText, Vdom.withKey textKey rightText)

            let processWorld = WorldProcessor.passthrough

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let renderState = MockTime.makeRenderState console MockTime.getStaticUtcNow None

            // This should not throw an IndexOutOfRangeException
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

            // Assert: only right side content is visible (proving left has 0 width)
            // If layout ever enforced a minimum width, this snapshot would change
            // The snapshot is the same regardless of which side is keyed
            expect {
                snapshot
                    @"
o|
t|
h|
e|
r|
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``regression test for wordWrapCount underestimating height for long words that exceed width`` () =
        task {
            // Test case: a single long word that exceeds the available width
            // wordWrapCount treats it as 1 line (places the whole word on one line)
            // But rendering wraps character-by-character, so it actually takes multiple lines
            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                // A 25-character word in a 10-character wide terminal
                // Should wrap to 3 lines: "AAAAAAAAAA" + "AAAAAAAAAA" + "AAAAA"
                let longWord = String.replicate 25 "A"
                let text = Vdom.textContent longWord

                // Put it in an auto split with another component
                let bottom = Vdom.textContent "bottom"
                Vdom.panelSplitAuto (SplitDirection.Horizontal, text, bottom)

            let processWorld = WorldProcessor.passthrough

            let renderState =
                MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

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
AAAAAAAAAA|
AAAAAAAAAA|
AAAAA     |
bottom    |
          |
          |
          |
          |
          |
          |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``CRLF line endings are normalized to LF in TopLeft alignment`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                // Text with CRLF line endings - the \r should not be rendered as a visible character
                Vdom.textContent "Line1\r\nLine2\r\nLine3"

            let processWorld = WorldProcessor.passthrough

            let renderState =
                MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

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
Line1               |
Line2               |
Line3               |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``standalone CR is normalized to LF in TopLeft alignment`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                // Text with old Mac-style CR line endings - should be treated as newlines
                Vdom.textContent "Line1\rLine2\rLine3"

            let processWorld = WorldProcessor.passthrough

            let renderState =
                MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

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
Line1               |
Line2               |
Line3               |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``CRLF line endings are normalized to LF in Centered alignment`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                // Text with CRLF line endings and Centered alignment
                Vdom.textContent ("AAA\r\nBBB\r\nCCC", alignment = ContentAlignment.Centered)

            let processWorld = WorldProcessor.passthrough

            let renderState =
                MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

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
        AAA         |
        BBB         |
        CCC         |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``standalone CR is normalized to LF in Centered alignment`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                // Text with old Mac-style CR line endings and Centered alignment
                Vdom.textContent ("AAA\rBBB\rCCC", alignment = ContentAlignment.Centered)

            let processWorld = WorldProcessor.passthrough

            let renderState =
                MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

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
        AAA         |
        BBB         |
        CCC         |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<TestCase("\n")>]
    [<TestCase("\r")>]
    [<TestCase("\r\n")>]
    let ``wordWrapCount correctly counts explicit newlines for layout measurement`` (newline : string) =
        task {
            // Regression test: wordWrapCount was splitting on \n as a word separator
            // but not incrementing line count for explicit newlines.
            // This caused text with multiple lines to be measured as needing only 1 line,
            // leading to truncation in auto-split layouts.
            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                // Multi-line text that should be measured as needing 5 lines
                let multiLineText =
                    String.concat newline [ "Line 1" ; "Line 2" ; "Line 3" ; "Line 4" ; "Line 5" ]

                let text = Vdom.textContent multiLineText

                // Put it in an auto split with another component
                // If measurement is correct, text gets 5 lines, footer gets 1 line
                let footer = Vdom.textContent "Footer"
                Vdom.panelSplitAuto (SplitDirection.Horizontal, text, footer)

            let processWorld = WorldProcessor.passthrough

            let renderState =
                MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

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

            // All 5 lines of text should be visible, plus the footer
            expect {
                snapshot
                    @"
Line 1              |
Line 2              |
Line 3              |
Line 4              |
Line 5              |
Footer              |
                    |
                    |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``wordWrapCount correctly counts empty lines from consecutive newlines`` () =
        task {
            // Test that blank lines (from \n\n) are counted correctly
            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 8)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                // Text with blank lines - should be measured as 5 lines total
                // (Para1, blank, Para2, blank, Para3)
                let textWithBlanks = "Para1\n\nPara2\n\nPara3"
                let text = Vdom.textContent textWithBlanks

                let footer = Vdom.textContent "Footer"
                Vdom.panelSplitAuto (SplitDirection.Horizontal, text, footer)

            let processWorld = WorldProcessor.passthrough

            let renderState =
                MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

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
Para1               |
                    |
Para2               |
                    |
Para3               |
Footer              |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``text with wrap=true wraps to next line when exceeding width`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                // Text that exceeds width - with wrap=true (default), it wraps
                Vdom.textContent ("Hello World, this is a long text", wrap = true)

            let processWorld = WorldProcessor.passthrough

            let renderState =
                MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

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
Hello Worl|
d, this is|
 a long te|
xt        |
          |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``text with wrap=false truncates at width boundary`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                // Text that exceeds width - with wrap=false, it truncates
                Vdom.textContent ("Hello World, this is a long text", wrap = false)

            let processWorld = WorldProcessor.passthrough

            let renderState =
                MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

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
Hello Worl|
          |
          |
          |
          |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``text with wrap=false and explicit newlines respects newlines`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                // Multi-line text with wrap=false - each line truncates independently
                Vdom.textContent ("First line is long\nSecond is too\nShort", wrap = false)

            let processWorld = WorldProcessor.passthrough

            let renderState =
                MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

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
First line|
Second is |
Short     |
          |
          |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``wrap=false affects layout measurement - text requests only 1 line height`` () =
        task {
            // This test demonstrates that wrap=false affects the layout measurement,
            // not just rendering. With wrap=false, text that would normally wrap
            // to multiple lines now only requests 1 line of height.
            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 6)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                // Long text with wrap=false - should only take 1 line in auto layout
                let text =
                    Vdom.textContent ("This is a very long text that would wrap", wrap = false)

                let footer = Vdom.textContent "Footer"
                Vdom.panelSplitAuto (SplitDirection.Horizontal, text, footer)

            let processWorld = WorldProcessor.passthrough

            let renderState =
                MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

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

            // The truncated text takes only 1 line, footer takes 1 line
            expect {
                snapshot
                    @"
This is a |
Footer    |
          |
          |
          |
          |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``wrap=true vs wrap=false comparison in auto layout`` () =
        task {
            // Compare wrap=true vs wrap=false side by side to show the difference
            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 6)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                // Left side: wrap=true, Right side: wrap=false
                let longText = "Long text here"
                let leftText = Vdom.textContent (longText, wrap = true)
                let rightText = Vdom.textContent (longText, wrap = false)
                Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, leftText, rightText)

            let processWorld = WorldProcessor.passthrough

            let renderState =
                MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

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

            // Left side wraps (10 chars wide), right side truncates (10 chars wide)
            expect {
                snapshot
                    @"
Long text Long text |
here                |
                    |
                    |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``centered text with wrap=true wraps long lines and centers each wrapped line`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 6)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                // Long text that exceeds width - with wrap=true and Centered alignment
                // "ABCDEFGHIJKLMNOPQRSTUVWXYZ" is 26 chars, width is 20
                // Should wrap to: "ABCDEFGHIJKLMNOPQRST" (20 chars, centered = offset 0)
                //                 "UVWXYZ" (6 chars, centered = offset 7)
                Vdom.textContent ("ABCDEFGHIJKLMNOPQRSTUVWXYZ", alignment = ContentAlignment.Centered, wrap = true)

            let processWorld = WorldProcessor.passthrough

            let renderState =
                MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

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

            // First line fills width (no centering offset), second line is centered
            expect {
                snapshot
                    @"
                    |
                    |
ABCDEFGHIJKLMNOPQRST|
       UVWXYZ       |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``centered text with wrap=false truncates long lines`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                // Long text with wrap=false and Centered alignment - should truncate
                Vdom.textContent ("ABCDEFGHIJKLMNOPQRSTUVWXYZ", alignment = ContentAlignment.Centered, wrap = false)

            let processWorld = WorldProcessor.passthrough

            let renderState =
                MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

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

            // Text is truncated at width boundary (left portion shown)
            expect {
                snapshot
                    @"
                    |
                    |
ABCDEFGHIJKLMNOPQRST|
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``centered text with wrap=true handles multiple lines with some needing wrapping`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 12) (fun () -> 7)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : IVdomContext<_>) (_ : FakeUnit) =
                // Multi-line text where some lines need wrapping
                // Line 1: "Short" (5 chars) - fits, centered
                // Line 2: "This is too long" (16 chars) - wraps to "This is too " (12) + "long" (4)
                // Line 3: "End" (3 chars) - fits, centered
                Vdom.textContent ("Short\nThis is too long\nEnd", alignment = ContentAlignment.Centered, wrap = true)

            let processWorld = WorldProcessor.passthrough

            let renderState =
                MockTime.makeRenderState<unit> console MockTime.getStaticUtcNow None

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

            // 4 lines of content (Short, "This is too ", "long", End), centered in 7 lines
            // startY = (7 - 4 + 1) / 2 = 2
            expect {
                snapshot
                    @"
            |
            |
   Short    |
This is too |
    long    |
    End     |
            |
"

                return ConsoleHarness.toString terminal
            }
        }
