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
            let terminalOps = ResizeArray<TerminalOp> ()

            // Use a very small terminal width so that after splitting, one side has 0 width
            let console =
                { IConsole.defaultForTests with
                    Execute = fun x -> terminalOps.Add x
                    WindowWidth = fun _ -> 1
                    WindowHeight = fun _ -> 5
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            // Create a vdom where text content has Width=0
            // With a terminal width of 1 and a 50/50 split, each side gets 0 or 1 width
            let vdom (_ : VdomContext) (_ : FakeUnit) =
                let leftText = Vdom.textContent "some text content"
                let rightText = Vdom.textContent "other text"
                // Split with 0.5 proportion, terminal has width 1, so left gets 0 width
                Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, leftText, rightText)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            // This should not throw an IndexOutOfRangeException
            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
            |> ignore<FakeUnit>
        }

    [<TestCase true>]
    [<TestCase false>]
    let ``Keyed text rendering handles zero-width bounds without error`` (leftIsKeyed : bool) =
        task {
            // Regression test for: "Text rendering does not handle zero-size bounds"
            // Test the keyed branch of text rendering
            let terminalOps = ResizeArray<TerminalOp> ()

            // Use a very small terminal width so that after splitting, one side has 0 width
            let console =
                { IConsole.defaultForTests with
                    Execute = fun x -> terminalOps.Add x
                    WindowWidth = fun _ -> 1
                    WindowHeight = fun _ -> 5
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            let textKey = NodeKey.make "text"

            // Create a vdom where keyed text content has Width=0
            // With a terminal width of 1 and a 50/50 split, each side gets 0 or 1 width
            let vdom (_ : VdomContext) (_ : FakeUnit) =
                let leftText = Vdom.textContent "some text content"
                let rightText = Vdom.textContent "other text"

                // Split with 0.5 proportion so that one side gets 0 width
                if leftIsKeyed then
                    Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, Vdom.withKey textKey leftText, rightText)
                else
                    Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, leftText, Vdom.withKey textKey rightText)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            // This should not throw an IndexOutOfRangeException
            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
            |> ignore<FakeUnit>
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

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // A 25-character word in a 10-character wide terminal
                // Should wrap to 3 lines: "AAAAAAAAAA" + "AAAAAAAAAA" + "AAAAA"
                let longWord = String.replicate 25 "A"
                let text = Vdom.textContent longWord

                // Put it in an auto split with another component
                let bottom = Vdom.textContent "bottom"
                Vdom.panelSplitAuto (SplitDirection.Horizontal, text, bottom)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
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

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Text with CRLF line endings - the \r should not be rendered as a visible character
                Vdom.textContent "Line1\r\nLine2\r\nLine3"

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (_, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
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

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Text with old Mac-style CR line endings - should be treated as newlines
                Vdom.textContent "Line1\rLine2\rLine3"

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (_, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
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

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Text with CRLF line endings and Centered alignment
                Vdom.textContent ("AAA\r\nBBB\r\nCCC", alignment = ContentAlignment.Centered)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (_, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
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

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Text with old Mac-style CR line endings and Centered alignment
                Vdom.textContent ("AAA\rBBB\rCCC", alignment = ContentAlignment.Centered)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (_, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
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
