namespace WoofWare.Zoomies.Test

open System
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStyledSpans =

    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    [<Test>]
    let ``StyledSpans renders text correctly`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 1)

            let vdom (_ : IVdomContext<_>) (_ : unit) =
                Vdom.styledSpans (
                    [
                        {
                            Text = "He"
                            Style = CellStyle.none
                        }
                        {
                            Text = "llo"
                            Style = CellStyle.none
                        }
                    ]
                )

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
Hello     |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``StyledSpans renders with colors`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 1)

            let vdom (_ : IVdomContext<_>) (_ : unit) =
                Vdom.styledSpans (
                    [
                        {
                            Text = "R"
                            Style =
                                CellStyle.none
                                |> CellStyle.withForeground (Color.fromConsoleColor ConsoleColor.Red)
                        }
                        {
                            Text = "G"
                            Style =
                                CellStyle.none
                                |> CellStyle.withForeground (Color.fromConsoleColor ConsoleColor.Green)
                        }
                        {
                            Text = "B"
                            Style =
                                CellStyle.none
                                |> CellStyle.withForeground (Color.fromConsoleColor ConsoleColor.Blue)
                        }
                    ]
                )

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

            // The terminal harness captures just characters; color is applied via escape codes
            // which the harness strips. So we just verify the text renders correctly.
            expect {
                snapshot
                    @"
RGB       |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``StyledSpans wraps long text`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 5) (fun () -> 3)

            let vdom (_ : IVdomContext<_>) (_ : unit) =
                Vdom.styledSpans (
                    [
                        {
                            Text = "Hello"
                            Style = CellStyle.none
                        }
                        {
                            Text = "World"
                            Style = CellStyle.none
                        }
                    ],
                    wrap = true
                )

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
Hello|
World|
     |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``StyledSpans handles empty spans`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 5) (fun () -> 1)

            let vdom (_ : IVdomContext<_>) (_ : unit) = Vdom.styledSpans []

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
     |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``StyledSpans truncates when wrap is false`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 5) (fun () -> 2)

            let vdom (_ : IVdomContext<_>) (_ : unit) =
                Vdom.styledSpans (
                    [
                        {
                            Text = "HelloWorld"
                            Style = CellStyle.none
                        }
                    ],
                    wrap = false
                )

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
Hello|
     |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``StyledSpans handles CRLF split across span boundaries`` () =
        // This tests a specific edge case: when \r\n is split across two spans,
        // it should be treated as a single newline (matching Layout.fs measurement).
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 3)

            let vdom (_ : IVdomContext<_>) (_ : unit) =
                Vdom.styledSpans (
                    [
                        {
                            Text = "Hello\r" // Span ends with \r
                            Style = CellStyle.none
                        }
                        {
                            Text = "\nWorld" // Span starts with \n
                            Style = CellStyle.none
                        }
                    ]
                )

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

            // Should render as two lines (Hello\nWorld), not three (Hello\n\nWorld)
            expect {
                snapshot
                    @"
Hello     |
World     |
          |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``StyledSpans with centered alignment`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 10) (fun () -> 3)

            let vdom (_ : IVdomContext<_>) (_ : unit) =
                Vdom.styledSpans (
                    [
                        {
                            Text = "Hi"
                            Style = CellStyle.none
                        }
                    ],
                    alignment = ContentAlignment.Centered
                )

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
          |
    Hi    |
          |
"

                return ConsoleHarness.toString terminal
            }
        }
