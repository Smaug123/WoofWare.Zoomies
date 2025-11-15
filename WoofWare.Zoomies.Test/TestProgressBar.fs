namespace WoofWare.Zoomies.Test

open System
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestProgressBar =
    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    type State = unit

    [<Test>]
    let ``progress bar at 0%`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) =
                ProgressBar.make 0.0 10
                |> Vdom.withKey (NodeKey.make "progress")

            let console, terminal = ConsoleHarness.make' (fun () -> 60) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<_, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        ProcessWorldResult.make state
                }

            let renderState = RenderState.make console None

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom
            |> ignore

            expect {
                snapshot
                    @"
[░░░░░░░░░░] 0%                                         |
                                                            |
                                                            |
                                                            |
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
    let ``progress bar at 50%`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) =
                ProgressBar.make 0.5 10
                |> Vdom.withKey (NodeKey.make "progress")

            let console, terminal = ConsoleHarness.make' (fun () -> 60) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<_, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        ProcessWorldResult.make state
                }

            let renderState = RenderState.make console None

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom
            |> ignore

            expect {
                snapshot
                    @"
[█████░░░░░] 50%                                        |
                                                            |
                                                            |
                                                            |
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
    let ``progress bar at 100%`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) =
                ProgressBar.make 1.0 10
                |> Vdom.withKey (NodeKey.make "progress")

            let console, terminal = ConsoleHarness.make' (fun () -> 60) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<_, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        ProcessWorldResult.make state
                }

            let renderState = RenderState.make console None

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom
            |> ignore

            expect {
                snapshot
                    @"
[██████████] 100%                                       |
                                                            |
                                                            |
                                                            |
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
    let ``progress bar with label`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) =
                ProgressBar.make 0.3 10 (label = "Loading:")
                |> Vdom.withKey (NodeKey.make "progress")

            let console, terminal = ConsoleHarness.make' (fun () -> 60) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<_, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        ProcessWorldResult.make state
                }

            let renderState = RenderState.make console None

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom
            |> ignore

            expect {
                snapshot
                    @"
Loading:[███░░░░░░░] 30%                                |
                                                            |
                                                            |
                                                            |
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
    let ``progress bar without percentage`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) =
                ProgressBar.make 0.7 10 (showPercentage = false)
                |> Vdom.withKey (NodeKey.make "progress")

            let console, terminal = ConsoleHarness.make' (fun () -> 60) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<_, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        ProcessWorldResult.make state
                }

            let renderState = RenderState.make console None

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom
            |> ignore

            expect {
                snapshot
                    @"
[███████░░░]                                            |
                                                            |
                                                            |
                                                            |
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
    let ``progress bar with label and no percentage`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) =
                ProgressBar.make 0.6 10 (label = "Progress:", showPercentage = false)
                |> Vdom.withKey (NodeKey.make "progress")

            let console, terminal = ConsoleHarness.make' (fun () -> 60) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<_, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        ProcessWorldResult.make state
                }

            let renderState = RenderState.make console None

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom
            |> ignore

            expect {
                snapshot
                    @"
Progress:[██████░░░░]                                   |
                                                            |
                                                            |
                                                            |
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
    let ``progress bar with varying widths`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) =
                let bar1 =
                    ProgressBar.make 0.5 5
                    |> Vdom.withKey (NodeKey.make "bar1")

                let bar2 =
                    ProgressBar.make 0.5 20
                    |> Vdom.withKey (NodeKey.make "bar2")

                Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 1, bar1, bar2)
                |> Vdom.withKey (NodeKey.make "root")

            let console, terminal = ConsoleHarness.make' (fun () -> 60) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<_, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        ProcessWorldResult.make state
                }

            let renderState = RenderState.make console None

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom
            |> ignore

            expect {
                snapshot
                    @"
[██░░░] 50%                                             |
[██████████░░░░░░░░░] 50%                               |
                                                            |
                                                            |
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
    let ``progress bar validates progress range`` () =
        Assert.Throws<ArgumentException> (fun () ->
            ProgressBar.make -0.1 10
            |> ignore
        )
        |> ignore

        Assert.Throws<ArgumentException> (fun () ->
            ProgressBar.make 1.1 10
            |> ignore
        )
        |> ignore

    [<Test>]
    let ``progress bar validates width`` () =
        Assert.Throws<ArgumentException> (fun () ->
            ProgressBar.make 0.5 0
            |> ignore
        )
        |> ignore

        Assert.Throws<ArgumentException> (fun () ->
            ProgressBar.make 0.5 -1
            |> ignore
        )
        |> ignore

    // TODO: add a test for how it renders when the numbers don't evenly divide
