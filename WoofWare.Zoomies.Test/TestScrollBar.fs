namespace WoofWare.Zoomies.Test

open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestScrollBar =
    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    type State = unit

    [<Test>]
    let ``horizontal scroll bar at start`` () =
        task {
            // 20 items, viewport of 5, offset 0, track length 10
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make ScrollBarOrientation.Horizontal 20 5 0 10

            let console, terminal = ConsoleHarness.make' (fun () -> 15) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

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
██░░░░░░░░     |
               |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``horizontal scroll bar at middle`` () =
        task {
            // 20 items, viewport of 5, offset 7 (roughly middle), track length 10
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make ScrollBarOrientation.Horizontal 20 5 7 10

            let console, terminal = ConsoleHarness.make' (fun () -> 15) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

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
░░░██░░░░░     |
               |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``horizontal scroll bar at end`` () =
        task {
            // 20 items, viewport of 5, offset 15 (max), track length 10
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make ScrollBarOrientation.Horizontal 20 5 15 10

            let console, terminal = ConsoleHarness.make' (fun () -> 15) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

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
░░░░░░░░██     |
               |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``vertical scroll bar at start`` () =
        task {
            // 20 items, viewport of 5, offset 0, track length 10
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make ScrollBarOrientation.Vertical 20 5 0 10

            let console, terminal = ConsoleHarness.make' (fun () -> 5) (fun () -> 12)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

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
█    |
█    |
░    |
░    |
░    |
░    |
░    |
░    |
░    |
░    |
     |
     |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``vertical scroll bar at middle`` () =
        task {
            // 20 items, viewport of 5, offset 7, track length 10
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make ScrollBarOrientation.Vertical 20 5 7 10

            let console, terminal = ConsoleHarness.make' (fun () -> 5) (fun () -> 12)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

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
░    |
░    |
░    |
█    |
█    |
░    |
░    |
░    |
░    |
░    |
     |
     |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``vertical scroll bar at end`` () =
        task {
            // 20 items, viewport of 5, offset 15, track length 10
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make ScrollBarOrientation.Vertical 20 5 15 10

            let console, terminal = ConsoleHarness.make' (fun () -> 5) (fun () -> 12)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

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
░    |
░    |
░    |
░    |
░    |
░    |
░    |
░    |
█    |
█    |
     |
     |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``scroll bar when all content visible`` () =
        task {
            // 5 items, viewport of 10 (larger than content), track length 10
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make ScrollBarOrientation.Horizontal 5 10 0 10

            let console, terminal = ConsoleHarness.make' (fun () -> 15) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

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
██████████     |
               |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``scroll bar with zero total items`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make ScrollBarOrientation.Horizontal 0 5 0 10

            let console, terminal = ConsoleHarness.make' (fun () -> 15) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

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
░░░░░░░░░░     |
               |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``scroll bar with offset beyond valid range`` () =
        task {
            // 20 items, viewport of 5, offset 100 (way beyond), track length 10
            // Should clamp to max valid offset (15)
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make ScrollBarOrientation.Horizontal 20 5 100 10

            let console, terminal = ConsoleHarness.make' (fun () -> 15) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

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
░░░░░░░░██     |
               |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``scroll bar with negative offset`` () =
        task {
            // Should clamp to 0
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make ScrollBarOrientation.Horizontal 20 5 -5 10

            let console, terminal = ConsoleHarness.make' (fun () -> 15) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

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
██░░░░░░░░     |
               |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``scroll bar with large viewport relative to total`` () =
        task {
            // 10 items, viewport of 8, offset 1, track length 10
            // Thumb should be large (80% of track)
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make ScrollBarOrientation.Horizontal 10 8 1 10

            let console, terminal = ConsoleHarness.make' (fun () -> 15) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

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
░████████░     |
               |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``scroll bar with small viewport relative to total`` () =
        task {
            // 100 items, viewport of 5, offset 50, track length 10
            // Thumb should be small (5% of track, minimum 1)
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make ScrollBarOrientation.Horizontal 100 5 50 10

            let console, terminal = ConsoleHarness.make' (fun () -> 15) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

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
░░░░█░░░░░     |
               |
"

                return ConsoleHarness.toString terminal
            }
        }
