namespace WoofWare.Zoomies.Test

open NUnit.Framework
open FsCheck
open FsUnitTyped
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
                ScrollBar.make
                    ScrollBarOrientation.Horizontal
                    {
                        TotalItems = 20
                        ViewportSize = 5
                        Offset = 0
                        TrackLength = 10
                    }

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
                ScrollBar.make
                    ScrollBarOrientation.Horizontal
                    {
                        TotalItems = 20
                        ViewportSize = 5
                        Offset = 7
                        TrackLength = 10
                    }

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
                ScrollBar.make
                    ScrollBarOrientation.Horizontal
                    {
                        TotalItems = 20
                        ViewportSize = 5
                        Offset = 15
                        TrackLength = 10
                    }

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
                ScrollBar.make
                    ScrollBarOrientation.Vertical
                    {
                        TotalItems = 20
                        ViewportSize = 5
                        Offset = 0
                        TrackLength = 10
                    }

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
                ScrollBar.make
                    ScrollBarOrientation.Vertical
                    {
                        TotalItems = 20
                        ViewportSize = 5
                        Offset = 7
                        TrackLength = 10
                    }

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
                ScrollBar.make
                    ScrollBarOrientation.Vertical
                    {
                        TotalItems = 20
                        ViewportSize = 5
                        Offset = 15
                        TrackLength = 10
                    }

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
                ScrollBar.make
                    ScrollBarOrientation.Horizontal
                    {
                        TotalItems = 5
                        ViewportSize = 10
                        Offset = 0
                        TrackLength = 10
                    }

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
                ScrollBar.make
                    ScrollBarOrientation.Horizontal
                    {
                        TotalItems = 0
                        ViewportSize = 5
                        Offset = 0
                        TrackLength = 10
                    }

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
                ScrollBar.make
                    ScrollBarOrientation.Horizontal
                    {
                        TotalItems = 20
                        ViewportSize = 5
                        Offset = 100
                        TrackLength = 10
                    }

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
                ScrollBar.make
                    ScrollBarOrientation.Horizontal
                    {
                        TotalItems = 20
                        ViewportSize = 5
                        Offset = -5
                        TrackLength = 10
                    }

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
                ScrollBar.make
                    ScrollBarOrientation.Horizontal
                    {
                        TotalItems = 10
                        ViewportSize = 8
                        Offset = 1
                        TrackLength = 10
                    }

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
                ScrollBar.make
                    ScrollBarOrientation.Horizontal
                    {
                        TotalItems = 100
                        ViewportSize = 5
                        Offset = 50
                        TrackLength = 10
                    }

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

    [<Test>]
    let ``horizontal scroll bar constrained width at start`` () =
        task {
            // Scroll bar requests track length 10, but only gets width 8
            // 10 items, viewport 2, offset 0
            // Expected: thumb adapts to actual width 8, positioned at start
            // Thumb size = max(1, int((2/10) * 8)) = max(1, 1) = 1 char
            // At offset 0: thumb at position 0
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make
                    ScrollBarOrientation.Horizontal
                    {
                        TotalItems = 10
                        ViewportSize = 2
                        Offset = 0
                        TrackLength = 10
                    }

            // Console is only 8 wide, scroll bar should adapt
            let console, terminal = ConsoleHarness.make' (fun () -> 8) (fun () -> 2)

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
█░░░░░░░|
        |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``horizontal scroll bar constrained width at end`` () =
        task {
            // Scroll bar requests track length 10, but only gets width 8
            // 10 items, viewport 2, offset 8 (max)
            // Expected: thumb adapts to actual width 8, positioned at end
            // Thumb size = 1, at max offset thumb should be at position 7
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make
                    ScrollBarOrientation.Horizontal
                    {
                        TotalItems = 10
                        ViewportSize = 2
                        Offset = 8
                        TrackLength = 10
                    }

            // Console is only 8 wide, scroll bar should adapt
            let console, terminal = ConsoleHarness.make' (fun () -> 8) (fun () -> 2)

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
░░░░░░░█|
        |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``horizontal scroll bar constrained width at middle`` () =
        task {
            // Scroll bar requests track length 10, but only gets width 8
            // 10 items, viewport 2, offset 4 (middle)
            // Expected: thumb adapts to actual width 8, positioned in middle
            // Thumb size = 1, thumbStart = (4/8) * (8-1) = 3.5 -> 3, thumb at position 3
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make
                    ScrollBarOrientation.Horizontal
                    {
                        TotalItems = 10
                        ViewportSize = 2
                        Offset = 4
                        TrackLength = 10
                    }

            // Console is only 8 wide, scroll bar should adapt
            let console, terminal = ConsoleHarness.make' (fun () -> 8) (fun () -> 2)

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
░░░█░░░░|
        |
"

                return ConsoleHarness.toString terminal
            }
        }

    /// Render a horizontal scroll bar to a string (just the scroll bar characters, no trailing spaces)
    let private renderHorizontalScrollBar (scrollParams : ScrollBarParams) : string =
        // Use max 1 to match ScrollBar clamping behaviour for non-positive track lengths
        let width = max 1 scrollParams.TrackLength
        let height = 1
        let console, terminal = ConsoleHarness.make' (fun () -> width) (fun () -> height)
        let renderState = RenderState.make console MockTime.getStaticUtcNow None

        Render.oneStep renderState () (fun () -> ScrollBar.make ScrollBarOrientation.Horizontal scrollParams)

        // Extract just the rendered row (without the trailing | from ConsoleHarness.toString)
        let fullString = ConsoleHarness.toString terminal
        // ConsoleHarness.toString returns "\n<row>|\n", so extract the row
        let lines = fullString.Split '\n'
        // lines[0] is empty (leading newline), lines[1] is "<chars>|"
        lines.[1].TrimEnd '|'

    /// Render a vertical scroll bar to a string array (one string per row)
    let private renderVerticalScrollBar (scrollParams : ScrollBarParams) : string[] =
        let width = 1
        // Use max 1 to match ScrollBar clamping behaviour for non-positive track lengths
        let height = max 1 scrollParams.TrackLength
        let console, terminal = ConsoleHarness.make' (fun () -> width) (fun () -> height)
        let renderState = RenderState.make console MockTime.getStaticUtcNow None

        Render.oneStep renderState () (fun () -> ScrollBar.make ScrollBarOrientation.Vertical scrollParams)

        // Extract the rendered column
        let fullString = ConsoleHarness.toString terminal
        let lines = fullString.Split '\n'
        // lines[0] is empty, then lines[1..height] are "<char>|"
        [| for i in 1..height -> lines.[i].TrimEnd '|' |]

    [<Test>]
    let ``property: horizontal scroll bar at offset 0 starts with thumb`` () =
        let property (viewportSize : PositiveInt) (extraItems : PositiveInt) (trackLength : PositiveInt) =
            // Construct valid inputs: totalItems > viewportSize to ensure scrollable content
            let viewportSize = viewportSize.Get
            let totalItems = viewportSize + extraItems.Get
            let trackLength = max 1 trackLength.Get

            let rendered =
                renderHorizontalScrollBar
                    {
                        TotalItems = totalItems
                        ViewportSize = viewportSize
                        Offset = 0
                        TrackLength = trackLength
                    }

            rendered.[0] |> shouldEqual '█'

        Check.One (propConfig, property)

    [<Test>]
    let ``property: horizontal scroll bar at max offset ends with thumb`` () =
        let property (viewportSize : PositiveInt) (extraItems : PositiveInt) (trackLength : PositiveInt) =
            // Construct valid inputs: totalItems > viewportSize to ensure scrollable content
            let viewportSize = viewportSize.Get
            let totalItems = viewportSize + extraItems.Get
            let trackLength = max 1 trackLength.Get
            let maxOffset = totalItems - viewportSize

            let rendered =
                renderHorizontalScrollBar
                    {
                        TotalItems = totalItems
                        ViewportSize = viewportSize
                        Offset = maxOffset
                        TrackLength = trackLength
                    }

            rendered.[rendered.Length - 1] |> shouldEqual '█'

        Check.One (propConfig, property)

    [<Test>]
    let ``property: vertical scroll bar at offset 0 starts with thumb`` () =
        let property (viewportSize : PositiveInt) (extraItems : PositiveInt) (trackLength : PositiveInt) =
            // Construct valid inputs: totalItems > viewportSize to ensure scrollable content
            let viewportSize = viewportSize.Get
            let totalItems = viewportSize + extraItems.Get
            let trackLength = max 1 trackLength.Get

            let rendered =
                renderVerticalScrollBar
                    {
                        TotalItems = totalItems
                        ViewportSize = viewportSize
                        Offset = 0
                        TrackLength = trackLength
                    }

            rendered.[0] |> shouldEqual "█"

        Check.One (propConfig, property)

    [<Test>]
    let ``property: vertical scroll bar at max offset ends with thumb`` () =
        let property (viewportSize : PositiveInt) (extraItems : PositiveInt) (trackLength : PositiveInt) =
            // Construct valid inputs: totalItems > viewportSize to ensure scrollable content
            let viewportSize = viewportSize.Get
            let totalItems = viewportSize + extraItems.Get
            let trackLength = max 1 trackLength.Get
            let maxOffset = totalItems - viewportSize

            let rendered =
                renderVerticalScrollBar
                    {
                        TotalItems = totalItems
                        ViewportSize = viewportSize
                        Offset = maxOffset
                        TrackLength = trackLength
                    }

            rendered.[rendered.Length - 1] |> shouldEqual "█"

        Check.One (propConfig, property)

    [<Test>]
    let ``property: horizontal scroll bar contains only thumb and track characters`` () =
        let property
            (viewportSize : PositiveInt)
            (totalItems : PositiveInt)
            (offset : NonNegativeInt)
            (trackLength : PositiveInt)
            =
            let rendered =
                renderHorizontalScrollBar
                    {
                        TotalItems = totalItems.Get
                        ViewportSize = viewportSize.Get
                        Offset = offset.Get
                        TrackLength = trackLength.Get
                    }

            rendered |> Seq.forall (fun c -> c = '█' || c = '░') |> shouldEqual true

        Check.One (propConfig, property)

    [<Test>]
    let ``property: horizontal scroll bar has exactly requested track length`` () =
        let property
            (viewportSize : PositiveInt)
            (totalItems : PositiveInt)
            (offset : NonNegativeInt)
            (trackLength : PositiveInt)
            =
            let trackLength = trackLength.Get

            let rendered =
                renderHorizontalScrollBar
                    {
                        TotalItems = totalItems.Get
                        ViewportSize = viewportSize.Get
                        Offset = offset.Get
                        TrackLength = trackLength
                    }

            rendered.Length |> shouldEqual trackLength

        Check.One (propConfig, property)

    [<Test>]
    let ``property: horizontal scroll bar has at least one thumb character`` () =
        let property
            (viewportSize : PositiveInt)
            (totalItems : PositiveInt)
            (offset : NonNegativeInt)
            (trackLength : PositiveInt)
            =
            let rendered =
                renderHorizontalScrollBar
                    {
                        TotalItems = totalItems.Get
                        ViewportSize = viewportSize.Get
                        Offset = offset.Get
                        TrackLength = trackLength.Get
                    }

            rendered |> Seq.exists (fun c -> c = '█') |> shouldEqual true

        Check.One (propConfig, property)

    [<Test>]
    let ``property: when viewport >= total, entire horizontal scroll bar is thumb`` () =
        let property (extraViewport : NonNegativeInt) (totalItems : PositiveInt) (trackLength : PositiveInt) =
            // Construct: viewportSize >= totalItems
            let totalItems = totalItems.Get
            let viewportSize = totalItems + extraViewport.Get
            let trackLength = trackLength.Get

            let rendered =
                renderHorizontalScrollBar
                    {
                        TotalItems = totalItems
                        ViewportSize = viewportSize
                        Offset = 0
                        TrackLength = trackLength
                    }

            rendered |> Seq.forall (fun c -> c = '█') |> shouldEqual true

        Check.One (propConfig, property)

    [<Test>]
    let ``property: thumb count is between 1 and track length inclusive`` () =
        let property
            (viewportSize : PositiveInt)
            (totalItems : PositiveInt)
            (offset : NonNegativeInt)
            (trackLength : PositiveInt)
            =
            let trackLength = trackLength.Get

            let rendered =
                renderHorizontalScrollBar
                    {
                        TotalItems = totalItems.Get
                        ViewportSize = viewportSize.Get
                        Offset = offset.Get
                        TrackLength = trackLength
                    }

            let thumbCount = rendered |> Seq.filter (fun c -> c = '█') |> Seq.length
            (thumbCount >= 1 && thumbCount <= trackLength) |> shouldEqual true

        Check.One (propConfig, property)

    [<Test>]
    let ``property: vertical scroll bar has exactly requested track length`` () =
        let property
            (viewportSize : PositiveInt)
            (totalItems : PositiveInt)
            (offset : NonNegativeInt)
            (trackLength : PositiveInt)
            =
            let trackLength = trackLength.Get

            let rendered =
                renderVerticalScrollBar
                    {
                        TotalItems = totalItems.Get
                        ViewportSize = viewportSize.Get
                        Offset = offset.Get
                        TrackLength = trackLength
                    }

            rendered.Length |> shouldEqual trackLength

        Check.One (propConfig, property)

    // Invalid input tests for clamping behaviour

    [<Test>]
    let ``scroll bar with zero viewport size is treated as 1`` () =
        task {
            // Zero viewport should be clamped to 1
            // 20 items, viewport 0 (treated as 1), offset 0, track length 10
            // Thumb size = max(1, int((1/20) * 10)) = 1
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make
                    ScrollBarOrientation.Horizontal
                    {
                        TotalItems = 20
                        ViewportSize = 0
                        Offset = 0
                        TrackLength = 10
                    }

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
█░░░░░░░░░     |
               |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``scroll bar with negative viewport size is treated as 1`` () =
        task {
            // Negative viewport should be clamped to 1
            // 20 items, viewport -5 (treated as 1), offset 10, track length 10
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make
                    ScrollBarOrientation.Horizontal
                    {
                        TotalItems = 20
                        ViewportSize = -5
                        Offset = 10
                        TrackLength = 10
                    }

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

    [<Test>]
    let ``scroll bar with zero track length is treated as 1`` () =
        task {
            // Zero track length should be clamped to 1
            // 20 items, viewport 5, offset 0, track length 0 (treated as 1)
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make
                    ScrollBarOrientation.Horizontal
                    {
                        TotalItems = 20
                        ViewportSize = 5
                        Offset = 0
                        TrackLength = 0
                    }

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
█              |
               |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``scroll bar with negative track length is treated as 1`` () =
        task {
            // Negative track length should be clamped to 1
            // 20 items, viewport 5, offset 15, track length -10 (treated as 1)
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make
                    ScrollBarOrientation.Horizontal
                    {
                        TotalItems = 20
                        ViewportSize = 5
                        Offset = 15
                        TrackLength = -10
                    }

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
█              |
               |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``vertical scroll bar with zero viewport size`` () =
        task {
            // Zero viewport in vertical orientation
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make
                    ScrollBarOrientation.Vertical
                    {
                        TotalItems = 20
                        ViewportSize = 0
                        Offset = 0
                        TrackLength = 5
                    }

            let console, terminal = ConsoleHarness.make' (fun () -> 5) (fun () -> 7)

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
    let ``vertical scroll bar with zero track length`` () =
        task {
            // Zero track length in vertical orientation
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                ScrollBar.make
                    ScrollBarOrientation.Vertical
                    {
                        TotalItems = 20
                        ViewportSize = 5
                        Offset = 0
                        TrackLength = 0
                    }

            let console, terminal = ConsoleHarness.make' (fun () -> 5) (fun () -> 3)

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
     |
     |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``property: non-positive viewport is clamped to 1`` () =
        let property (viewportNonPositive : NegativeInt) (totalItems : PositiveInt) (trackLength : PositiveInt) =
            let totalItems = totalItems.Get
            let trackLength = trackLength.Get

            let rendered =
                renderHorizontalScrollBar
                    {
                        TotalItems = totalItems
                        ViewportSize = viewportNonPositive.Get
                        Offset = 0
                        TrackLength = trackLength
                    }

            // Should render normally with effective viewport of 1
            rendered.Length |> shouldEqual trackLength
            rendered |> Seq.exists (fun c -> c = '█') |> shouldEqual true

        Check.One (propConfig, property)

    [<Test>]
    let ``property: non-positive track length is clamped to 1`` () =
        let property (viewportSize : PositiveInt) (totalItems : PositiveInt) (trackLengthNonPositive : NegativeInt) =
            let rendered =
                renderHorizontalScrollBar
                    {
                        TotalItems = totalItems.Get
                        ViewportSize = viewportSize.Get
                        Offset = 0
                        TrackLength = trackLengthNonPositive.Get
                    }

            // Should render with effective track length of 1
            rendered.Length |> shouldEqual 1
            rendered |> shouldEqual "█"

        Check.One (propConfig, property)
