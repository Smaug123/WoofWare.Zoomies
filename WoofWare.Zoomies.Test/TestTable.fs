namespace WoofWare.Zoomies.Test

open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies
open WoofWare.Zoomies.Components
open FsUnitTyped

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTable =
    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    type State = unit

    [<Test>]
    let ``empty table`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> = Table.makeAuto []

            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 5)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
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
    let ``simple 2x2 auto table`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.makeAuto
                    [
                        [ Vdom.textContent false "A1" ; Vdom.textContent false "B1" ]
                        [ Vdom.textContent false "A2" ; Vdom.textContent false "B2" ]
                    ]

            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 5)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
A1B1                |
A2B2                |
                    |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``3x2 auto table with different cell widths`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.makeAuto
                    [
                        [ Vdom.textContent false "Name" ; Vdom.textContent false "Age" ]
                        [ Vdom.textContent false "Alice" ; Vdom.textContent false "30" ]
                        [ Vdom.textContent false "Bob" ; Vdom.textContent false "25" ]
                    ]

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 5)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
Name Age                      |
Alice30                       |
Bob  25                       |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``table with fixed column widths`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.make
                    [
                        [ Vdom.textContent false "A" ; Vdom.textContent false "B" ]
                        [ Vdom.textContent false "1" ; Vdom.textContent false "2" ]
                    ]
                    [ FixedColumn 5 ; FixedColumn 10 ]
                    []

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 5)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
A    B                        |
1    2                        |
                              |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``table with proportion columns`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.make
                    [
                        [ Vdom.textContent false "Left" ; Vdom.textContent false "Right" ]
                        [ Vdom.textContent false "A" ; Vdom.textContent false "B" ]
                    ]
                    [ ProportionColumn 0.7 ; ProportionColumn 0.3 ]
                    []

            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 5)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
Left        Right   |
A           B       |
                    |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``ragged rows are padded with empty`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.makeAuto
                    [
                        [
                            Vdom.textContent false "A"
                            Vdom.textContent false "B"
                            Vdom.textContent false "C"
                        ]
                        [ Vdom.textContent false "1" ]
                        [ Vdom.textContent false "X" ; Vdom.textContent false "Y" ]
                    ]

            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 5)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
ABC                 |
1                   |
XY                  |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``single cell table`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.makeAuto [ [ Vdom.textContent false "Single" ] ]

            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 5)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
Single              |
                    |
                    |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``table with fixed row heights`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.make
                    [
                        [ Vdom.textContent false "Row1" ]
                        [ Vdom.textContent false "Row2" ]
                        [ Vdom.textContent false "Row3" ]
                    ]
                    []
                    [ FixedRow 2 ; FixedRow 1 ; FixedRow 2 ]

            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 10)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
Row1                |
                    |
Row2                |
Row3                |
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
    let ``table with proportion rows`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.make
                    [
                        [ Vdom.textContent false "Top" ]
                        [ Vdom.textContent false "Middle" ]
                        [ Vdom.textContent false "Bottom" ]
                    ]
                    []
                    [ ProportionRow 0.5 ; ProportionRow 0.3 ; ProportionRow 0.2 ]

            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 10)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
Top                 |
                    |
                    |
                    |
                    |
Middle              |
                    |
                    |
Bottom              |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``table with mixed row specs`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.make
                    [
                        [ Vdom.textContent false "Fixed2" ]
                        [ Vdom.textContent false "Auto" ]
                        [ Vdom.textContent false "Prop" ]
                    ]
                    []
                    [ FixedRow 2 ; AutoRow ; ProportionRow 1.0 ]

            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 10)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
Fixed2              |
                    |
Auto                |
Prop                |
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
    let ``table with simple keyed cells`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                // Table internally assigns keys to cells based on (row, col) position
                // This test verifies that tables handle cell content correctly
                Table.makeAuto [ [ Vdom.textContent false "Cell1" ] ; [ Vdom.textContent false "Cell2" ] ]

            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 5)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
Cell1               |
Cell2               |
                    |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    // TEST EXPECTATION WAS WRONG: Expected FixedColumn 15 to span 25 chars (absorbing slack), but it should only span 15 chars
    let ``table with multiline text wrapping in cells`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.make
                    [
                        [
                            Vdom.textContent false "Short"
                            Vdom.textContent false "This is a longer text that should wrap"
                        ]
                        [ Vdom.textContent false "X" ; Vdom.textContent false "Y" ]
                    ]
                    [ FixedColumn 5 ; FixedColumn 15 ]
                    []

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 5)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
ShortThis is a longe          |
     r text that sho          |
     uld wrap                 |
                              |
X    Y                        |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``table constrained by terminal size`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.makeAuto
                    [
                        [
                            Vdom.textContent false "VeryLongColumnName1"
                            Vdom.textContent false "VeryLongColumnName2"
                        ]
                        [ Vdom.textContent false "Data1" ; Vdom.textContent false "Data2" ]
                    ]

            // Very small terminal
            let console, terminal = ConsoleHarness.make' (fun () -> 15) (fun () -> 5)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
VeryLongVeryLon|
ColumnNagColumn|
me1     Name2  |
Data1   Data2  |
               |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``negative proportions are sanitized to Auto`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.make
                    [
                        [ Vdom.textContent false "A" ; Vdom.textContent false "B" ]
                        [ Vdom.textContent false "1" ; Vdom.textContent false "2" ]
                    ]
                    [ ProportionColumn -0.5 ; ProportionColumn 1.0 ]
                    []

            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 5)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
AB                  |
12                  |
                    |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``NaN and Infinity proportions are sanitized to Auto`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.make
                    [
                        [
                            Vdom.textContent false "Col1"
                            Vdom.textContent false "Col2"
                            Vdom.textContent false "C3"
                        ]
                    ]
                    [
                        ProportionColumn System.Double.NaN
                        ProportionColumn System.Double.PositiveInfinity
                        AutoColumn
                    ]
                    [ ProportionRow System.Double.NegativeInfinity ]

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 5)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
Col1Col2C3                    |
                              |
                              |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``fewer column specs than columns pads with Auto`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.make
                    [
                        [
                            Vdom.textContent false "A"
                            Vdom.textContent false "B"
                            Vdom.textContent false "C"
                        ]
                        [
                            Vdom.textContent false "1"
                            Vdom.textContent false "22"
                            Vdom.textContent false "3"
                        ]
                    ]
                    [ FixedColumn 5 ] // Only 1 spec for 3 columns
                    []

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 5)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
A    B C                      |
1    223                      |
                              |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``more column specs than columns truncates extras`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.make
                    [ [ Vdom.textContent false "X" ; Vdom.textContent false "Y" ] ]
                    [ FixedColumn 3 ; FixedColumn 4 ; FixedColumn 10 ; AutoColumn ] // 4 specs for 2 columns
                    []

            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 5)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
X  Y                |
                    |
                    |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``zero proportion total divides space equally`` () =
        task {
            // This shouldn't happen in practice after sanitization, but test the allocation logic
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.make
                    [
                        [
                            Vdom.textContent false "A"
                            Vdom.textContent false "B"
                            Vdom.textContent false "C"
                        ]
                    ]
                    [ ProportionColumn 0.0 ; ProportionColumn 0.0 ; ProportionColumn 0.0 ]
                    []

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 5)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            // After sanitization all become Auto, so should size to content
            expect {
                snapshot
                    @"
ABC                           |
                              |
                              |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``mixed column specs Auto Fixed and Proportion`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.make
                    [
                        [
                            Vdom.textContent false "AutoCol"
                            Vdom.textContent false "Fix"
                            Vdom.textContent false "Prop"
                        ]
                        [
                            Vdom.textContent false "X"
                            Vdom.textContent false "Y"
                            Vdom.textContent false "Z"
                        ]
                    ]
                    [ AutoColumn ; FixedColumn 8 ; ProportionColumn 1.0 ]
                    []

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 5)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
AutoColFix     Prop           |
X      Y       Z              |
                              |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``over-constrained columns trigger shrink-to-fit`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.make
                    [
                        [
                            Vdom.textContent false "VeryLongColumn1"
                            Vdom.textContent false "VeryLongColumn2"
                            Vdom.textContent false "VeryLongColumn3"
                        ]
                    ]
                    [ AutoColumn ; AutoColumn ; AutoColumn ]
                    []

            // Terminal only 20 chars wide, but content wants 45 chars
            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 5)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
VeryLonVeryLonVeryLo|
gColumngColumnngColu|
1      2      mn3   |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``over-constrained rows trigger shrink-to-fit`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.make
                    [
                        [ Vdom.textContent false "Row1" ]
                        [ Vdom.textContent false "Row2" ]
                        [ Vdom.textContent false "Row3" ]
                        [ Vdom.textContent false "Row4" ]
                        [ Vdom.textContent false "Row5" ]
                    ]
                    []
                    [ AutoRow ; AutoRow ; AutoRow ; AutoRow ; AutoRow ]

            // Terminal only 3 lines high, but content wants 5 lines
            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 3)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
Row1                |
Row2                |
Row3                |
"

                return ConsoleHarness.toString terminal
            }
        }

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTableMeasurements =
    type private Folded =
        {
            Run : Rectangle -> int option
        }

    let private findAllocatedWidth
        (targetKey : string)
        (bounds : Rectangle)
        (vdom : Vdom<DesiredBounds, 'keyed>)
        : int option
        =
        let matchKey (key : NodeKey) (child : Folded) : Folded =
            let target = NodeKey.toString key

            {
                Run =
                    fun rect ->
                        if target = targetKey then
                            Some rect.Width
                        else
                            child.Run rect
            }

        let cata : VdomCata<Folded> =
            {
                KeylessVdom =
                    { new KeylessVdomCataCase<Folded, Folded, Folded> with
                        member _.Keyed v = v
                        member _.Unkeyed v = v
                    }
                KeyedVdom =
                    { new KeyedVdomCataCase<Folded, Folded, Folded> with
                        member _.WithKey key child = matchKey key child
                    }
                UnkeyedVdom =
                    { new UnkeyedVdomCataCase<Folded, Folded, Folded> with
                        member _.Bordered child = child

                        member _.PanelSplit direction behaviour child1 child2 =
                            match direction, behaviour with
                            | SplitDirection.Vertical, SplitBehaviour.Absolute p ->
                                {
                                    Run =
                                        fun rect ->
                                            let allocated = abs p

                                            let leftWidth, rightWidth =
                                                if p >= 0 then
                                                    allocated, max 0 (rect.Width - allocated)
                                                else
                                                    max 0 (rect.Width - allocated), allocated

                                            let leftBounds =
                                                { rect with
                                                    Width = leftWidth
                                                }

                                            let rightBounds =
                                                { rect with
                                                    Width = rightWidth
                                                }

                                            child1.Run leftBounds |> Option.orElse (child2.Run rightBounds)
                                }
                            | SplitDirection.Horizontal, SplitBehaviour.Absolute p ->
                                {
                                    Run =
                                        fun rect ->
                                            let allocated = abs p

                                            let topHeight, bottomHeight =
                                                if p >= 0 then
                                                    allocated, max 0 (rect.Height - allocated)
                                                else
                                                    max 0 (rect.Height - allocated), allocated

                                            let topBounds =
                                                { rect with
                                                    Height = topHeight
                                                }

                                            let bottomBounds =
                                                { rect with
                                                    TopLeftY = rect.TopLeftY + topHeight
                                                    Height = bottomHeight
                                                }

                                            child1.Run topBounds |> Option.orElse (child2.Run bottomBounds)
                                }
                            | _ ->
                                // Non-absolute splits share the same bounds for our key search
                                {
                                    Run = fun rect -> child1.Run rect |> Option.orElse (child2.Run rect)
                                }

                        member _.TextContent _content _style _alignment _focused =
                            {
                                Run = fun _ -> None
                            }

                        member _.Focusable _isFirstToFocus _isInitiallyFocused child = child

                        member _.Empty =
                            {
                                Run = fun _ -> None
                            }

                        member _.FlexibleContent _measure render =
                            {
                                Run = fun rect -> (render rect).Run rect
                            }

                        member _.Tag _tag inner = inner
                    }
            }

        VdomCata.run cata vdom |> fun folded -> folded.Run bounds

    [<Test>]
    let ``table MinWidth equals sum of column minimums`` () =
        // Create cells with known MinWidth values
        let cell1 = Vdom.textContent false "Short" // MinWidth = 5 (longest word)
        let cell2 = Vdom.textContent false "VeryLongWord" // MinWidth = 12
        let cell3 = Vdom.textContent false "A B C" // MinWidth = 1

        let table = Table.makeAuto [ [ cell1 ; cell2 ; cell3 ] ]

        let constraints =
            {
                MaxWidth = 1000
                MaxHeight = 1000
            }

        let measured = Vdom.measure table constraints

        // Sum of column minimums: 5 + 12 + 1 = 18
        measured.MinWidth |> shouldEqual 18

    [<Test>]
    let ``table MinWidth with FixedColumn uses fixed width`` () =
        let cell1 = Vdom.textContent false "Short" // MinWidth = 5
        let cell2 = Vdom.textContent false "X" // MinWidth = 1

        let table = Table.make [ [ cell1 ; cell2 ] ] [ FixedColumn 10 ; AutoColumn ] []

        let constraints =
            {
                MaxWidth = 1000
                MaxHeight = 1000
            }

        let measured = Vdom.measure table constraints

        // FixedColumn contributes its fixed size to MinWidth: 10 + 1 = 11
        measured.MinWidth |> shouldEqual 11

    [<Test>]
    let ``table MinWidth with ProportionColumn uses cell minimum`` () =
        let cell1 = Vdom.textContent false "Hello" // MinWidth = 5
        let cell2 = Vdom.textContent false "World" // MinWidth = 5

        let table =
            Table.make [ [ cell1 ; cell2 ] ] [ ProportionColumn 0.5 ; ProportionColumn 0.5 ] []

        let constraints =
            {
                MaxWidth = 1000
                MaxHeight = 1000
            }

        let measured = Vdom.measure table constraints

        // Even proportion columns must report child minima: 5 + 5 = 10
        measured.MinWidth |> shouldEqual 10

    [<Test>]
    let ``table MinHeightForWidth uses cell MinHeightForWidth`` () =
        // Create a cell with text that wraps
        let cell =
            Vdom.textContent false "This is a long text that will wrap when constrained"

        let table = Table.makeAuto [ [ cell ] ]

        let constraints =
            {
                MaxWidth = 1000
                MaxHeight = 1000
            }

        let measured = Vdom.measure table constraints

        // Get the cell's MinHeightForWidth at a specific width
        let cellMeasured = Vdom.measure cell constraints
        let cellMinHeight = cellMeasured.MinHeightForWidth 10

        // Table's MinHeightForWidth should match
        let tableMinHeight = measured.MinHeightForWidth 10
        tableMinHeight |> shouldEqual cellMinHeight

    [<Test>]
    let ``table PreferredHeightForWidth uses cell PreferredHeightForWidth`` () =
        let cell =
            Vdom.textContent false "This is text that wraps differently based on width"

        let table = Table.makeAuto [ [ cell ] ]

        let constraints =
            {
                MaxWidth = 1000
                MaxHeight = 1000
            }

        let measured = Vdom.measure table constraints

        let cellMeasured = Vdom.measure cell constraints
        let cellPreferredHeight = cellMeasured.PreferredHeightForWidth 15

        let tablePreferredHeight = measured.PreferredHeightForWidth 15
        tablePreferredHeight |> shouldEqual cellPreferredHeight

    [<Test>]
    let ``table with multiple rows uses max height per row`` () =
        // Row 1: two cells with different heights when wrapped
        let cell1 = Vdom.textContent false "Short"

        let cell2 =
            Vdom.textContent false "This is a much longer text that will wrap to multiple lines"

        // Row 2: both short
        let cell3 = Vdom.textContent false "A"
        let cell4 = Vdom.textContent false "B"

        let table = Table.makeAuto [ [ cell1 ; cell2 ] ; [ cell3 ; cell4 ] ]

        let constraints =
            {
                MaxWidth = 1000
                MaxHeight = 1000
            }

        let measured = Vdom.measure table constraints

        // At width 20 per column, measure individual cells
        let cell1Meas = Vdom.measure cell1 constraints
        let cell2Meas = Vdom.measure cell2 constraints
        let cell3Meas = Vdom.measure cell3 constraints
        let cell4Meas = Vdom.measure cell4 constraints

        // Row 1 height = max(cell1 height at allocated width, cell2 height at allocated width)
        // Row 2 height = max(cell3 height, cell4 height)
        let row1Height =
            max (cell1Meas.PreferredHeightForWidth 5) (cell2Meas.PreferredHeightForWidth 59)

        let row2Height =
            max (cell3Meas.PreferredHeightForWidth 5) (cell4Meas.PreferredHeightForWidth 59)

        let expectedTotalHeight = row1Height + row2Height
        let tableHeight = measured.PreferredHeightForWidth 64 // 5 + 59 = 64 total width

        tableHeight |> shouldEqual expectedTotalHeight

    [<Test>]
    let ``proportion column gets at least its minimum width when table allocated MinWidth`` () =
        task {
            // This test exposes a bug: proportion columns can be allocated 0 width
            // even when the parent honors the table's MinWidth

            // Create a table with:
            // - AutoColumn with VERY LARGE preferred width but SMALL min width (wrappable text)
            // - ProportionColumn with non-zero minimum width
            let autoCell =
                Vdom.textContent false "Some long text with many words that wraps nicely" // Preferred ~50, MinWidth ~6 ("nicely")

            let propCell = Vdom.textContent false "PropCol" // Preferred 7, MinWidth 7 (single word)

            let table =
                Table.make [ [ autoCell ; propCell ] ] [ AutoColumn ; ProportionColumn 1.0 ] []

            let constraints =
                {
                    MaxWidth = 1000
                    MaxHeight = 1000
                }

            let measured = Vdom.measure table constraints

            // Table should report MinWidth = sum of column mins (~6 + 7 = ~13)
            let tableMinWidth = measured.MinWidth
            // MinWidth should be approximately 13 (longest word in each column)
            (tableMinWidth >= 10 && tableMinWidth <= 15) |> shouldEqual true

            // Now render the table at exactly its MinWidth
            // When allocated exactly MinWidth, BOTH columns should get at least their minimums
            let console, terminal = ConsoleHarness.make' (fun () -> tableMinWidth) (fun () -> 5)
            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> =
                Table.make [ [ autoCell ; propCell ] ] [ AutoColumn ; ProportionColumn 1.0 ] []

            let processWorld =
                { new WorldProcessor<unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                Unchecked.defaultof<State>
                (fun _ -> false)
                renderState
                processWorld
                vdom
                ActivationResolver.none
            |> ignore

            let output = ConsoleHarness.toString terminal

            // The proportion column should display its content without being truncated
            // If it gets 0 width or less than MinWidth, "PropCol" won't appear completely
            // This assertion will FAIL with the current bug - the proportion column gets 0 width
            output |> shouldContainText "PropCol"
        }

    [<Test>]
    let ``last column keeps its allocated width even with slack`` () =
        let table =
            Table.make
                [ [ Vdom.textContent false "L" ; Vdom.textContent false "R" ] ]
                [ FixedColumn 3 ; FixedColumn 4 ]
                []

        // Warm the measurement cache so render reuses the precomputed column widths
        let constraints =
            {
                MaxWidth = 1000
                MaxHeight = 1000
            }

        Vdom.measure table constraints |> ignore

        let bounds =
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = 20 // Wider than sum of allocated widths to expose the slack bug
                Height = 3
            }

        let lastWidth = findAllocatedWidth "cell_0_1" bounds table

        lastWidth |> shouldEqual (Some 4)

    [<Test>]
    let ``single cell row uses its computed column width`` () =
        let cell = Vdom.textContent false "Hello"
        let table = Table.makeAuto [ [ cell ] ]

        let constraints =
            {
                MaxWidth = 1000
                MaxHeight = 1000
            }

        // The allocated column width should match the cell's preferred width for an Auto column
        let expectedWidth = (Vdom.measure cell constraints).PreferredWidth

        Vdom.measure table constraints |> ignore

        let bounds =
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = expectedWidth + 10 // Leave slack to expose the width propagation bug
                Height = 3
            }

        let width = findAllocatedWidth "cell_0_0" bounds table

        width |> shouldEqual (Some expectedWidth)

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTablePerformance =
    type State = unit

    [<Test>]
    let ``10x10 table renders without errors`` () =
        task {
            let cells =
                [
                    for row in 0..9 do
                        [
                            for col in 0..9 do
                                Vdom.textContent false $"R{row}C{col}"
                        ]
                ]

            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> = Table.makeAuto cells

            let console, terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 30)

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

            // Just verify it renders without throwing
            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            // Verify first row rendered
            let output = ConsoleHarness.toString terminal
            output |> shouldContainText "R0C0"
        }

    [<Test>]
    let ``50x20 table renders without errors`` () =
        task {
            let cells =
                [
                    for row in 0..19 do
                        [
                            for col in 0..49 do
                                Vdom.textContent false $"{col}"
                        ]
                ]

            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> = Table.makeAuto cells

            let console, terminal = ConsoleHarness.make' (fun () -> 200) (fun () -> 50)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            let output = ConsoleHarness.toString terminal
            output |> shouldContainText "0"
        }

    [<Test>]
    let ``100x5 table renders without errors`` () =
        task {
            let cells =
                [
                    for row in 0..4 do
                        [
                            for col in 0..99 do
                                Vdom.textContent false $"C{col}"
                        ]
                ]

            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds, Unkeyed> = Table.makeAuto cells

            let console, terminal = ConsoleHarness.make' (fun () -> 400) (fun () -> 20)

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

            App.pumpOnce worldFreezer () haveFrameworkHandleFocus renderState processWorld vdom ActivationResolver.none

            let output = ConsoleHarness.toString terminal
            output |> shouldContainText "C0"
        }

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestVdomMeasure =
    [<Test>]
    let ``Vdom.measure returns correct measurements for text content`` () =
        let vdom = Vdom.textContent false "Hello World"

        let constraints =
            {
                MaxWidth = 100
                MaxHeight = 100
            }

        let measured = Vdom.measure vdom constraints

        measured.MinWidth |> shouldEqual 5 // "World" is the longest word
        measured.PreferredWidth |> shouldEqual 11 // "Hello World" full length
        measured.MinHeightForWidth 11 |> shouldEqual 1
        measured.PreferredHeightForWidth 11 |> shouldEqual 1

    [<Test>]
    let ``Vdom.measure handles text wrapping`` () =
        let vdom = Vdom.textContent false "Hello World Test"

        let constraints =
            {
                MaxWidth = 100
                MaxHeight = 100
            }

        let measured = Vdom.measure vdom constraints

        measured.MinWidth |> shouldEqual 5 // "Hello" or "World" is longest word
        measured.PreferredWidth |> shouldEqual 16 // Full text length

        // When constrained to 10 chars, should wrap
        let heightAt10 = measured.PreferredHeightForWidth 10

        (heightAt10 > 1) |> shouldEqual true

    [<Test>]
    let ``Vdom.measurehandles bordered content`` () =
        let innerVdom = Vdom.textContent false "Test"
        let vdom = Vdom.bordered innerVdom

        let constraints =
            {
                MaxWidth = 100
                MaxHeight = 100
            }

        let measured = Vdom.measure vdom constraints

        // Border adds 2 to width (1 on each side)
        measured.MinWidth |> shouldEqual 6 // 4 + 2 for border
        measured.PreferredWidth |> shouldEqual 6 // 4 + 2 for border

        // Border adds 2 to height
        measured.MinHeightForWidth 10 |> shouldEqual 3 // 1 + 2 for border

    [<Test>]
    let ``Vdom.measurehandles panel splits`` () =
        let left = Vdom.textContent false "Left"
        let right = Vdom.textContent false "Right"

        let leftKeyed = Vdom.withKey (NodeKey.make "left") left
        let rightKeyed = Vdom.withKey (NodeKey.make "right") right

        let vdom = Vdom.panelSplitAuto (SplitDirection.Vertical, leftKeyed, rightKeyed)

        let constraints =
            {
                MaxWidth = 100
                MaxHeight = 100
            }

        let measured = Vdom.measure vdom constraints

        // Auto split: MinWidth is sum of both
        measured.MinWidth |> shouldEqual 9 // 4 + 5 = 9
        measured.PreferredWidth |> shouldEqual 9

    [<Test>]
    let ``Vdom.measurehandles empty vdom`` () =
        let vdom = Vdom.empty

        let constraints =
            {
                MaxWidth = 100
                MaxHeight = 100
            }

        let measured = Vdom.measure vdom constraints

        measured.MinWidth |> shouldEqual 0
        measured.PreferredWidth |> shouldEqual 0
        measured.MinHeightForWidth 10 |> shouldEqual 0
        measured.PreferredHeightForWidth 10 |> shouldEqual 0

    [<Test>]
    let ``Vdom.measure respects max width constraint`` () =
        let vdom =
            Vdom.textContent false "This is a very long text that exceeds constraints"

        let constraints =
            {
                MaxWidth = 20
                MaxHeight = 100
            }

        let measured = Vdom.measure vdom constraints

        // Should be clamped to max width
        (measured.PreferredWidth <= 20) |> shouldEqual true
        (measured.MinWidth <= 20) |> shouldEqual true

    [<Test>]
    let ``Vdom.measure works with FlexibleContent`` () =
        let customMeasure (constraints : MeasureConstraints) : MeasuredSize =
            {
                MinWidth = 5
                PreferredWidth = 10
                MaxWidth = Some 20
                MinHeightForWidth = fun _ -> 2
                PreferredHeightForWidth = fun _ -> 3
                MaxHeightForWidth = fun _ -> Some 5
            }

        let customRender (bounds : Rectangle) : Vdom<DesiredBounds, Unkeyed> = Vdom.textContent false "Custom"

        let vdom = Vdom.flexibleContent customMeasure customRender

        let constraints =
            {
                MaxWidth = 100
                MaxHeight = 100
            }

        let measured = Vdom.measure vdom constraints

        measured.MinWidth |> shouldEqual 5
        measured.PreferredWidth |> shouldEqual 10
        measured.MinHeightForWidth 15 |> shouldEqual 2
        measured.PreferredHeightForWidth 15 |> shouldEqual 3
