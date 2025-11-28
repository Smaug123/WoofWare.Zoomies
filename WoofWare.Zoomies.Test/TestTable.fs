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
Left          Right |
A             B     |
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

            let haveFrameworkHandleFocus _ = true

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
ShortThis is a longer text tha|
     t should wrap            |
                              |
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
VeryLonVeryLong|
gColumnColumnNa|
Name1  me2     |
Data1  Data2   |
               |
"

                return ConsoleHarness.toString terminal
            }
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
    let ``Vdom.measurehandles text wrapping`` () =
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
    let ``Vdom.measurerespects max width constraint`` () =
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
    let ``Vdom.measureworks with FlexibleContent`` () =
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
