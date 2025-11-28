namespace WoofWare.Zoomies.Test

open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

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
