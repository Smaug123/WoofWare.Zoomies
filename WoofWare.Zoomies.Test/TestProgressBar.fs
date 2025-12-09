namespace WoofWare.Zoomies.Test

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

    [<Struct>]
    type State = | NoState

    [<Test>]
    let ``progress bar at 0%`` () =
        task {
            let vdom (_ : IVdomContext<_>) (_ : State) : Vdom<DesiredBounds> = ProgressBar.make 0.0 (Some 10)

            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                    member _.ProcessPostLayoutEvents (_events, _ctx, state) : State = state
                }

            let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                NoState
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<State>

            expect {
                snapshot
                    @"
[░░░░░░░░░░] 0%     |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``progress bar at 50%`` () =
        task {
            let vdom (_ : IVdomContext<_>) (_ : State) : Vdom<DesiredBounds> = ProgressBar.make 0.5 (Some 10)

            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                    member _.ProcessPostLayoutEvents (_events, _ctx, state) : State = state
                }

            let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                NoState
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<State>

            expect {
                snapshot
                    @"
[█████░░░░░] 50%    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``progress bar at 100%`` () =
        task {
            let vdom (_ : IVdomContext<_>) (_ : State) : Vdom<DesiredBounds> = ProgressBar.make 1.0 (Some 10)

            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                    member _.ProcessPostLayoutEvents (_events, _ctx, state) : State = state
                }

            let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                NoState
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<State>

            expect {
                snapshot
                    @"
[██████████] 100%   |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``progress bar with label`` () =
        task {
            let vdom (_ : IVdomContext<_>) (_ : State) : Vdom<DesiredBounds> =
                let options =
                    ProgressBar.Options.Default |> ProgressBar.Options.WithLabel "Loading:"

                ProgressBar.make' options 0.3 (Some 10)

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                    member _.ProcessPostLayoutEvents (_events, _ctx, state) : State = state
                }

            let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                NoState
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<State>

            expect {
                snapshot
                    @"
Loading:[███░░░░░░░] 30%      |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``progress bar without percentage`` () =
        task {
            let vdom (_ : IVdomContext<_>) (_ : State) : Vdom<DesiredBounds> =
                let options = ProgressBar.Options.Default |> ProgressBar.Options.WithoutPercentage
                ProgressBar.make' options 0.7 (Some 10)

            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                    member _.ProcessPostLayoutEvents (_events, _ctx, state) : State = state
                }

            let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                NoState
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<State>

            expect {
                snapshot
                    @"
[███████░░░]        |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``progress bar with label and no percentage`` () =
        task {
            let vdom (_ : IVdomContext<_>) (_ : State) : Vdom<DesiredBounds> =
                let options =
                    ProgressBar.Options.Default
                    |> ProgressBar.Options.WithLabel "Progress:"
                    |> ProgressBar.Options.WithoutPercentage

                ProgressBar.make' options 0.6 (Some 10)

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                    member _.ProcessPostLayoutEvents (_events, _ctx, state) : State = state
                }

            let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                NoState
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<State>

            expect {
                snapshot
                    @"
Progress:[██████░░░░]         |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``progress bar with varying widths`` () =
        task {
            let vdom (_ : IVdomContext<_>) (_ : State) : Vdom<DesiredBounds> =
                let bar1 = ProgressBar.make 0.5 (Some 5)
                let bar2 = ProgressBar.make 0.5 (Some 20)

                Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 1, bar1, bar2)

            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                    member _.ProcessPostLayoutEvents (_events, _ctx, state) : State = state
                }

            let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                NoState
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<State>

            expect {
                snapshot
                    @"
[██░░░] 50%                             |
[██████████░░░░░░░░░░] 50%              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``progress bar displays n/a for invalid progress`` () =
        task {
            let vdom (_ : IVdomContext<_>) (_ : State) : Vdom<DesiredBounds> = ProgressBar.make -0.1 (Some 10)

            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                    member _.ProcessPostLayoutEvents (_events, _ctx, state) : State = state
                }

            let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                NoState
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore

            expect {
                snapshot
                    @"
[░░░░░░░░░░] n/a%   |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``progress bar handles invalid width gracefully`` () =
        task {
            let vdom (_ : IVdomContext<_>) (_ : State) : Vdom<DesiredBounds> =
                let bar1 = ProgressBar.make 0.5 (Some 0)
                let bar2 = ProgressBar.make 0.5 (Some -1)

                Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 1, bar1, bar2)

            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                    member _.ProcessPostLayoutEvents (_events, _ctx, state) : State = state
                }

            let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                NoState
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<State>

            expect {
                snapshot
                    @"
[█████░░░░░] 50%                        |
[█████░░░░░] 50%                        |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``progress bar with non-divisible progress`` () =
        task {
            let vdom (_ : IVdomContext<_>) (_ : State) : Vdom<DesiredBounds> = ProgressBar.make 0.33 (Some 10)

            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, unit, State> with
                    member _.ProcessWorld (inputs, renderState, state) = ProcessWorldResult.make state
                    member _.ProcessPostLayoutEvents (_events, _ctx, state) : State = state
                }

            let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                NoState
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<State>

            expect {
                snapshot
                    @"
[███░░░░░░░] 33%    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }
