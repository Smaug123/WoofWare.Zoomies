namespace WoofWare.Zoomies.Test

open System
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies
open WoofWare.Zoomies.Components
open FsUnitTyped

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMultiSelection =
    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    type State =
        {
            Selected : Set<string>
        }

    let itemAKey = NodeKey.make "item-a"
    let itemBKey = NodeKey.make "item-b"
    let itemCKey = NodeKey.make "item-c"
    let multiSelectPrefix = NodeKey.make "multi-select"

    [<Test>]
    let ``empty multi-selection`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                MultiSelection.make' multiSelectPrefix [||]

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

            App.pumpOnce
                worldFreezer
                {
                    Selected = Set.empty
                }
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
    let ``multi-selection with three items none selected`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                MultiSelection.make'
                    multiSelectPrefix
                    [|
                        {
                            Label = "Option A"
                            IsSelected = false
                            IsFocused = false
                        }
                        {
                            Label = "Option B"
                            IsSelected = false
                            IsFocused = false
                        }
                        {
                            Label = "Option C"
                            IsSelected = false
                            IsFocused = false
                        }
                    |]

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

            App.pumpOnce
                worldFreezer
                {
                    Selected = Set.empty
                }
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
 ☐ Option A                   |
 ☐ Option B                   |
 ☐ Option C                   |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``multi-selection with some items selected`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                MultiSelection.make'
                    multiSelectPrefix
                    [|
                        {
                            Label = "Option A"
                            IsSelected = true
                            IsFocused = false
                        }
                        {
                            Label = "Option B"
                            IsSelected = false
                            IsFocused = false
                        }
                        {
                            Label = "Option C"
                            IsSelected = true
                            IsFocused = false
                        }
                    |]

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

            App.pumpOnce
                worldFreezer
                {
                    Selected = Set.empty
                }
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
 ☑ Option A                   |
 ☐ Option B                   |
 ☑ Option C                   |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``multi-selection with focused item`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                MultiSelection.make'
                    multiSelectPrefix
                    [|
                        {
                            Label = "Option A"
                            IsSelected = false
                            IsFocused = false
                        }
                        {
                            Label = "Option B"
                            IsSelected = false
                            IsFocused = true
                        }
                        {
                            Label = "Option C"
                            IsSelected = false
                            IsFocused = false
                        }
                    |]

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

            App.pumpOnce
                worldFreezer
                {
                    Selected = Set.empty
                }
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
 ☐ Option A                   |
[☐]Option B                   |
 ☐ Option C                   |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``multi-selection with focused and selected item`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                MultiSelection.make'
                    multiSelectPrefix
                    [|
                        {
                            Label = "Option A"
                            IsSelected = true
                            IsFocused = false
                        }
                        {
                            Label = "Option B"
                            IsSelected = true
                            IsFocused = true
                        }
                        {
                            Label = "Option C"
                            IsSelected = false
                            IsFocused = false
                        }
                    |]

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

            App.pumpOnce
                worldFreezer
                {
                    Selected = Set.empty
                }
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
 ☑ Option A                   |
[☑]Option B                   |
 ☐ Option C                   |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``multi-selection with framework integration and focus cycling`` () =
        task {
            let makeItems (state : State) =
                [|
                    {
                        Id = itemAKey
                        Label = "Option A"
                        IsSelected = Set.contains "a" state.Selected
                    }
                    {
                        Id = itemBKey
                        Label = "Option B"
                        IsSelected = Set.contains "b" state.Selected
                    }
                    {
                        Id = itemCKey
                        Label = "Option C"
                        IsSelected = Set.contains "c" state.Selected
                    }
                |]

            let vdom (ctx : VdomContext) (state : State) : Vdom<DesiredBounds> =
                MultiSelection.make (ctx, multiSelectPrefix, makeItems state, isFirstToFocus = true)

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 5)

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

            // Initial render - no focus yet
            App.pumpOnce
                worldFreezer
                {
                    Selected = Set.empty
                }
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<State>

            // Press Tab to focus first item
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            App.pumpOnce
                worldFreezer
                {
                    Selected = Set.empty
                }
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
[☐]Option A                   |
 ☐ Option B                   |
 ☐ Option C                   |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``multi-selection focus cycles through all items`` () =
        task {
            let makeItems (state : State) =
                [|
                    {
                        Id = itemAKey
                        Label = "Option A"
                        IsSelected = Set.contains "a" state.Selected
                    }
                    {
                        Id = itemBKey
                        Label = "Option B"
                        IsSelected = Set.contains "b" state.Selected
                    }
                    {
                        Id = itemCKey
                        Label = "Option C"
                        IsSelected = Set.contains "c" state.Selected
                    }
                |]

            let vdom (ctx : VdomContext) (state : State) : Vdom<DesiredBounds> =
                MultiSelection.make (ctx, multiSelectPrefix, makeItems state, isFirstToFocus = true)

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 5)

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

            // Initial render
            App.pumpOnce
                worldFreezer
                {
                    Selected = Set.empty
                }
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<State>

            // Tab to first item
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            App.pumpOnce
                worldFreezer
                {
                    Selected = Set.empty
                }
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<State>

            // Tab to second item
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            App.pumpOnce
                worldFreezer
                {
                    Selected = Set.empty
                }
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<State>

            // Tab to third item
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            App.pumpOnce
                worldFreezer
                {
                    Selected = Set.empty
                }
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
 ☐ Option A                   |
 ☐ Option B                   |
[☐]Option C                   |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    type ToggleEvent = | Toggle of NodeKey

    [<Test>]
    let ``multi-selection activation toggles items`` () =
        task {
            let mutable state =
                {
                    Selected = Set.empty
                }

            let makeItems () =
                [|
                    {
                        Id = itemAKey
                        Label = "Option A"
                        IsSelected = Set.contains "a" state.Selected
                    }
                    {
                        Id = itemBKey
                        Label = "Option B"
                        IsSelected = Set.contains "b" state.Selected
                    }
                    {
                        Id = itemCKey
                        Label = "Option C"
                        IsSelected = Set.contains "c" state.Selected
                    }
                |]

            let vdom (ctx : VdomContext) (s : State) : Vdom<DesiredBounds> =
                MultiSelection.make (ctx, multiSelectPrefix, makeItems (), isFirstToFocus = true)

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let processWorld =
                { new WorldProcessor<ToggleEvent, State> with
                    member _.ProcessWorld (inputs, renderState, s) =
                        let mutable newState = s

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent (Toggle toggledKey) ->
                                let id =
                                    if toggledKey = itemAKey then "a"
                                    elif toggledKey = itemBKey then "b"
                                    elif toggledKey = itemCKey then "c"
                                    else failwith "unexpected key"

                                newState <-
                                    { newState with
                                        Selected =
                                            if Set.contains id newState.Selected then
                                                Set.remove id newState.Selected
                                            else
                                                Set.add id newState.Selected
                                    }
                            | _ -> ()

                        state <- newState
                        ProcessWorldResult.make newState
                }

            let resolver =
                ActivationResolver.multiSelection [| itemAKey ; itemBKey ; itemCKey |] Toggle

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            // Initial render
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

            // Tab to first item
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

            // Press Space to toggle first item
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

            expect {
                snapshot
                    @"
[☑]Option A                   |
 ☐ Option B                   |
 ☐ Option C                   |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``multi-selection with long labels`` () =
        task {
            let vdom (_ : VdomContext) (_ : State) : Vdom<DesiredBounds> =
                MultiSelection.make'
                    multiSelectPrefix
                    [|
                        {
                            Label = "A very long option label that might wrap"
                            IsSelected = true
                            IsFocused = false
                        }
                        {
                            Label = "Short"
                            IsSelected = false
                            IsFocused = true
                        }
                    |]

            let console, terminal = ConsoleHarness.make' (fun () -> 25) (fun () -> 5)

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
                {
                    Selected = Set.empty
                }
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
   A very long option lab|
 ☑ el that might wrap    |
[☐]Short                 |
                         |
                         |
"

                return ConsoleHarness.toString terminal
            }
        }
