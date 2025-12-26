namespace WoofWare.Zoomies.Test

open System
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies
open WoofWare.Zoomies.Components
open FsUnitTyped

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSingleSelection =
    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    type State =
        {
            SelectedIndex : int option
        }

    /// Simple event type for tests that only need viewport tracking
    type SimpleViewportEvent = | SimpleViewportInfo of SelectionListViewportInfo

    let itemAKey = NodeKey.make "item-a"
    let itemBKey = NodeKey.make "item-b"
    let itemCKey = NodeKey.make "item-c"
    let singleSelectPrefix = NodeKey.make "single-select"

    [<Test>]
    let ``empty single-selection`` () =
        task {
            let vdom (_ : IVdomContext) (_ : State) : Vdom<DesiredBounds> =
                (SingleSelection.make' (singleSelectPrefix, [||], SelectionListState.AtStart)).Vdom

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld = WorldProcessor.passthrough

            let renderState = MockTime.makeRenderStateStatic console None

            App.pumpOnce
                worldFreezer
                {
                    SelectedIndex = None
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
    let ``single-selection with three items none selected`` () =
        task {
            let vdom (_ : IVdomContext) (_ : State) : Vdom<DesiredBounds> =
                (SingleSelection.make' (
                    singleSelectPrefix,
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
                    |],
                    SelectionListState.AtStart
                ))
                    .Vdom

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld = WorldProcessor.passthrough

            let renderState = MockTime.makeRenderStateStatic console None

            App.pumpOnce
                worldFreezer
                {
                    SelectedIndex = None
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
 ○ Option A                   |
 ○ Option B                   |
 ○ Option C                   |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``single-selection with one item selected`` () =
        task {
            let vdom (_ : IVdomContext) (_ : State) : Vdom<DesiredBounds> =
                (SingleSelection.make' (
                    singleSelectPrefix,
                    [|
                        {
                            Label = "Option A"
                            IsSelected = false
                            IsFocused = false
                        }
                        {
                            Label = "Option B"
                            IsSelected = true
                            IsFocused = false
                        }
                        {
                            Label = "Option C"
                            IsSelected = false
                            IsFocused = false
                        }
                    |],
                    SelectionListState.AtStart
                ))
                    .Vdom

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld = WorldProcessor.passthrough

            let renderState = MockTime.makeRenderStateStatic console None

            App.pumpOnce
                worldFreezer
                {
                    SelectedIndex = Some 1
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
 ○ Option A                   |
 ◉ Option B                   |
 ○ Option C                   |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``single-selection with focused item`` () =
        task {
            let vdom (_ : IVdomContext) (_ : State) : Vdom<DesiredBounds> =
                (SingleSelection.make' (
                    singleSelectPrefix,
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
                    |],
                    SelectionListState.AtStart
                ))
                    .Vdom

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld = WorldProcessor.passthrough

            let renderState = MockTime.makeRenderStateStatic console None

            App.pumpOnce
                worldFreezer
                {
                    SelectedIndex = None
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
 ○ Option A                   |
[○]Option B                   |
 ○ Option C                   |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``single-selection with focused and selected item`` () =
        task {
            let vdom (_ : IVdomContext) (_ : State) : Vdom<DesiredBounds> =
                (SingleSelection.make' (
                    singleSelectPrefix,
                    [|
                        {
                            Label = "Option A"
                            IsSelected = false
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
                    |],
                    SelectionListState.AtStart
                ))
                    .Vdom

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld = WorldProcessor.passthrough

            let renderState = MockTime.makeRenderStateStatic console None

            App.pumpOnce
                worldFreezer
                {
                    SelectedIndex = Some 1
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
 ○ Option A                   |
[◉]Option B                   |
 ○ Option C                   |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``single-selection with framework integration and focus cycling`` () =
        task {
            let makeItems () =
                [|
                    {
                        Id = itemAKey
                        Label = "Option A"
                    }
                    {
                        Id = itemBKey
                        Label = "Option B"
                    }
                    {
                        Id = itemCKey
                        Label = "Option C"
                    }
                |]

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
                (SingleSelection.make (
                    ctx,
                    singleSelectPrefix,
                    makeItems (),
                    state.SelectedIndex,
                    SelectionListState.AtStart,
                    (fun _ -> ()),
                    isFirstToFocus = true
                ))
                    .Vdom

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
                { new WorldProcessor<SimpleViewportEvent, unit, State> with
                    member _.ProcessWorld (_, _, state) = ProcessWorldResult.make state
                    member _.ProcessPostLayoutEvents (_, _, state) = state
                }

            let renderState = MockTime.makeRenderStateStatic console None

            // Initial render - no focus yet
            App.pumpOnce
                worldFreezer
                {
                    SelectedIndex = None
                }
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<State>

            // Verify that initially no item is focused (no brackets around any radio button)
            expect {
                snapshot
                    @"
 ○ Option A                   |
 ○ Option B                   |
 ○ Option C                   |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }

            // Press Tab to focus first item
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            App.pumpOnce
                worldFreezer
                {
                    SelectedIndex = None
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
[○]Option A                   |
 ○ Option B                   |
 ○ Option C                   |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    type SelectListEvent =
        | SelectCursorUp
        | SelectCursorDown
        | SelectItem of int

    type SelectPostLayoutEvent = | SelectViewportInfo of SelectionListViewportInfo

    type SelectListState =
        {
            SelectedIndex : int option
            ListState : SelectionListState
        }

    [<Test>]
    let ``single-selection activation selects items`` () =
        task {
            let files =
                [|
                    {|
                        Id = itemAKey
                        Label = "Option A"
                    |}
                    {|
                        Id = itemBKey
                        Label = "Option B"
                    |}
                    {|
                        Id = itemCKey
                        Label = "Option C"
                    |}
                |]

            let makeItems () =
                files
                |> Array.map (fun f ->
                    {
                        Id = f.Id
                        Label = f.Label
                    }
                )

            let vdom (ctx : IVdomContext<_>) (s : SelectListState) : Vdom<DesiredBounds> =
                (SingleSelection.make (
                    ctx,
                    singleSelectPrefix,
                    makeItems (),
                    s.SelectedIndex,
                    s.ListState,
                    SelectViewportInfo,
                    isFirstToFocus = true
                ))
                    .Vdom

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
                { new WorldProcessor<SelectListEvent, SelectPostLayoutEvent, SelectListState> with
                    member _.ProcessWorld (inputs, renderState, s) =
                        let mutable newState = s

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent (SelectItem index) ->
                                if index >= 0 && index < files.Length then
                                    // Single selection: just set to this index
                                    newState <-
                                        { newState with
                                            SelectedIndex = Some index
                                        }
                            | WorldStateChange.ApplicationEvent SelectCursorUp ->
                                newState <-
                                    { newState with
                                        ListState = newState.ListState.MoveUp files.Length
                                    }
                            | WorldStateChange.ApplicationEvent SelectCursorDown ->
                                newState <-
                                    { newState with
                                        ListState = newState.ListState.MoveDown files.Length
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState

                    member _.ProcessPostLayoutEvents (events, _, state) =
                        let mutable newState = state

                        for (SelectViewportInfo info) in events do
                            newState <-
                                { newState with
                                    ListState = newState.ListState.EnsureVisible info.ViewportHeight
                                }

                        newState
                }

            let resolver =
                ActivationResolver.selectionList
                    singleSelectPrefix
                    (fun s -> s.ListState.CursorIndex)
                    SelectCursorUp
                    SelectCursorDown
                    SelectItem

            let renderState = MockTime.makeRenderStateStatic console None

            let initialState : SelectListState =
                {
                    SelectedIndex = None
                    ListState = SelectionListState.AtStart
                }

            // Initial render
            let mutable state =
                App.pumpOnce
                    worldFreezer
                    initialState
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

            // Tab to focus the list (list is a single focusable unit now)
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

            // Press Space to select first item (cursor is at 0)
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
[◉]Option A                   |
 ○ Option B                   |
 ○ Option C                   |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``single-selection changes selection when different item selected`` () =
        task {
            let files =
                [|
                    {|
                        Id = itemAKey
                        Label = "Option A"
                    |}
                    {|
                        Id = itemBKey
                        Label = "Option B"
                    |}
                    {|
                        Id = itemCKey
                        Label = "Option C"
                    |}
                |]

            let makeItems () =
                files
                |> Array.map (fun f ->
                    {
                        Id = f.Id
                        Label = f.Label
                    }
                )

            let vdom (ctx : IVdomContext<_>) (s : SelectListState) : Vdom<DesiredBounds> =
                (SingleSelection.make (
                    ctx,
                    singleSelectPrefix,
                    makeItems (),
                    s.SelectedIndex,
                    s.ListState,
                    SelectViewportInfo,
                    isFirstToFocus = true
                ))
                    .Vdom

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
                { new WorldProcessor<SelectListEvent, SelectPostLayoutEvent, SelectListState> with
                    member _.ProcessWorld (inputs, renderState, s) =
                        let mutable newState = s

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent (SelectItem index) ->
                                if index >= 0 && index < files.Length then
                                    newState <-
                                        { newState with
                                            SelectedIndex = Some index
                                        }
                            | WorldStateChange.ApplicationEvent SelectCursorUp ->
                                newState <-
                                    { newState with
                                        ListState = newState.ListState.MoveUp files.Length
                                    }
                            | WorldStateChange.ApplicationEvent SelectCursorDown ->
                                newState <-
                                    { newState with
                                        ListState = newState.ListState.MoveDown files.Length
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState

                    member _.ProcessPostLayoutEvents (events, _, state) =
                        let mutable newState = state

                        for (SelectViewportInfo info) in events do
                            newState <-
                                { newState with
                                    ListState = newState.ListState.EnsureVisible info.ViewportHeight
                                }

                        newState
                }

            let resolver =
                ActivationResolver.selectionList
                    singleSelectPrefix
                    (fun s -> s.ListState.CursorIndex)
                    SelectCursorUp
                    SelectCursorDown
                    SelectItem

            let renderState = MockTime.makeRenderStateStatic console None

            // Start with Option A selected
            let initialState : SelectListState =
                {
                    SelectedIndex = Some 0
                    ListState = SelectionListState.AtStart
                }

            // Initial render
            let mutable state =
                App.pumpOnce
                    worldFreezer
                    initialState
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

            // Tab to focus the list
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

            // Down arrow twice to move cursor to Option C
            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.DownArrow, false, false, false))

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

            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.DownArrow, false, false, false))

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

            // Press Space to select Option C (cursor is at 2)
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

            // Option A should no longer be selected, Option C should be selected
            expect {
                snapshot
                    @"
 ○ Option A                   |
 ○ Option B                   |
[◉]Option C                   |
                              |
                              |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``single-selection with long labels`` () =
        task {
            let vdom (_ : IVdomContext) (_ : State) : Vdom<DesiredBounds> =
                (SingleSelection.make' (
                    singleSelectPrefix,
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
                    |],
                    SelectionListState.AtStart
                ))
                    .Vdom

            let console, terminal = ConsoleHarness.make' (fun () -> 25) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld = WorldProcessor.passthrough

            let renderState = MockTime.makeRenderStateStatic console None

            App.pumpOnce
                worldFreezer
                {
                    SelectedIndex = Some 0
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
 ◉ el that might wrap    |
[○]Short                 |
                         |
                         |
"

                return ConsoleHarness.toString terminal
            }
        }

    // =====================================================================
    // Scrolling behavior tests
    // =====================================================================

    [<Test>]
    let ``list larger than viewport only shows visible items`` () =
        task {
            // 5 items in a viewport that can only show 3 lines
            let vdom (_ : IVdomContext) (_ : State) : Vdom<DesiredBounds> =
                (SingleSelection.make' (
                    singleSelectPrefix,
                    [|
                        {
                            Label = "Item 1"
                            IsSelected = false
                            IsFocused = false
                        }
                        {
                            Label = "Item 2"
                            IsSelected = false
                            IsFocused = false
                        }
                        {
                            Label = "Item 3"
                            IsSelected = false
                            IsFocused = false
                        }
                        {
                            Label = "Item 4"
                            IsSelected = false
                            IsFocused = false
                        }
                        {
                            Label = "Item 5"
                            IsSelected = false
                            IsFocused = false
                        }
                    |],
                    SelectionListState.AtStart
                ))
                    .Vdom

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld = WorldProcessor.passthrough

            let renderState = MockTime.makeRenderStateStatic console None

            App.pumpOnce
                worldFreezer
                {
                    SelectedIndex = None
                }
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<State>

            // Only first 3 items visible (viewport height = 3)
            expect {
                snapshot
                    @"
 ○ Item 1                     |
 ○ Item 2                     |
 ○ Item 3                     |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``scroll offset shows items starting from offset`` () =
        task {
            // 5 items, scroll offset at 2, viewport of 3
            let vdom (_ : IVdomContext) (_ : State) : Vdom<DesiredBounds> =
                (SingleSelection.make' (
                    singleSelectPrefix,
                    [|
                        {
                            Label = "Item 1"
                            IsSelected = false
                            IsFocused = false
                        }
                        {
                            Label = "Item 2"
                            IsSelected = false
                            IsFocused = false
                        }
                        {
                            Label = "Item 3"
                            IsSelected = false
                            IsFocused = false
                        }
                        {
                            Label = "Item 4"
                            IsSelected = false
                            IsFocused = false
                        }
                        {
                            Label = "Item 5"
                            IsSelected = false
                            IsFocused = false
                        }
                    |],
                    SelectionListState.AtOffset 2
                ))
                    .Vdom

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld = WorldProcessor.passthrough

            let renderState = MockTime.makeRenderStateStatic console None

            App.pumpOnce
                worldFreezer
                {
                    SelectedIndex = None
                }
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<State>

            // Items 3, 4, 5 visible (offset 2, viewport 3)
            expect {
                snapshot
                    @"
 ○ Item 3                     |
 ○ Item 4                     |
 ○ Item 5                     |
"

                return ConsoleHarness.toString terminal
            }
        }

    type ArrowTestEvent =
        | CursorUpEvt
        | CursorDownEvt
        | SelectEvt of int

    type ArrowPostLayoutEvent = | ArrowViewportInfo of SelectionListViewportInfo

    type ArrowTestState =
        {
            SelectedIndex : int option
            ListState : SelectionListState
        }

    [<Test>]
    let ``arrow keys move cursor through all items and scroll`` () =
        task {
            // 5 items in viewport of 3 - arrow keys should navigate through all items
            // When cursor moves past viewport, list should auto-scroll
            let makeItems () =
                [|
                    {
                        Id = NodeKey.make "item-1"
                        Label = "Item 1"
                    }
                    {
                        Id = NodeKey.make "item-2"
                        Label = "Item 2"
                    }
                    {
                        Id = NodeKey.make "item-3"
                        Label = "Item 3"
                    }
                    {
                        Id = NodeKey.make "item-4"
                        Label = "Item 4"
                    }
                    {
                        Id = NodeKey.make "item-5"
                        Label = "Item 5"
                    }
                |]

            let vdom (ctx : IVdomContext<_>) (s : ArrowTestState) : Vdom<DesiredBounds> =
                (SingleSelection.make (
                    ctx,
                    singleSelectPrefix,
                    makeItems (),
                    s.SelectedIndex,
                    s.ListState,
                    ArrowViewportInfo,
                    isFirstToFocus = true
                ))
                    .Vdom

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let processWorld =
                { new WorldProcessor<ArrowTestEvent, ArrowPostLayoutEvent, ArrowTestState> with
                    member _.ProcessWorld (inputs, renderState, s) =
                        let mutable newState = s

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent CursorUpEvt ->
                                newState <-
                                    { newState with
                                        ListState = newState.ListState.MoveUp 5
                                    }
                            | WorldStateChange.ApplicationEvent CursorDownEvt ->
                                newState <-
                                    { newState with
                                        ListState = newState.ListState.MoveDown 5
                                    }
                            | WorldStateChange.ApplicationEvent (SelectEvt index) ->
                                newState <-
                                    { newState with
                                        SelectedIndex = Some index
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState

                    member _.ProcessPostLayoutEvents (events, _, state) =
                        let mutable newState = state

                        for (ArrowViewportInfo info) in events do
                            newState <-
                                { newState with
                                    ListState = newState.ListState.EnsureVisible info.ViewportHeight
                                }

                        newState
                }

            let resolver =
                ActivationResolver.selectionList
                    singleSelectPrefix
                    (fun s -> s.ListState.CursorIndex)
                    CursorUpEvt
                    CursorDownEvt
                    SelectEvt

            let renderState = MockTime.makeRenderStateStatic console None

            let initialState : ArrowTestState =
                {
                    SelectedIndex = None
                    ListState = SelectionListState.AtStart
                }

            // Initial render
            let mutable state =
                App.pumpOnce
                    worldFreezer
                    initialState
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

            // Tab to focus the list
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

            // Down arrow 4 times to reach item 5
            for _ in 1..4 do
                world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.DownArrow, false, false, false))

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

            // Cursor should be on item 5, list should have scrolled to show items 3, 4, 5
            expect {
                snapshot
                    @"
 ○ Item 3                     |
 ○ Item 4                     |
[○]Item 5                     |
"

                return ConsoleHarness.toString terminal
            }

            // Scroll should be at 2 (showing items 3, 4, 5)
            state.ListState.ScrollOffset |> shouldEqual 2
            // Cursor should be at index 4 (item 5)
            state.ListState.CursorIndex |> shouldEqual 4
        }

    type FocusLeaveEvent =
        | FocusLeaveCursorUp
        | FocusLeaveCursorDown
        | FocusLeaveSelect

    type FocusLeavePostLayoutEvent = | FocusLeaveViewportInfo of SelectionListViewportInfo

    type FocusLeaveState =
        {
            SelectedIndex : int option
            ListState : SelectionListState
        }

    [<Test>]
    let ``scroll position preserved when focus leaves list`` () =
        task {
            // This test verifies that when focus moves away from the list,
            // the scroll position is remembered (because it's user-managed state)
            // We: (1) Tab to focus list, (2) scroll down, (3) Tab to checkbox, (4) verify scroll preserved
            // We use a vertical split with a checkbox on the right so Tab can move focus away from the list.

            let makeItems () =
                [|
                    {
                        Id = NodeKey.make "item-1"
                        Label = "Item 1"
                    }
                    {
                        Id = NodeKey.make "item-2"
                        Label = "Item 2"
                    }
                    {
                        Id = NodeKey.make "item-3"
                        Label = "Item 3"
                    }
                    {
                        Id = NodeKey.make "item-4"
                        Label = "Item 4"
                    }
                    {
                        Id = NodeKey.make "item-5"
                        Label = "Item 5"
                    }
                |]

            let checkboxKey = NodeKey.make "checkbox"

            let vdom (ctx : IVdomContext<_>) (s : FocusLeaveState) : Vdom<DesiredBounds> =
                let list =
                    (SingleSelection.make (
                        ctx,
                        singleSelectPrefix,
                        makeItems (),
                        s.SelectedIndex,
                        s.ListState,
                        FocusLeaveViewportInfo,
                        isFirstToFocus = true
                    ))
                        .Vdom

                let checkbox = Checkbox.make (ctx, checkboxKey, false)

                // Split: list on left, checkbox on right
                Vdom.panelSplitProportion (SplitDirection.Vertical, 0.8, list, checkbox)

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let processWorld =
                { new WorldProcessor<FocusLeaveEvent, FocusLeavePostLayoutEvent, FocusLeaveState> with
                    member _.ProcessWorld (inputs, _, s) =
                        let mutable newState = s

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent FocusLeaveCursorUp ->
                                newState <-
                                    { newState with
                                        ListState = newState.ListState.MoveUp 5
                                    }
                            | WorldStateChange.ApplicationEvent FocusLeaveCursorDown ->
                                newState <-
                                    { newState with
                                        ListState = newState.ListState.MoveDown 5
                                    }
                            | WorldStateChange.ApplicationEvent FocusLeaveSelect -> ()
                            | _ -> ()

                        ProcessWorldResult.make newState

                    member _.ProcessPostLayoutEvents (events, _, state) =
                        let mutable newState = state

                        for (FocusLeaveViewportInfo info) in events do
                            newState <-
                                { newState with
                                    ListState = newState.ListState.EnsureVisible info.ViewportHeight
                                }

                        newState
                }

            let resolver =
                ActivationResolver.selectionList
                    singleSelectPrefix
                    (fun s -> s.ListState.CursorIndex)
                    FocusLeaveCursorUp
                    FocusLeaveCursorDown
                    (fun _ -> FocusLeaveSelect)

            let renderState = MockTime.makeRenderStateStatic console None

            let initialState : FocusLeaveState =
                {
                    SelectedIndex = None
                    ListState = SelectionListState.AtStart
                }

            // Initial render - nothing focused
            let mutable state =
                App.pumpOnce
                    worldFreezer
                    initialState
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

            // Tab to focus the list (first focusable element)
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

            // Navigate down 4 times to reach item 5 and scroll
            for _ in 1..4 do
                world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.DownArrow, false, false, false))

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

            // Verify we're focused on item 5 and scrolled (showing items 3, 4, 5)
            // The checkbox is on the right (unfocused, shown as ☐)
            expect {
                snapshot
                    @"
 ○ Item 3                     |
 ○ Item 4                 ☐   |
[○]Item 5                     |
"

                return ConsoleHarness.toString terminal
            }

            // Scroll offset should be 2
            state.ListState.ScrollOffset |> shouldEqual 2

            // Tab to move focus to the checkbox (away from the list)
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

            // Scroll position preserved - still showing items 3, 4, 5, but now list is unfocused
            // Checkbox is now focused (shown with brackets around ☐)
            expect {
                snapshot
                    @"
 ○ Item 3                     |
 ○ Item 4                [☐]  |
 ○ Item 5                     |
"

                return ConsoleHarness.toString terminal
            }

            // Verify scroll offset is still 2
            state.ListState.ScrollOffset |> shouldEqual 2
        }

    type NoDanceEvent =
        | NoDanceCursorUp
        | NoDanceCursorDown
        | NoDanceSelect of int

    type NoDancePostLayoutEvent = | NoDanceViewportInfo of SelectionListViewportInfo

    type NoDanceState =
        {
            SelectedIndex : int option
            ListState : SelectionListState
        }

    [<Test>]
    let ``scroll does not dance when cursor moves within visible area`` () =
        task {
            // 5 items, viewport of 3, scroll at 1, cursor at 1 (showing items 2, 3, 4)
            // Cursor moves from item 2 to item 3 - scroll should NOT change
            let makeItems () =
                [|
                    {
                        Id = NodeKey.make "item-1"
                        Label = "Item 1"
                    }
                    {
                        Id = NodeKey.make "item-2"
                        Label = "Item 2"
                    }
                    {
                        Id = NodeKey.make "item-3"
                        Label = "Item 3"
                    }
                    {
                        Id = NodeKey.make "item-4"
                        Label = "Item 4"
                    }
                    {
                        Id = NodeKey.make "item-5"
                        Label = "Item 5"
                    }
                |]

            let vdom (ctx : IVdomContext<_>) (s : NoDanceState) : Vdom<DesiredBounds> =
                (SingleSelection.make (
                    ctx,
                    singleSelectPrefix,
                    makeItems (),
                    s.SelectedIndex,
                    s.ListState,
                    NoDanceViewportInfo
                ))
                    .Vdom

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let processWorld =
                { new WorldProcessor<NoDanceEvent, NoDancePostLayoutEvent, NoDanceState> with
                    member _.ProcessWorld (inputs, renderState, s) =
                        let mutable newState = s

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent NoDanceCursorUp ->
                                newState <-
                                    { newState with
                                        ListState = newState.ListState.MoveUp 5
                                    }
                            | WorldStateChange.ApplicationEvent NoDanceCursorDown ->
                                newState <-
                                    { newState with
                                        ListState = newState.ListState.MoveDown 5
                                    }
                            | WorldStateChange.ApplicationEvent (NoDanceSelect index) ->
                                newState <-
                                    { newState with
                                        SelectedIndex = Some index
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState

                    member _.ProcessPostLayoutEvents (events, _, state) =
                        let mutable newState = state

                        for (NoDanceViewportInfo info) in events do
                            // EnsureVisible won't change scroll if cursor is already visible
                            newState <-
                                { newState with
                                    ListState = newState.ListState.EnsureVisible info.ViewportHeight
                                }

                        newState
                }

            let resolver =
                ActivationResolver.selectionList
                    singleSelectPrefix
                    (fun s -> s.ListState.CursorIndex)
                    NoDanceCursorUp
                    NoDanceCursorDown
                    NoDanceSelect

            let renderState = MockTime.makeRenderStateStatic console None

            // Start with scroll at 1, cursor at 1 (showing items 2, 3, 4 with cursor on item 2)
            let initialState : NoDanceState =
                {
                    SelectedIndex = None
                    ListState =
                        {
                            ScrollOffset = 1
                            CursorIndex = 1
                        }
                }

            // Initial render (not focused yet, cursor at item 2)
            let mutable state =
                App.pumpOnce
                    worldFreezer
                    initialState
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

            // Tab to focus the list
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

            // Scroll should still be at 1
            state.ListState.ScrollOffset |> shouldEqual 1

            // Down arrow to move cursor to item 3 - still within viewport
            world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.DownArrow, false, false, false))

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

            // Scroll should STILL be at 1 (no dancing)
            state.ListState.ScrollOffset |> shouldEqual 1

            expect {
                snapshot
                    @"
 ○ Item 2                     |
[○]Item 3                     |
 ○ Item 4                     |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``EnsureVisible adjusts scroll correctly`` () =
        // Unit test for the EnsureVisible method
        // EnsureVisible uses the CursorIndex from state

        // Cursor at index 0 with offset 0 - already visible
        let state0 = SelectionListState.AtIndex 0
        (state0.EnsureVisible 3).ScrollOffset |> shouldEqual 0

        // Cursor at index 2 with offset 0, viewport 3 - already visible
        let state2 = SelectionListState.AtIndex 2
        (state2.EnsureVisible 3).ScrollOffset |> shouldEqual 0

        // Cursor at index 3 with offset 0, viewport 3 - need to scroll down
        let state3 = SelectionListState.AtIndex 3
        (state3.EnsureVisible 3).ScrollOffset |> shouldEqual 1

        // Cursor at index 5 with offset 0, viewport 3 - need to scroll down
        let state5 = SelectionListState.AtIndex 5
        (state5.EnsureVisible 3).ScrollOffset |> shouldEqual 3

        // Cursor at index 0 with offset 2 - need to scroll up
        let scrolledState0 =
            {
                ScrollOffset = 2
                CursorIndex = 0
            }

        (scrolledState0.EnsureVisible 3).ScrollOffset |> shouldEqual 0

        // Cursor at index 1 with offset 2 - need to scroll up
        let scrolledState1 =
            {
                ScrollOffset = 2
                CursorIndex = 1
            }

        (scrolledState1.EnsureVisible 3).ScrollOffset |> shouldEqual 1

        // Cursor at index 3 with offset 2 - already visible
        let scrolledState3 =
            {
                ScrollOffset = 2
                CursorIndex = 3
            }

        (scrolledState3.EnsureVisible 3).ScrollOffset |> shouldEqual 2

    // =====================================================================
    // Viewport-aware scrolling with PostLayoutEvent
    // =====================================================================

    type ViewportAwareEvent =
        | ViewportAwareCursorUp
        | ViewportAwareCursorDown
        | ViewportAwareSelect of int

    type ViewportAwarePostLayoutEvent = | ViewportAwareViewportInfo of SelectionListViewportInfo

    type ViewportAwareState =
        {
            SelectedIndex : int option
            ListState : SelectionListState
        }

    [<Test>]
    let ``viewport-aware scrolling keeps cursor visible using PostLayoutEvent`` () =
        task {
            // This test demonstrates the correct approach: use the typed IVdomContext<'appEvent>
            // with onViewportRendered to post viewport info during render.
            // The processWorld handler uses this info to call EnsureVisible.
            //
            // 5 items in viewport of 3, starting with scroll at 0, cursor at 0
            // Press down 4 times to reach item 5 (index 4)
            // With PostLayoutEvent, scroll offset is updated to 2 (items 3, 4, 5 visible)
            let makeItems () =
                [|
                    {
                        Id = NodeKey.make "item-1"
                        Label = "Item 1"
                    }
                    {
                        Id = NodeKey.make "item-2"
                        Label = "Item 2"
                    }
                    {
                        Id = NodeKey.make "item-3"
                        Label = "Item 3"
                    }
                    {
                        Id = NodeKey.make "item-4"
                        Label = "Item 4"
                    }
                    {
                        Id = NodeKey.make "item-5"
                        Label = "Item 5"
                    }
                |]

            let vdom (ctx : IVdomContext<_>) (s : ViewportAwareState) : Vdom<DesiredBounds> =
                (SingleSelection.make (
                    ctx,
                    singleSelectPrefix,
                    makeItems (),
                    s.SelectedIndex,
                    s.ListState,
                    ViewportAwareViewportInfo,
                    isFirstToFocus = true
                ))
                    .Vdom

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            // This processWorld handles the viewport event to call EnsureVisible
            let processWorld =
                { new WorldProcessor<ViewportAwareEvent, ViewportAwarePostLayoutEvent, ViewportAwareState> with
                    member _.ProcessWorld (inputs, renderState, s) =
                        let mutable newState = s

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent ViewportAwareCursorUp ->
                                newState <-
                                    { newState with
                                        ListState = newState.ListState.MoveUp 5
                                    }
                            | WorldStateChange.ApplicationEvent ViewportAwareCursorDown ->
                                newState <-
                                    { newState with
                                        ListState = newState.ListState.MoveDown 5
                                    }
                            | WorldStateChange.ApplicationEvent (ViewportAwareSelect index) ->
                                newState <-
                                    { newState with
                                        SelectedIndex = Some index
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState

                    member _.ProcessPostLayoutEvents (events, _, state) =
                        let mutable newState = state

                        for (ViewportAwareViewportInfo info) in events do
                            // Use the viewport height from the render to ensure cursor is visible
                            newState <-
                                { newState with
                                    ListState = newState.ListState.EnsureVisible info.ViewportHeight
                                }

                        newState
                }

            let resolver =
                ActivationResolver.selectionList
                    singleSelectPrefix
                    (fun s -> s.ListState.CursorIndex)
                    ViewportAwareCursorUp
                    ViewportAwareCursorDown
                    ViewportAwareSelect

            let renderState = MockTime.makeRenderStateStatic console None

            let initialState : ViewportAwareState =
                {
                    SelectedIndex = None
                    ListState = SelectionListState.AtStart
                }

            // Initial render
            let mutable state =
                App.pumpOnce
                    worldFreezer
                    initialState
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolver
                    (fun () -> false)

            // Tab to focus the list
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

            // Down arrow 4 times to reach item 5 (index 4)
            for _ in 1..4 do
                world.SendKey (ConsoleKeyInfo ('\000', ConsoleKey.DownArrow, false, false, false))

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

            // Cursor should be at index 4 (item 5)
            state.ListState.CursorIndex |> shouldEqual 4

            // With PostLayoutEvent, scroll offset should be 2 (showing items 3, 4, 5)
            state.ListState.ScrollOffset |> shouldEqual 2

            // Visual check: Item 5 should be visible with cursor
            expect {
                snapshot
                    @"
 ○ Item 3                     |
 ○ Item 4                     |
[○]Item 5                     |
"

                return ConsoleHarness.toString terminal
            }
        }
