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

    /// Simple event type for tests that only need viewport tracking
    type SimpleViewportEvent = | SimpleViewportInfo of SelectionListViewportInfo

    let itemAKey = NodeKey.make "item-a"
    let itemBKey = NodeKey.make "item-b"
    let itemCKey = NodeKey.make "item-c"
    let multiSelectPrefix = NodeKey.make "multi-select"

    [<Test>]
    let ``empty multi-selection`` () =
        task {
            let vdom (_ : IVdomContext) (_ : State) : Vdom<DesiredBounds> =
                (MultiSelection.make' (multiSelectPrefix, [||], SelectionListState.AtStart)).Vdom

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
            let vdom (_ : IVdomContext) (_ : State) : Vdom<DesiredBounds> =
                (MultiSelection.make' (
                    multiSelectPrefix,
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
            let vdom (_ : IVdomContext) (_ : State) : Vdom<DesiredBounds> =
                (MultiSelection.make' (
                    multiSelectPrefix,
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
            let vdom (_ : IVdomContext) (_ : State) : Vdom<DesiredBounds> =
                (MultiSelection.make' (
                    multiSelectPrefix,
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
            let vdom (_ : IVdomContext) (_ : State) : Vdom<DesiredBounds> =
                (MultiSelection.make' (
                    multiSelectPrefix,
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

            // Create a vdom with the list AND another focusable element so we can cycle
            let buttonKey = NodeKey.make "button"

            let vdom (ctx : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> =
                let list =
                    (MultiSelection.make (
                        ctx,
                        multiSelectPrefix,
                        makeItems state,
                        SelectionListState.AtStart,
                        (fun _ -> ()),
                        isFirstToFocus = true
                    ))
                        .Vdom

                let button = Button.make (ctx, buttonKey, "OK")

                Vdom.panelSplitProportion (SplitDirection.Horizontal, 0.8, list, button)

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
                    Selected = Set.empty
                }
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<State>

            // Press Tab to focus list (first focusable)
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
           [  OK  ]           |
"

                return ConsoleHarness.toString terminal
            }

            // Press Tab again to cycle focus to button
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
 ☐ Option C                   |
                              |
           [[ OK ]]           |
"

                return ConsoleHarness.toString terminal
            }

            // Press Shift+Tab to cycle focus back to list
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, true, false, false))

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
           [  OK  ]           |
"

                return ConsoleHarness.toString terminal
            }
        }

    type ToggleListEvent =
        | ToggleCursorUp
        | ToggleCursorDown
        | ToggleItem of int

    type TogglePostLayoutEvent = | ToggleViewportInfo of SelectionListViewportInfo

    type ToggleListState =
        {
            Selected : Set<string>
            ListState : SelectionListState
        }

    [<Test>]
    let ``multi-selection activation toggles items`` () =
        task {
            let files =
                [|
                    {|
                        Id = itemAKey
                        Label = "Option A"
                        FileId = "a"
                    |}
                    {|
                        Id = itemBKey
                        Label = "Option B"
                        FileId = "b"
                    |}
                    {|
                        Id = itemCKey
                        Label = "Option C"
                        FileId = "c"
                    |}
                |]

            let makeItems (s : ToggleListState) =
                files
                |> Array.map (fun f ->
                    {
                        Id = f.Id
                        Label = f.Label
                        IsSelected = Set.contains f.FileId s.Selected
                    }
                )

            let vdom (ctx : IVdomContext<_>) (s : ToggleListState) : Vdom<DesiredBounds> =
                (MultiSelection.make (
                    ctx,
                    multiSelectPrefix,
                    makeItems s,
                    s.ListState,
                    ToggleViewportInfo,
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
                { new WorldProcessor<ToggleListEvent, TogglePostLayoutEvent, ToggleListState> with
                    member _.ProcessWorld (inputs, renderState, s) =
                        let mutable newState = s

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent (ToggleItem index) ->
                                if index >= 0 && index < files.Length then
                                    let fileId = files.[index].FileId

                                    newState <-
                                        { newState with
                                            Selected =
                                                if Set.contains fileId newState.Selected then
                                                    Set.remove fileId newState.Selected
                                                else
                                                    Set.add fileId newState.Selected
                                        }
                            | WorldStateChange.ApplicationEvent ToggleCursorUp ->
                                newState <-
                                    { newState with
                                        ListState = newState.ListState.MoveUp files.Length
                                    }
                            | WorldStateChange.ApplicationEvent ToggleCursorDown ->
                                newState <-
                                    { newState with
                                        ListState = newState.ListState.MoveDown files.Length
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState

                    member _.ProcessPostLayoutEvents (events, _, state) =
                        let mutable newState = state

                        for (ToggleViewportInfo info) in events do
                            newState <-
                                { newState with
                                    ListState = newState.ListState.EnsureVisible info.ViewportHeight
                                }

                        newState
                }

            let resolver =
                ActivationResolver.selectionList
                    multiSelectPrefix
                    (fun s -> s.ListState.CursorIndex)
                    ToggleCursorUp
                    ToggleCursorDown
                    ToggleItem

            let renderState = MockTime.makeRenderStateStatic console None

            let initialState : ToggleListState =
                {
                    Selected = Set.empty
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

            // Press Space to toggle first item (cursor is at 0)
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
            let vdom (_ : IVdomContext) (_ : State) : Vdom<DesiredBounds> =
                (MultiSelection.make' (
                    multiSelectPrefix,
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
                    Selected = Set.empty
                }
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<State>

            // With TopLeft alignment, checkbox appears on first line of wrapped item
            expect {
                snapshot
                    @"
 ☑ A very long option lab|
   el that might wrap    |
[☐]Short                 |
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
                (MultiSelection.make' (
                    multiSelectPrefix,
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
                    Selected = Set.empty
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
 ☐ Item 1                     |
 ☐ Item 2                     |
 ☐ Item 3                     |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``scroll offset shows items starting from offset`` () =
        task {
            // 5 items, scroll offset at 2, viewport of 3
            let vdom (_ : IVdomContext) (_ : State) : Vdom<DesiredBounds> =
                (MultiSelection.make' (
                    multiSelectPrefix,
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
                    Selected = Set.empty
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
 ☐ Item 3                     |
 ☐ Item 4                     |
 ☐ Item 5                     |
"

                return ConsoleHarness.toString terminal
            }
        }

    type ArrowTestEvent =
        | CursorUpEvt
        | CursorDownEvt
        | ToggleEvt of int

    type ArrowPostLayoutEvent = | ArrowViewportInfo of SelectionListViewportInfo

    type ArrowTestState =
        {
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
                        IsSelected = false
                    }
                    {
                        Id = NodeKey.make "item-2"
                        Label = "Item 2"
                        IsSelected = false
                    }
                    {
                        Id = NodeKey.make "item-3"
                        Label = "Item 3"
                        IsSelected = false
                    }
                    {
                        Id = NodeKey.make "item-4"
                        Label = "Item 4"
                        IsSelected = false
                    }
                    {
                        Id = NodeKey.make "item-5"
                        Label = "Item 5"
                        IsSelected = false
                    }
                |]

            let vdom (ctx : IVdomContext<_>) (s : ArrowTestState) : Vdom<DesiredBounds> =
                (MultiSelection.make (
                    ctx,
                    multiSelectPrefix,
                    makeItems (),
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
                            | WorldStateChange.ApplicationEvent (ToggleEvt _) -> ()
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
                    multiSelectPrefix
                    (fun s -> s.ListState.CursorIndex)
                    CursorUpEvt
                    CursorDownEvt
                    ToggleEvt

            let renderState = MockTime.makeRenderStateStatic console None

            let initialState : ArrowTestState =
                {
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
 ☐ Item 3                     |
 ☐ Item 4                     |
[☐]Item 5                     |
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
        /// Required by ActivationResolver.selectionList; handler is a no-op since this test
        /// focuses on scroll preservation during focus changes, not item toggling.
        | FocusLeaveToggle of int

    type FocusLeavePostLayoutEvent = | FocusLeaveViewportInfo of SelectionListViewportInfo

    type FocusLeaveState =
        {
            ListState : SelectionListState
        }

    [<Test>]
    let ``scroll position preserved when focus leaves list`` () =
        task {
            // This test verifies that when focus moves away from the list,
            // the scroll position is remembered (because it's user-managed state).
            //
            // Console height is 4, split 0.75/0.25 -> list gets 3 rows, button gets 1 row.
            // With 5 items and cursor at index 4, EnsureVisible(3) sets ScrollOffset = 2,
            // showing items 3, 4, 5 in the 3-row viewport.

            let makeItems () =
                [|
                    {
                        Id = NodeKey.make "item-1"
                        Label = "Item 1"
                        IsSelected = false
                    }
                    {
                        Id = NodeKey.make "item-2"
                        Label = "Item 2"
                        IsSelected = false
                    }
                    {
                        Id = NodeKey.make "item-3"
                        Label = "Item 3"
                        IsSelected = false
                    }
                    {
                        Id = NodeKey.make "item-4"
                        Label = "Item 4"
                        IsSelected = false
                    }
                    {
                        Id = NodeKey.make "item-5"
                        Label = "Item 5"
                        IsSelected = false
                    }
                |]

            let buttonKey = NodeKey.make "button"

            let vdom (ctx : IVdomContext<FocusLeavePostLayoutEvent>) (s : FocusLeaveState) : Vdom<DesiredBounds> =
                let list =
                    (MultiSelection.make (
                        ctx,
                        multiSelectPrefix,
                        makeItems (),
                        s.ListState,
                        FocusLeaveViewportInfo,
                        isFirstToFocus = true
                    ))
                        .Vdom

                let button = Button.make (ctx, buttonKey, "OK")

                Vdom.panelSplitProportion (SplitDirection.Horizontal, 0.75, list, button)

            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 4)

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
                    member _.ProcessWorld (inputs, renderState, s) =
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
                            | WorldStateChange.ApplicationEvent (FocusLeaveToggle _) -> ()
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
                    multiSelectPrefix
                    (fun s -> s.ListState.CursorIndex)
                    FocusLeaveCursorUp
                    FocusLeaveCursorDown
                    FocusLeaveToggle

            let renderState = MockTime.makeRenderStateStatic console None

            let initialState : FocusLeaveState =
                {
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

            // Down arrow 4 times to scroll to showing items 3, 4, 5 with cursor on item 5
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

            // Verify we're scrolled and cursor is on item 5
            state.ListState.ScrollOffset |> shouldEqual 2
            state.ListState.CursorIndex |> shouldEqual 4

            // Now Tab away from the list to the button
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

            // Scroll position should be preserved even though focus left
            state.ListState.ScrollOffset |> shouldEqual 2

            // List should still show items 3, 4, 5 (but no cursor highlight)
            // Button should now be focused
            expect {
                snapshot
                    @"
 ☐ Item 3                     |
 ☐ Item 4                     |
 ☐ Item 5                     |
           [[ OK ]]           |
"

                return ConsoleHarness.toString terminal
            }
        }

    type NoDanceEvent =
        | NoDanceCursorUp
        | NoDanceCursorDown
        | NoDanceToggle of int

    type NoDancePostLayoutEvent = | NoDanceViewportInfo of SelectionListViewportInfo

    type NoDanceState =
        {
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
                        IsSelected = false
                    }
                    {
                        Id = NodeKey.make "item-2"
                        Label = "Item 2"
                        IsSelected = false
                    }
                    {
                        Id = NodeKey.make "item-3"
                        Label = "Item 3"
                        IsSelected = false
                    }
                    {
                        Id = NodeKey.make "item-4"
                        Label = "Item 4"
                        IsSelected = false
                    }
                    {
                        Id = NodeKey.make "item-5"
                        Label = "Item 5"
                        IsSelected = false
                    }
                |]

            let vdom (ctx : IVdomContext<_>) (s : NoDanceState) : Vdom<DesiredBounds> =
                (MultiSelection.make (ctx, multiSelectPrefix, makeItems (), s.ListState, NoDanceViewportInfo)).Vdom

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
                            | WorldStateChange.ApplicationEvent (NoDanceToggle _) -> ()
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
                    multiSelectPrefix
                    (fun s -> s.ListState.CursorIndex)
                    NoDanceCursorUp
                    NoDanceCursorDown
                    NoDanceToggle

            let renderState = MockTime.makeRenderStateStatic console None

            // Start with scroll at 1, cursor at 1 (showing items 2, 3, 4 with cursor on item 2)
            let initialState : NoDanceState =
                {
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
 ☐ Item 2                     |
[☐]Item 3                     |
 ☐ Item 4                     |
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
        | ViewportAwareToggle of int

    type ViewportAwarePostLayoutEvent = | ViewportAwareViewportInfo of SelectionListViewportInfo

    type ViewportAwareState =
        {
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
                        IsSelected = false
                    }
                    {
                        Id = NodeKey.make "item-2"
                        Label = "Item 2"
                        IsSelected = false
                    }
                    {
                        Id = NodeKey.make "item-3"
                        Label = "Item 3"
                        IsSelected = false
                    }
                    {
                        Id = NodeKey.make "item-4"
                        Label = "Item 4"
                        IsSelected = false
                    }
                    {
                        Id = NodeKey.make "item-5"
                        Label = "Item 5"
                        IsSelected = false
                    }
                |]

            let vdom (ctx : IVdomContext<_>) (s : ViewportAwareState) : Vdom<DesiredBounds> =
                (MultiSelection.make (
                    ctx,
                    multiSelectPrefix,
                    makeItems (),
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
                            | WorldStateChange.ApplicationEvent (ViewportAwareToggle _) -> ()
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
                    multiSelectPrefix
                    (fun s -> s.ListState.CursorIndex)
                    ViewportAwareCursorUp
                    ViewportAwareCursorDown
                    ViewportAwareToggle

            let renderState = MockTime.makeRenderStateStatic console None

            let initialState : ViewportAwareState =
                {
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
 ☐ Item 3                     |
 ☐ Item 4                     |
[☐]Item 5                     |
"

                return ConsoleHarness.toString terminal
            }
        }
