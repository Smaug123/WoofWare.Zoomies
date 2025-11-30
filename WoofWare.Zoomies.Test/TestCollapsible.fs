namespace WoofWare.Zoomies.Test

open System
open NUnit.Framework
open FsUnitTyped
open WoofWare.Expect
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCollapsible =
    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    type State =
        {
            CollapsibleState : Collapsible.State
        }

    [<Test>]
    let ``collapsible toggles between collapsed and expanded states`` () =
        task {
            let collapsibleKey = NodeKey.make "collapsible"

            let vdom (vdomContext : VdomContext) (state : State) =
                let childContent = Vdom.textContent "This stuff was hidden"

                Collapsible.make vdomContext collapsibleKey state.CollapsibleState "Collapsible section" childContent

            let console, terminal = ConsoleHarness.make' (fun () -> 60) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let state =
                {
                    CollapsibleState = Collapsible.State.Collapsed
                }

            let haveFrameworkHandleFocus _ = true

            let processWorld =
                { new WorldProcessor<_, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        let mutable newState = state

                        for s in inputs do
                            match s with
                            | WorldStateChange.Keystroke c ->
                                if c.KeyChar = ' ' then
                                    match VdomContext.focusedKey renderState with
                                    | None -> ()
                                    | Some focused ->
                                        if focused = collapsibleKey then
                                            newState <-
                                                {
                                                    CollapsibleState = state.CollapsibleState.ToggledExpansion ()
                                                }
                                        else
                                            failwith "unexpected key"
                                else
                                    failwith "unexpected key char"
                            | WorldStateChange.MouseEvent _ -> failwith "no mouse events"
                            | WorldStateChange.ApplicationEvent () -> failwith "no app events"
                            | WorldStateChange.KeyboardEvent _ -> failwith "no keyboard events"
                            | WorldStateChange.ApplicationEventException _ -> failwith "no exceptions possible"

                        ProcessWorldResult.make newState
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None
            let mutable currentState = state

            // Initial render: collapsed and unfocused
            currentState <-
                App.pumpOnce
                    worldFreezer
                    currentState
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none

            expect {
                snapshot
                    @"
 ▶  Collapsible section                                     |
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

            // Tab to focus the collapsible
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            currentState <-
                App.pumpOnce
                    worldFreezer
                    currentState
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none

            expect {
                snapshot
                    @"
[▶] Collapsible section                                     |
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

            // Press space to expand
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            currentState <-
                App.pumpOnce
                    worldFreezer
                    currentState
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none

            expect {
                snapshot
                    @"
[▼] Collapsible section                                     |
This stuff was hidden                                       |
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

            // Press space again to collapse
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            currentState <-
                App.pumpOnce
                    worldFreezer
                    currentState
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none

            expect {
                snapshot
                    @"
[▶] Collapsible section                                     |
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
    let ``collapsible with multiple nested elements`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 60) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let state =
                {
                    CollapsibleState = Collapsible.State.Collapsed
                }

            let collapsibleKey = NodeKey.make "collapsible"

            let vdom (vdomContext : VdomContext) (state : State) =
                let childContent =
                    let line1 =
                        Vdom.textContent "Line 1 of content" |> Vdom.withKey (NodeKey.make "line1")

                    let line2 =
                        Vdom.textContent "Line 2 of content" |> Vdom.withKey (NodeKey.make "line2")

                    Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 1, line1, line2)

                Collapsible.make vdomContext collapsibleKey state.CollapsibleState "Multi-line section" childContent

            let haveFrameworkHandleFocus _ = true

            let processWorld =
                { new WorldProcessor<_, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        let mutable newState = state

                        for s in inputs do
                            match s with
                            | WorldStateChange.Keystroke c ->
                                if c.KeyChar = ' ' then
                                    match VdomContext.focusedKey renderState with
                                    | None -> ()
                                    | Some focused ->
                                        if focused = collapsibleKey then
                                            newState <-
                                                {
                                                    CollapsibleState = state.CollapsibleState.ToggledExpansion ()
                                                }
                                        else
                                            failwith "unexpected key"
                                else
                                    failwith "unexpected key char"
                            | WorldStateChange.MouseEvent _ -> failwith "no mouse events"
                            | WorldStateChange.ApplicationEvent () -> failwith "no app events"
                            | WorldStateChange.KeyboardEvent _ -> failwith "no keyboard events"
                            | WorldStateChange.ApplicationEventException _ -> failwith "no exceptions possible"

                        ProcessWorldResult.make newState
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None
            let mutable currentState = state

            // Initial render: collapsed
            currentState <-
                App.pumpOnce
                    worldFreezer
                    currentState
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none

            expect {
                snapshot
                    @"
 ▶  Multi-line section                                      |
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

            // Tab to focus
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            currentState <-
                App.pumpOnce
                    worldFreezer
                    currentState
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none

            // Expand
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            currentState <-
                App.pumpOnce
                    worldFreezer
                    currentState
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none

            expect {
                snapshot
                    @"
[▼] Multi-line section                                      |
Line 1 of content                                           |
Line 2 of content                                           |
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
    let ``collapsible with long label text`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let state =
                {
                    CollapsibleState =
                        {
                            IsExpanded = false
                        }
                }

            let longLabel = "This is a very long label that should wrap onto multiple lines"

            let vdom (vdomContext : VdomContext) (state : State) =
                let collapsibleKey = NodeKey.make "collapsible"

                let childContent = Vdom.textContent "Child content here"

                Collapsible.make vdomContext collapsibleKey state.CollapsibleState longLabel childContent

            let haveFrameworkHandleFocus _ = true

            let processWorld =
                { new WorldProcessor<_, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        let mutable newState = state

                        for s in inputs do
                            match s with
                            | WorldStateChange.Keystroke c ->
                                if c.KeyChar = ' ' then
                                    match VdomContext.focusedKey renderState with
                                    | None -> ()
                                    | Some focused ->
                                        NodeKey.toHumanReadableString focused |> shouldEqual "collapsible"

                                        newState <-
                                            {
                                                CollapsibleState =
                                                    {
                                                        IsExpanded = not state.CollapsibleState.IsExpanded
                                                    }
                                            }
                                else
                                    failwith "unexpected key char"
                            | WorldStateChange.MouseEvent _ -> failwith "no mouse events"
                            | WorldStateChange.ApplicationEvent () -> failwith "no app events"
                            | WorldStateChange.KeyboardEvent _ -> failwith "no keyboard events"
                            | WorldStateChange.ApplicationEventException _ -> failwith "no exceptions possible"

                        ProcessWorldResult.make newState
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None
            let mutable currentState = state

            // Initial render: collapsed and unfocused - long text is truncated but on same line as glyph
            currentState <-
                App.pumpOnce
                    worldFreezer
                    currentState
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none

            expect {
                snapshot
                    @"
 ▶  This is a very l|
    ong label that s|
    hould wrap onto |
    multiple lines  |
                    |
                    |
                    |
                    |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }

            // Tab to focus
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            currentState <-
                App.pumpOnce
                    worldFreezer
                    currentState
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none

            expect {
                snapshot
                    @"
[▶] This is a very l|
    ong label that s|
    hould wrap onto |
    multiple lines  |
                    |
                    |
                    |
                    |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }

            // Expand
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            currentState <-
                App.pumpOnce
                    worldFreezer
                    currentState
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none

            expect {
                snapshot
                    @"
[▼] This is a very l|
    ong label that s|
    hould wrap onto |
    multiple lines  |
Child content here  |
                    |
                    |
                    |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }
