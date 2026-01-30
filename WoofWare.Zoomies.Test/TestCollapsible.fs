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

    type CollapsibleEvent = | ToggleCollapsible

    [<Test>]
    let ``collapsible toggles between collapsed and expanded states`` () =
        task {
            let collapsibleKey = NodeKey.make "collapsible"

            let vdom (vdomContext : IVdomContext<_>) (state : State) =
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

            let initial =
                {
                    CollapsibleState = Collapsible.State.Collapsed
                }

            let transition (state : State) (event : CollapsibleEvent) : State =
                match event with
                | ToggleCollapsible ->
                    { state with
                        CollapsibleState = state.CollapsibleState.ToggledExpansion ()
                    }

            let activationResolver : ActivationResolver<CollapsibleEvent, State> =
                ActivationResolver (fun key keyInfo _state ->
                    if keyInfo.KeyChar = ' ' && key = collapsibleKey then
                        Some ToggleCollapsible
                    else
                        None
                )

            let config =
                AppConfig.simple initial transition (App.pureView vdom) activationResolver

            use ctx = IncrTestContext.make console config None

            // Initial render: collapsed and unfocused
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            let initial =
                {
                    CollapsibleState = Collapsible.State.Collapsed
                }

            let collapsibleKey = NodeKey.make "collapsible"

            let vdom (vdomContext : IVdomContext<_>) (state : State) =
                let childContent =
                    let line1 =
                        Vdom.textContent "Line 1 of content" |> Vdom.withKey (NodeKey.make "line1")

                    let line2 =
                        Vdom.textContent "Line 2 of content" |> Vdom.withKey (NodeKey.make "line2")

                    Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 1, line1, line2)

                Collapsible.make vdomContext collapsibleKey state.CollapsibleState "Multi-line section" childContent

            let transition (state : State) (event : CollapsibleEvent) : State =
                match event with
                | ToggleCollapsible ->
                    { state with
                        CollapsibleState = state.CollapsibleState.ToggledExpansion ()
                    }

            let activationResolver : ActivationResolver<CollapsibleEvent, State> =
                ActivationResolver (fun key keyInfo _state ->
                    if keyInfo.KeyChar = ' ' && key = collapsibleKey then
                        Some ToggleCollapsible
                    else
                        None
                )

            let config =
                AppConfig.simple initial transition (App.pureView vdom) activationResolver

            use ctx = IncrTestContext.make console config None

            // Initial render: collapsed
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // Expand
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            let initial =
                {
                    CollapsibleState =
                        {
                            IsExpanded = false
                        }
                }

            let longLabel = "This is a very long label that should wrap onto multiple lines"

            let collapsibleKey = NodeKey.make "collapsible"

            let vdom (vdomContext : IVdomContext<_>) (state : State) =
                let childContent = Vdom.textContent "Child content here"

                Collapsible.make vdomContext collapsibleKey state.CollapsibleState longLabel childContent

            let transition (state : State) (event : CollapsibleEvent) : State =
                match event with
                | ToggleCollapsible ->
                    { state with
                        CollapsibleState =
                            {
                                IsExpanded = not state.CollapsibleState.IsExpanded
                            }
                    }

            let activationResolver : ActivationResolver<CollapsibleEvent, State> =
                ActivationResolver (fun key keyInfo _state ->
                    if keyInfo.KeyChar = ' ' && key = collapsibleKey then
                        Some ToggleCollapsible
                    else
                        None
                )

            let config =
                AppConfig.simple initial transition (App.pureView vdom) activationResolver

            use ctx = IncrTestContext.make console config None

            // Initial render: collapsed and unfocused - long label wraps across multiple lines
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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
