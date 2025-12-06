namespace WoofWare.Zoomies.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPanelSplit =
    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    [<Test>]
    let ``PanelSplit does not repaint background when only child changes`` () =
        let terminalOps = ResizeArray<TerminalOp> ()

        let panelWidth = 15

        let console =
            { IConsole.defaultForTests with
                Execute = fun x -> terminalOps.Add x
                WindowWidth = fun _ -> 2 * panelWidth
                WindowHeight = fun _ -> 3
            }

        let renderState = RenderState.make console MockTime.getStaticUtcNow None

        // Create vdom with a PanelSplit where a bordered child changes
        // Bordered doesn't repaint its entire area, so we can detect if PanelSplit is adding extra repaints
        let vdom (text : string) =
            let left = Vdom.textContent text |> Vdom.bordered
            let right = Vdom.textContent "static" |> Vdom.bordered
            Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, left, right)

        // First render
        Render.oneStep renderState () (fun _ -> vdom "initial")

        // Clear ops from first render
        terminalOps.Clear ()

        // Change only the left child's text content
        Render.oneStep renderState () (fun _ -> vdom "changed")

        // Collect all the cells that were written to
        let writtenCells =
            let result = ResizeArray<int * int> ()

            for i = 0 to terminalOps.Count - 1 do
                match terminalOps.[i] with
                | TerminalOp.MoveCursor (x, y) ->
                    if i + 1 < terminalOps.Count then
                        match terminalOps.[i + 1] with
                        | TerminalOp.WriteChar _ -> result.Add (x, y)
                        | _ -> ()
                | _ -> ()

            result

        // The left bordered text changed, but not the right.
        // (The border cells don't repaint, which means the entire first and last row don't repaint, nor do the leftmost
        // and rightmost single cells of the middle row.)

        // We could do better here by diffing the text so we don't repaint the unchanged characters;
        // there's a TODO for that in the code.
        writtenCells.Count |> shouldEqual (panelWidth - 2)

    [<Test>]
    let ``Keyed PanelSplit clears background on initial render`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let splitKey = NodeKey.make "split"

            let vdom (vdomContext : VdomContext) (showSplit : bool) =
                if showSplit then
                    // A keyed PanelSplit with small children
                    // The background should be cleared
                    let left = Vdom.textContent "L"
                    let right = Vdom.textContent "R"

                    let split =
                        Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, left, right)
                        |> Vdom.withKey splitKey
                    // Wrap in bordered to make it Unkeyed at the top level
                    Vdom.bordered split
                else
                    // Fill the screen with characters to create "artifacts"
                    Vdom.textContent "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" |> Vdom.bordered

            let processWorld =
                { new WorldProcessor<unit, bool> with
                    member _.ProcessWorld (worldChanges, _, state) =
                        // Toggle state on any keystroke
                        let newState = if worldChanges.Length > 0 then not state else state
                        ProcessWorldResult.make newState
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            // First render: fill with X's
            let mutable state =
                App.pumpOnce worldFreezer false (fun _ -> true) renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
┌──────────────────────────────────────┐|
│XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX│|
│XX                                    │|
│                                      │|
└──────────────────────────────────────┘|
"

                return ConsoleHarness.toString terminal
            }

            // Send a keystroke to trigger state change
            world.SendKey (ConsoleKeyInfo ('x', ConsoleKey.NoName, false, false, false))

            // Second render: show keyed PanelSplit
            // The X's should be cleared (replaced with spaces), not left as artifacts
            state <-
                App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
┌──────────────────────────────────────┐|
│L                  R                  │|
│                                      │|
│                                      │|
└──────────────────────────────────────┘|
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``Proportion splits can yield zero-size children`` () =
        task {
            // Demonstrates that proportion splits with small terminals can allocate zero width/height
            let console, _ = ConsoleHarness.make' (fun () -> 1) (fun () -> 1)

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            let leftKey = NodeKey.make "left"
            let rightKey = NodeKey.make "right"

            // Create a vdom with proportion split
            // Terminal width is 1, split at 0.1 proportion
            // This means left gets: int (float 1 * 0.1) = int 0.1 = 0
            // And right gets: 1 - 0 = 1
            let vdom (_ : VdomContext) (_ : FakeUnit) =
                let leftText = Vdom.textContent "left" |> Vdom.withKey leftKey
                let rightText = Vdom.textContent "right" |> Vdom.withKey rightKey
                Vdom.panelSplitProportion (SplitDirection.Vertical, 0.1, leftText, rightText)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            // Render
            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
            |> ignore<FakeUnit>

            // Check that the left child has zero width
            match RenderState.layoutOf leftKey renderState with
            | None -> failwith "should have found leftKey node"
            | Some leftLayout -> leftLayout.Width |> shouldEqual 0

            // Check that the right child has the remaining width
            match RenderState.layoutOf rightKey renderState with
            | None -> failwith "should have found rightKey node"
            | Some rightLayout -> rightLayout.Width |> shouldEqual 1
        }

    [<Test>]
    let ``Horizontal proportion splits can yield zero-size children`` () =
        task {
            // Same as above but for horizontal splits
            let console, _ = ConsoleHarness.make' (fun () -> 10) (fun () -> 2)

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            let topKey = NodeKey.make "top"
            let bottomKey = NodeKey.make "bottom"

            // Create a vdom with horizontal proportion split
            // Terminal height is 2, split at 0.1 proportion
            // This means top gets: int (float 2 * 0.1) = int 0.2 = 0
            // And bottom gets: 2 - 0 = 2
            let vdom (_ : VdomContext) (_ : FakeUnit) =
                let topText = Vdom.textContent "top" |> Vdom.withKey topKey
                let bottomText = Vdom.textContent "bottom" |> Vdom.withKey bottomKey
                Vdom.panelSplitProportion (SplitDirection.Horizontal, 0.1, topText, bottomText)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            // Render
            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
            |> ignore<FakeUnit>

            // Check that the top child has zero height
            match RenderState.layoutOf topKey renderState with
            | None -> failwith "should have found topKey node"
            | Some topLayout -> topLayout.Height |> shouldEqual 0

            // Check that the bottom child has the remaining height
            match RenderState.layoutOf bottomKey renderState with
            | None -> failwith "should have found bottomKey node"
            | Some bottomLayout -> bottomLayout.Height |> shouldEqual 2
        }

    [<Test>]
    let ``panelSplitAuto distributes space normally when all components fit`` () =
        task {
            // Test the normal case where there's enough space for all preferred sizes
            let console, terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Create two text components with different preferred widths
                // "Hello world" has preferred width ~11, "Hi" has preferred width ~2
                let left = Vdom.textContent "Hello world"
                let right = Vdom.textContent "Hi"

                Vdom.panelSplitAuto (SplitDirection.Vertical, left, right)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
Hello world                                                        Hi           |
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
    let ``panelSplitAuto handles stress when not enough space for all preferences`` () =
        task {
            // Test when available space is between minimums and preferences
            // Terminal is 20 wide, but text wants more
            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Long text content that will wrap when space is limited.
                // The longer one (`left`) has its longest word of length 6, vs the shorter one having longest word
                // of length 4, so satisfying their minimum requests allocates more space to `left`;
                // then leftover spare space gets allocated equally between them, and `left` ends up a bit bigger.
                let left = Vdom.textContent "This is a longer piece of text"
                let right = Vdom.textContent "Also long text here"

                Vdom.panelSplitAuto (SplitDirection.Vertical, left, right)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
This is a lAlso long|
onger piece text her|
 of text   e        |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``panelSplitAuto handles extreme stress when not enough space for minimums`` () =
        task {
            // Test when available space is less than sum of minimums
            // This forces proportional scaling of minimums
            let console, terminal = ConsoleHarness.make' (fun () -> 8) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Words with minimum widths that exceed available space
                let left = Vdom.textContent "Hello" // min width ~5
                let right = Vdom.textContent "World" // min width ~5

                Vdom.panelSplitAuto (SplitDirection.Vertical, left, right)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
HellWorl|
o   d   |
        |
        |
        |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``panelSplitAuto works with horizontal splits in normal conditions`` () =
        task {
            // Test horizontal auto splits with enough space
            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Short text (prefers 1 line) and longer text (prefers multiple lines)
                let top = Vdom.textContent "Short"

                let bottom =
                    Vdom.textContent "This is a much longer text that will take multiple lines when rendered"

                Vdom.panelSplitAuto (SplitDirection.Horizontal, top, bottom)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
Short                                   |
This is a much longer text that will tak|
e multiple lines when rendered          |
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
    let ``panelSplitAuto handles horizontal stress when height is limited but still sufficient`` () =
        task {
            // Test horizontal auto splits when not enough vertical space
            let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 4)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Both components want more height than available
                let top = Vdom.textContent "Top section with some content that wraps around"
                let bottom = Vdom.textContent "Bottom section also with content"

                Vdom.panelSplitAuto (SplitDirection.Horizontal, top, bottom)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
Top section with some content |
that wraps around             |
Bottom section also with conte|
nt                            |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``panelSplitAuto with bordered components shows layout adaptation`` () =
        task {
            // Test auto splits with bordered components to make the allocation visible
            let console, terminal = ConsoleHarness.make' (fun () -> 50) (fun () -> 8)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Bordered panels to clearly show space allocation
                let left = Vdom.textContent "Small" |> Vdom.bordered

                let right = Vdom.textContent "This is much larger content" |> Vdom.bordered

                Vdom.panelSplitAuto (SplitDirection.Vertical, left, right)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
┌───────┐┌───────────────────────────────────────┐|
│Small  ││This is much larger content            │|
│       ││                                       │|
│       ││                                       │|
│       ││                                       │|
│       ││                                       │|
│       ││                                       │|
└───────┘└───────────────────────────────────────┘|
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``panelSplitAuto handles zero-width allocation gracefully`` () =
        task {
            // Test extreme stress where one component gets zero space
            let console, terminal = ConsoleHarness.make' (fun () -> 1) (fun () -> 2)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                let left = Vdom.textContent "A"
                let right = Vdom.textContent "B"

                Vdom.panelSplitAuto (SplitDirection.Vertical, left, right)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
            |> ignore<FakeUnit>

            expect {
                snapshot
                    @"
B|
 |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``Unkeyed PanelSplit clears exposed area when child shrinks`` () =
        task {
            // Regression test for: "PanelSplit background only cleared when container node/bounds change"
            // When a PanelSplit rebalances (same container bounds, different child bounds),
            // exposed areas should be cleared
            let console, terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (useLargeSplit : bool) =
                if useLargeSplit then
                    // 50/50 split - left side filled with X's
                    let left = Vdom.textContent (String.replicate 200 "X")
                    let right = Vdom.textContent "right"
                    Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, left, right)
                else
                    // 25/75 split - left side has only "AAA"
                    // Bug: the area between where "AAA" ends and where the old 50% split was
                    // still contained X's from the previous render
                    let left = Vdom.textContent "AAA"
                    let right = Vdom.textContent "right"
                    Vdom.panelSplitProportion (SplitDirection.Vertical, 0.25, left, right)

            let processWorld =
                { new WorldProcessor<unit, bool> with
                    member _.ProcessWorld (worldChanges, _, state) =
                        let newState = if worldChanges.Length > 0 then not state else state
                        ProcessWorldResult.make newState
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            // First render: 50/50 split with X's filling the left side
            let mutable state =
                App.pumpOnce worldFreezer true (fun _ -> true) renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXright                                   |
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                        |
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                        |
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                        |
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                        |
"

                return ConsoleHarness.toString terminal
            }

            // Send keystroke to trigger rebalance
            world.SendKey (ConsoleKeyInfo ('x', ConsoleKey.NoName, false, false, false))

            // Second render: 25/75 split with only "AAA" on left
            state <-
                App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom ActivationResolver.none

            // For reference, although the actual test assertion comes afterwards:
            let secondRender = ConsoleHarness.toString terminal

            expect {
                snapshot
                    @"
AAA                 right                                                       |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
"

                return secondRender
            }

            // The bug: stale X's remain in the area between "AAA" and where the right panel starts
            if secondRender.Contains 'X' then
                failwith $"Bug detected: Second render contains stale X's. %s{secondRender}"
        }

    [<Test>]
    let ``Keyed PanelSplit clears exposed area when child shrinks`` () =
        task {
            // Same as above but with a keyed PanelSplit
            let console, terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let splitKey = NodeKey.make "split"

            let vdom (_ : VdomContext) (useLargeSplit : bool) =
                let split =
                    if useLargeSplit then
                        // 50/50 split
                        let left = Vdom.textContent (String.replicate 200 "X")
                        let right = Vdom.textContent "right"
                        Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, left, right)
                    else
                        // 25/75 split
                        let left = Vdom.textContent "AAA"
                        let right = Vdom.textContent "right"
                        Vdom.panelSplitProportion (SplitDirection.Vertical, 0.25, left, right)

                // Wrap in bordered to make it Unkeyed at the top level
                split |> Vdom.withKey splitKey |> Vdom.bordered

            let processWorld =
                { new WorldProcessor<unit, bool> with
                    member _.ProcessWorld (worldChanges, _, state) =
                        let newState = if worldChanges.Length > 0 then not state else state
                        ProcessWorldResult.make newState
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            // First render: 50/50 split
            let mutable state =
                App.pumpOnce worldFreezer true (fun _ -> true) renderState processWorld vdom ActivationResolver.none

            // Send keystroke to trigger rebalance
            world.SendKey (ConsoleKeyInfo ('x', ConsoleKey.NoName, false, false, false))

            // Second render: 25/75 split
            state <-
                App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom ActivationResolver.none

            let secondRender = ConsoleHarness.toString terminal

            // Bug: the keyed PanelSplit's bounds don't change, so it doesn't clear its background
            // When children are re-laid out, stale content remains in the exposed areas
            if secondRender.Contains 'X' then
                failwith $"Bug detected: Keyed PanelSplit contains stale X's after rebalance. %s{secondRender}"
        }

    [<Test>]
    let ``PanelSplit with Bordered children should not show stale content when rebalancing`` () =
        task {
            // This test demonstrates the bug scenario:
            // PanelSplit containing Bordered panels. When the split rebalances,
            // the PanelSplit's background might not be fully covered by the new child positions.
            let console, terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (useWideLeft : bool) =
                if useWideLeft then
                    // Left side gets 70% - fill it with X's in a bordered panel
                    let left = Vdom.textContent (String.replicate 500 "X") |> Vdom.bordered
                    let right = Vdom.textContent "R" |> Vdom.bordered
                    Vdom.panelSplitProportion (SplitDirection.Vertical, 0.7, left, right)
                else
                    // Left side gets 30% - just add a couple of A's in a bordered panel
                    // The area from 30% to 70% should be cleared, but if the bug exists,
                    // it will still contain X's from the previous render
                    let left = Vdom.textContent "AAA" |> Vdom.bordered
                    let right = Vdom.textContent "R" |> Vdom.bordered
                    Vdom.panelSplitProportion (SplitDirection.Vertical, 0.3, left, right)

            let processWorld =
                { new WorldProcessor<unit, bool> with
                    member _.ProcessWorld (worldChanges, _, state) =
                        let newState = if worldChanges.Length > 0 then not state else state
                        ProcessWorldResult.make newState
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            // First render: 70/30 split with X's on the left
            let mutable state =
                App.pumpOnce worldFreezer true (fun _ -> true) renderState processWorld vdom ActivationResolver.none

            ConsoleHarness.toString terminal |> shouldContainText "XXXX"

            // Send keystroke to trigger rebalance
            world.SendKey (ConsoleKeyInfo ('x', ConsoleKey.NoName, false, false, false))

            // Second render: 30/70 split
            state <-
                App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom ActivationResolver.none

            let secondRender = ConsoleHarness.toString terminal

            // Check if any line contains X's - they should all be cleared
            if secondRender.Contains 'X' then
                failwith $"Bug detected: Stale X's remain after PanelSplit rebalance. %s{secondRender}"
        }

    [<Test>]
    let ``Absolute split with wide non-fixed child violates MinWidth constraint`` () =
        task {
            // Regression test for: "In measureVerticalSplitAbsolute, only the fixed child is constrained"
            // This test exposes the bug where container MinWidth can exceed parent's MaxWidth constraint.
            //
            // Setup: Terminal width = 50
            //   - Absolute vertical split: give child1 20 pixels (positive n=20), child2 gets remainder
            //   - child1: simple text (MinWidth ~5)
            //   - child2: long text in bordered panel (MinWidth ~40)
            //   - Container reports MinWidth = 20 + 40 = 60 > 50 (VIOLATES INVARIANT!)
            //
            // The bug is subtle: it violates documented invariants but may not cause visible artifacts
            // because the arrange phase works with actual allocated space, not reported MinWidth.
            // However, if this absolute split is nested in an Auto split, the Auto split sees
            // the inflated MinWidth and makes incorrect space allocation decisions.

            let console, terminal = ConsoleHarness.make' (fun () -> 50) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Outer Auto split allocates space to its children based on their reported MinWidth
                let left =
                    // This is the problematic absolute split
                    // It's measured with MaxWidth=50 (full terminal width for Auto split)
                    // But it will report MinWidth = 20 + child2.MinWidth
                    let child1 = Vdom.textContent "small"
                    // Long text in bordered panel: needs ~35 chars minimum for longest word "unnecessarily"
                    // Plus 2 for border = ~37
                    let child2 =
                        Vdom.textContent "This text contains unnecessarily long words that wrap"
                        |> Vdom.bordered

                    // Fixed width split: child1 gets 20, child2 gets the rest
                    // When this is measured with MaxWidth=50:
                    // - child1 measured with MaxWidth=min(20,50)=20, reports MinWidth=5
                    // - child2 measured with MaxWidth=50, reports MinWidth=~37
                    // - Container reports MinWidth = 20 + 37 = 57 > 50 ❌ VIOLATES INVARIANT
                    Vdom.panelSplitAbsolute (SplitDirection.Vertical, 20, child1, child2)

                let right = Vdom.textContent "Right side content"

                // Auto split sees left.MinWidth=57, right.MinWidth=18
                // minSum = 75 > 50 (terminal width)
                // Auto split thinks it can't satisfy minimums and scales proportionally
                // This is the wrong decision - the absolute split could actually fit in less space
                Vdom.panelSplitAuto (SplitDirection.Vertical, left, right)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
            |> ignore<FakeUnit>

            // Expected behavior after fix:
            // - Absolute split now constrains child2 to remainder: MaxWidth=50-20=30
            // - child2 (bordered text) reports MinWidth=min(longest_word+2, 30) = 30
            // - Container reports MinWidth = 20 + 30 = 50 (respects constraint ✓)
            // - Auto split sees left.MinWidth=50, right.MinWidth=18, total=68 > 50
            // - Auto split still needs to scale, but makes better decisions
            // - Result: bordered panel is narrower, but "Right side content" is no longer cut off

            expect {
                // This snapshot documents the corrected behavior
                // The layout is now better: "Right side content" displays completely
                snapshot
                    @"
small               ┌──────────────────┐Right side|
                    │This text contains│ content  |
                    │ unnecessarily lon│          |
                    │g words that wrap │          |
                    │                  │          |
                    │                  │          |
                    │                  │          |
                    │                  │          |
                    │                  │          |
                    └──────────────────┘          |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``panelSplitAutoExpand gives all excess to first component`` () =
        task {
            // With panelSplitAutoExpand, the first component should get all excess space
            // while the second stays at its content size
            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // "Left" has preferred width ~4, "Right" has preferred width ~5
                // Total preferred = 9, available = 40, so excess = 31
                // With panelSplitAutoExpand, Left should get all 31 excess (width=35), Right stays at 5
                let left = Vdom.textContent "Left"
                let right = Vdom.textContent "Right"

                Vdom.panelSplitAutoExpand (SplitDirection.Vertical, left, right)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
            |> ignore<FakeUnit>

            // Left should expand to fill most of the space, Right should be exactly 5 chars wide
            expect {
                snapshot
                    @"
Left                               Right|
                                        |
                                        |
                                        |
                                        |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``panelSplitAuto vs panelSplitAutoExpand comparison`` () =
        task {
            // This test demonstrates the difference between Auto and AutoExpand
            // by showing that Auto distributes excess proportionally while AutoExpand gives it all to the first

            // First, test panelSplitAuto
            let consoleAuto, terminalAuto = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)
            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdomAuto (_ : VdomContext) (_ : FakeUnit) =
                let left = Vdom.textContent "Left"
                let right = Vdom.textContent "Right"
                Vdom.panelSplitAuto (SplitDirection.Vertical, left, right)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderStateAuto = RenderState.make consoleAuto MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderStateAuto
                processWorld
                vdomAuto
                ActivationResolver.none
            |> ignore<FakeUnit>

            // Now test panelSplitAutoExpand
            let consoleExpand, terminalExpand =
                ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let vdomExpand (_ : VdomContext) (_ : FakeUnit) =
                let left = Vdom.textContent "Left"
                let right = Vdom.textContent "Right"
                Vdom.panelSplitAutoExpand (SplitDirection.Vertical, left, right)

            let renderStateExpand = RenderState.make consoleExpand MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderStateExpand
                processWorld
                vdomExpand
                ActivationResolver.none
            |> ignore<FakeUnit>

            // With panelSplitAuto, excess is distributed proportionally by preferred width
            // Left (4) : Right (5) ratio, so Left gets 4 + int(31*4/9) = 17, Right gets 23
            expect {
                snapshot
                    @"
Left             Right                  |
                                        |
                                        |
"

                return ConsoleHarness.toString terminalAuto
            }

            // With panelSplitAutoExpand, Left gets ALL the excess, Right stays at 5
            expect {
                snapshot
                    @"
Left                               Right|
                                        |
                                        |
"

                return ConsoleHarness.toString terminalExpand
            }
        }

    [<Test>]
    let ``panelSplitAutoExpand horizontal respects max height constraints`` () =
        task {
            // Test horizontal split - when both components have max height constraints,
            // they are clamped and excess space is unused
            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Both components have preferred height of 1 and max height of 1 (text content)
                // Total preferred = 2, available = 10, so excess = 8
                // With panelSplitAutoExpand, Top would get all excess, but max height clamps it to 1
                // Bottom also has max height 1, so gets clamped to 1
                // The 8 rows of excess are unused container space
                let top = Vdom.textContent "Top"
                let bottom = Vdom.textContent "Bottom"

                Vdom.panelSplitAutoExpand (SplitDirection.Horizontal, top, bottom)

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
            |> ignore<FakeUnit>

            // Both Top and Bottom have max height of 1, so each gets exactly 1 row.
            // The remaining 8 rows are unused container space (not assigned to either child).
            expect {
                snapshot
                    @"
Top                 |
Bottom              |
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
    let ``AutoWeighted with Fixed 0 on both sides gives each their preferred size`` () =
        task {
            // When both sides have Fixed 0.0 weight, neither wants excess
            // Each should get exactly their preferred size, with remaining space unused
            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                let left = Vdom.textContent "Left"
                let right = Vdom.textContent "Right"

                // Use the raw panelSplit with explicit weights
                Vdom.panelSplit (
                    SplitDirection.Vertical,
                    SplitBehaviour.AutoWeighted (ExpansionWeight.Fixed 0.0, ExpansionWeight.Fixed 0.0),
                    left,
                    right
                )
                |> Vdom.Unkeyed

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
            |> ignore<FakeUnit>

            // Each should get exactly their preferred width: Left=4, Right=5
            // Total = 9, leaving 31 as unassigned container space to the right of both children
            expect {
                snapshot
                    @"
LeftRight                               |
                                        |
                                        |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``horizontal AutoWeighted clamps second child height to its max`` () =
        task {
            // Regression test for: second child's height was not clamped to maxHeight
            // With a 10-row container and two 1-row max-height children,
            // the second child should get 1 row (clamped), not 9 rows (unclamped)
            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // First child has max height 1 (text content) and weight 0
                // Second child has max height 1 (text content) and weight 1 (gets excess)
                // Even though second child wants excess, it should be clamped to max height 1
                let top = Vdom.textContent "Top"
                let bottom = Vdom.textContent "Bottom"

                // Use AutoWeighted with first=0, second=1 so second child would get all excess
                Vdom.panelSplit (
                    SplitDirection.Horizontal,
                    SplitBehaviour.AutoWeighted (ExpansionWeight.Fixed 0.0, ExpansionWeight.Fixed 1.0),
                    top,
                    bottom
                )
                |> Vdom.Unkeyed

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
            |> ignore<FakeUnit>

            // Second child should be clamped to 1 row despite wanting all excess
            // Top on row 0, Bottom on row 1, rows 2-9 are unused (cleared)
            expect {
                snapshot
                    @"
Top                 |
Bottom              |
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
    let ``PanelSplit clears unallocated container space when children don't fill bounds`` () =
        task {
            // Regression test for: unallocated container space was not cleared
            // When both children have max heights that don't fill the container,
            // the remaining space should be cleared (not show stale content)
            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            // State: true = show full content, false = show minimal content
            let vdom (_ : VdomContext) (showContent : bool) =
                if showContent then
                    // First render: show content that fills the container
                    let top = Vdom.textContent "Line1"
                    let middle = Vdom.textContent "Line2"
                    let bottom = Vdom.textContent "Line3"

                    Vdom.panelSplitAuto (
                        SplitDirection.Horizontal,
                        top,
                        Vdom.panelSplitAuto (SplitDirection.Horizontal, middle, bottom)
                    )
                else
                    // Second render: show less content (children don't fill container)
                    // The old "Line2" and "Line3" should be cleared
                    let top = Vdom.textContent "OnlyThis"
                    Vdom.panelSplitAuto (SplitDirection.Horizontal, top, Vdom.empty)

            let processWorld =
                { new WorldProcessor<unit, bool> with
                    member _.ProcessWorld (worldChanges, _, state) =
                        // Toggle state when any key is pressed
                        let newState = if worldChanges.Length > 0 then not state else state
                        ProcessWorldResult.make newState
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None
            let mutable state = true

            // First render with content
            state <-
                App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom ActivationResolver.none

            expect {
                snapshot
                    @"
Line1               |
Line2               |
Line3               |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }

            // Press any key to toggle state and trigger re-render with less content
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            state <-
                App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom ActivationResolver.none

            // "Line2" and "Line3" should be cleared, only "OnlyThis" remains
            expect {
                snapshot
                    @"
OnlyThis            |
                    |
                    |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``vertical AutoWeighted clamps width to MaxWidth like horizontal clamps height`` () =
        task {
            // Regression test for: vertical AutoWeighted does not clamp widths to MaxWidth
            // but horizontal AutoWeighted does clamp heights to MaxHeight.
            // This test verifies that both dimensions are handled symmetrically.
            //
            // Setup:
            // - First child: minW=10, prefW=30, maxW=12
            // - Second child: minW=1, prefW=10, maxW=None
            // - Container width = 20
            //
            // Measurement phase clamps first child's PreferredWidth to MaxWidth:
            // - m1.PreferredWidth = min(30, 12) = 12
            // - m2.PreferredWidth = 10
            // - totalPref = 22, minSum = 11
            //
            // bounds.Width=20 is in the "between" branch (11 <= 20 <= 22).
            // Without the fix:
            // - p = 12/22 = 0.545
            // - remainder = 20 - 11 = 9
            // - w1 = 10 + int(9 * 0.545) = 14 (EXCEEDS maxW=12!)
            //
            // With the fix, w1 should be clamped to min(14, 12) = 12.
            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Create first child with maxWidth=12 using FlexibleContent
                // The proportional calculation will give it 14 columns, which exceeds maxWidth
                let leftMeasure (_ : MeasureConstraints) =
                    {
                        MinWidth = 10
                        PreferredWidth = 30
                        MaxWidth = Some 12
                        MinHeightForWidth = fun _ -> 1
                        PreferredHeightForWidth = fun _ -> 1
                        MaxHeightForWidth = fun _ -> None
                    }

                let leftRender (bounds : Rectangle) =
                    // Render "L" repeated to show how many columns we got
                    Vdom.textContent (String.replicate bounds.Width "L")

                let left = Vdom.flexibleContent leftMeasure leftRender

                // Second child has lower prefWidth
                let rightMeasure (_ : MeasureConstraints) =
                    {
                        MinWidth = 1
                        PreferredWidth = 10
                        MaxWidth = None
                        MinHeightForWidth = fun _ -> 1
                        PreferredHeightForWidth = fun _ -> 1
                        MaxHeightForWidth = fun _ -> None
                    }

                let rightRender (bounds : Rectangle) =
                    Vdom.textContent (String.replicate bounds.Width "R")

                let right = Vdom.flexibleContent rightMeasure rightRender

                // Use vertical AutoWeighted with default weights
                Vdom.panelSplit (
                    SplitDirection.Vertical,
                    SplitBehaviour.AutoWeighted (ExpansionWeight.FromContent, ExpansionWeight.FromContent),
                    left,
                    right
                )
                |> Vdom.Unkeyed

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
            |> ignore<FakeUnit>

            // First child should be clamped to 12 columns (its maxWidth)
            // So we should see LLLLLLLLLLLL (12 L's) followed by RRRRRRRR (8 R's)
            expect {
                snapshot
                    @"
LLLLLLLLLLLLRRRRRRRR|
                    |
                    |
                    |
                    |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``horizontal AutoWeighted clamps first child height to its max`` () =
        task {
            // Regression test for: first child's height was not clamped to maxHeight
            // when bounds.Height is between minSum and totalPref (lines 1036-1046 branch).
            //
            // Setup:
            // - First child: minH=1, prefH=10, maxH=2
            // - Second child: minH=1, prefH=10, maxH=None
            // - Container height = 10
            //
            // With totalPref=20 and minSum=2, bounds.Height=10 is in the "between" branch.
            // Without the fix, h1 = 1 + int(8 * 0.5) = 5, which exceeds maxH1=2.
            // With the fix, h1 should be clamped to 2.
            let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (_ : FakeUnit) =
                // Create first child with maxHeight=2 using FlexibleContent
                // It has prefHeight=10 but maxHeight=2, so it should be clamped
                let topMeasure (_ : MeasureConstraints) =
                    {
                        MinWidth = 1
                        PreferredWidth = 20
                        MaxWidth = None
                        MinHeightForWidth = fun _ -> 1
                        PreferredHeightForWidth = fun _ -> 10
                        MaxHeightForWidth = fun _ -> Some 2
                    }

                let topRender (bounds : Rectangle) =
                    // Render "T" on each row to show how many rows we got
                    let lines = [ for _ in 1 .. bounds.Height -> "T" ]
                    Vdom.textContent (String.concat "\n" lines)

                let top = Vdom.flexibleContent topMeasure topRender

                // Second child has high prefHeight but no maxHeight constraint
                let bottomMeasure (_ : MeasureConstraints) =
                    {
                        MinWidth = 1
                        PreferredWidth = 20
                        MaxWidth = None
                        MinHeightForWidth = fun _ -> 1
                        PreferredHeightForWidth = fun _ -> 10
                        MaxHeightForWidth = fun _ -> None
                    }

                let bottomRender (bounds : Rectangle) =
                    let lines = [ for _ in 1 .. bounds.Height -> "B" ]
                    Vdom.textContent (String.concat "\n" lines)

                let bottom = Vdom.flexibleContent bottomMeasure bottomRender

                // Use AutoWeighted with default weights
                Vdom.panelSplit (
                    SplitDirection.Horizontal,
                    SplitBehaviour.AutoWeighted (ExpansionWeight.FromContent, ExpansionWeight.FromContent),
                    top,
                    bottom
                )
                |> Vdom.Unkeyed

            let processWorld =
                { new WorldProcessor<unit, FakeUnit> with
                    member _.ProcessWorld (worldChanges, _, state) = ProcessWorldResult.make state
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
            |> ignore<FakeUnit>

            // First child should be clamped to 2 rows (its maxHeight)
            // So we should see T on rows 0-1, B on rows 2-9
            expect {
                snapshot
                    @"
T                   |
T                   |
B                   |
B                   |
B                   |
B                   |
B                   |
B                   |
B                   |
B                   |
"

                return ConsoleHarness.toString terminal
            }
        }
