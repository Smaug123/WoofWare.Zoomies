namespace WoofWare.Zoomies.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestBordered =
    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    [<Test>]
    let ``Bordered does not repaint border when only child changes`` () =
        let terminalOps = ResizeArray<TerminalOp> ()

        let panelWidth = 20

        let console =
            { IConsole.defaultForTests with
                Execute = fun x -> terminalOps.Add x
                WindowWidth = fun _ -> panelWidth
                WindowHeight = fun _ -> 3
            }

        let renderState = RenderState.make console MockTime.getStaticUtcNow None

        // Create vdom with bordered text that can change
        let vdom (content : string) =
            Vdom.textContent content |> Vdom.bordered

        // First render
        Render.oneStep renderState () (fun _ -> vdom "initial text")

        // Clear ops from first render
        terminalOps.Clear ()

        // Change only the text content inside the border
        Render.oneStep renderState () (fun _ -> vdom "changed text")

        // Collect all the cells that were written to, tracking cursor position
        // (cursor advances automatically after each write, and MoveCursor may be skipped for consecutive cells)
        let writtenCells = ResizeArray<int * int> ()
        let mutable cursorX = 0
        let mutable cursorY = 0

        for op in terminalOps do
            match op with
            | TerminalOp.MoveCursor (x, y) ->
                cursorX <- x
                cursorY <- y
            | TerminalOp.WriteRun (text, _, _) ->
                for _ in text do
                    writtenCells.Add (cursorX, cursorY)
                    cursorX <- cursorX + 1
            | _ -> ()

        // The text content changed, so we should write to text cells
        // But we should NOT repaint the border (which would be the perimeter cells)

        let borderCells =
            writtenCells
            |> Seq.filter (fun (x, y) -> x = 0 || x = panelWidth - 1 || y = 0 || y = 2)
            |> Seq.length

        // We should not have written to any border cells
        borderCells |> shouldEqual 0

        // And we changed the right number of cells: just the middle row, because that's the one that doesn't contain
        // the border.
        // Again, this will get smaller when we've implemented a more efficient text render with diffing.
        writtenCells.Count |> shouldEqual (panelWidth - 2)

    [<Test>]
    let ``Keyed Bordered draws its border`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 40) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let borderedKey = NodeKey.make "bordered"

            let vdom (vdomContext : VdomContext) (showBordered : bool) =
                if showBordered then
                    // A keyed Bordered with small text content
                    // The border should be drawn
                    let content = Vdom.textContent "content"

                    let keyedBordered = Vdom.bordered content |> Vdom.withKey borderedKey
                    // Wrap in another bordered to make it Unkeyed at the top level
                    Vdom.bordered keyedBordered
                else
                    // Fill the screen with characters to create "artifacts"
                    Vdom.textContent "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

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
                App.pumpOnce
                    worldFreezer
                    false
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            expect {
                snapshot
                    @"
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX|
                                        |
                                        |
                                        |
                                        |
"

                return ConsoleHarness.toString terminal
            }

            // Send a keystroke to trigger state change
            world.SendKey (ConsoleKeyInfo ('x', ConsoleKey.NoName, false, false, false))

            // Second render: show keyed Bordered
            // The border should be drawn, and the X's should be cleared
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            expect {
                snapshot
                    @"
┌──────────────────────────────────────┐|
│┌────────────────────────────────────┐│|
││content                             ││|
│└────────────────────────────────────┘│|
└──────────────────────────────────────┘|
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``Unkeyed Bordered clears exposed area when child content shrinks`` () =
        task {
            // Test for Bordered containers with shrinking child content
            let console, terminal = ConsoleHarness.make' (fun () -> 60) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (_ : VdomContext) (useLongText : bool) =
                if useLongText then
                    // Long text filling the bordered area
                    Vdom.textContent (String.replicate 200 "X") |> Vdom.bordered
                else
                    // Short text - exposed area should be cleared
                    Vdom.textContent "AAA" |> Vdom.bordered

            let processWorld =
                { new WorldProcessor<unit, bool> with
                    member _.ProcessWorld (worldChanges, _, state) =
                        let newState = if worldChanges.Length > 0 then not state else state
                        ProcessWorldResult.make newState
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            // First render: long text
            let mutable state =
                App.pumpOnce
                    worldFreezer
                    true
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            expect {
                snapshot
                    @"
┌──────────────────────────────────────────────────────────┐|
│XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX│|
│XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX│|
│XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX│|
└──────────────────────────────────────────────────────────┘|
"

                return ConsoleHarness.toString terminal
            }

            // Send keystroke to change content
            world.SendKey (ConsoleKeyInfo ('x', ConsoleKey.NoName, false, false, false))

            // Second render: short text
            // Border bounds unchanged, child bounds unchanged, but content shrinks
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            expect {
                snapshot
                    @"
┌──────────────────────────────────────────────────────────┐|
│AAA                                                       │|
│                                                          │|
│                                                          │|
└──────────────────────────────────────────────────────────┘|
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``Keyed Bordered clears exposed area when child content shrinks`` () =
        task {
            // Same as above but with a keyed Bordered
            let console, terminal = ConsoleHarness.make' (fun () -> 60) (fun () -> 5)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let borderedKey = NodeKey.make "bordered"

            let vdom (_ : VdomContext) (useLongText : bool) =
                let bordered =
                    if useLongText then
                        Vdom.textContent (String.replicate 200 "X") |> Vdom.bordered
                    else
                        Vdom.textContent "AAA" |> Vdom.bordered

                // Wrap in another bordered to make it Unkeyed at the top level
                bordered |> Vdom.withKey borderedKey |> Vdom.bordered

            let processWorld =
                { new WorldProcessor<unit, bool> with
                    member _.ProcessWorld (worldChanges, _, state) =
                        let newState = if worldChanges.Length > 0 then not state else state
                        ProcessWorldResult.make newState
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            // First render: long text
            let mutable state =
                App.pumpOnce
                    worldFreezer
                    true
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            expect {
                snapshot
                    @"
┌──────────────────────────────────────────────────────────┐|
│┌────────────────────────────────────────────────────────┐│|
││XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX││|
│└────────────────────────────────────────────────────────┘│|
└──────────────────────────────────────────────────────────┘|
"

                return ConsoleHarness.toString terminal
            }

            // Send keystroke to change content
            world.SendKey (ConsoleKeyInfo ('x', ConsoleKey.NoName, false, false, false))

            // Second render: short text
            state <-
                App.pumpOnce
                    worldFreezer
                    state
                    (fun _ -> true)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            expect {
                snapshot
                    @"
┌──────────────────────────────────────────────────────────┐|
│┌────────────────────────────────────────────────────────┐│|
││AAA                                                     ││|
│└────────────────────────────────────────────────────────┘│|
└──────────────────────────────────────────────────────────┘|
"

                return ConsoleHarness.toString terminal
            }
        }
