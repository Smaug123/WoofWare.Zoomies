namespace WoofWare.Zoomies.Test

open System
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFocusCycle =
    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    let vdom (renderState : RenderState) (checkboxes : bool[]) =
        let currentFocus = RenderState.focusedKey renderState

        List.init
            4
            (fun i ->
                let key = NodeKey.make $"checkbox{i}"

                Vdom.checkbox (currentFocus = Some key) checkboxes.[i]
                |> Vdom.withKey key
                |> Vdom.focusable
            )
        |> List.reduce (fun x y -> Vdom.panelSplitAbsolute (Direction.Vertical, -3, x, y))

    [<Test>]
    let ``example 1`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 16) (fun () -> 1)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let renderState = RenderState.make' console
            let state = [| false ; false ; false ; false |]
            let haveFrameworkHandleFocus _ = true

            let processWorld =
                { new WorldProcessor<_, bool[]> with
                    member _.ProcessWorld (inputs, renderState, checkboxes) =
                        for s in inputs do
                            match s with
                            | WorldStateChange.Keystroke c ->
                                if c.KeyChar = ' ' then
                                    match RenderState.focusedKey renderState with
                                    | None ->
                                        // pressed space while nothing focused
                                        ()
                                    | Some focused ->
                                        let key = NodeKey.toString focused
                                        let prefix = "checkbox"

                                        if key.StartsWith (prefix, StringComparison.Ordinal) then
                                            let key = key.Substring prefix.Length |> Int32.Parse
                                            checkboxes.[key] <- not checkboxes.[key]
                                        else
                                            failwith "unexpected key"
                                else
                                    failwith "unexpected key char"
                            | WorldStateChange.MouseEvent _ -> failwith "no mouse events"
                            | WorldStateChange.ApplicationEvent () -> failwith "no app events"
                            | WorldStateChange.KeyboardEvent _ -> failwith "no keyboard events"
                            | WorldStateChange.ApplicationEventException _ -> failwith "no exceptions possible"
                }

            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
   ☐    ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            // Nothing focused, so space does nothing
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
   ☐    ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            // Move focus to the first focusable element
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
  [☐]   ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
  [☑]   ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
   ☑   [☐] ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
   ☑   [☑] ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
   ☑    ☑ [☐] ☐ |
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
   ☑    ☑ [☑] ☐ |
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
   ☑    ☑  ☑ [☐]|
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
   ☑    ☑  ☑ [☑]|
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
  [☑]   ☑  ☑  ☑ |
"

                return ConsoleHarness.toString terminal
            }
        }
