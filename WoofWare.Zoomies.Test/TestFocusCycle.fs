namespace WoofWare.Zoomies.Test

open System
open System.Text
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

    let vdom (state : int ref) =
        List.init 4 (fun i -> Vdom.checkbox (fun () -> state.Value <- i) (state.Value = i) false)
        |> List.reduce (Vdom.panelSplitAbsolute Direction.Vertical -3)

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
            let state = ref 0
            let haveFrameworkHandleFocus _ = true

            let processWorld =
                { new WorldProcessor<_, _> with
                    member _.ProcessWorld (inputs, _, _) =
                        let sb = StringBuilder ()

                        for s in inputs do
                            match s with
                            | WorldStateChange.Keystroke c -> string c.Key
                            | WorldStateChange.MouseEvent _ -> failwith "no mouse events"
                            | WorldStateChange.ApplicationEvent () -> failwith "no app events"
                            | WorldStateChange.KeyboardEvent _ -> failwith "no keyboard events"
                            | WorldStateChange.ApplicationEventException _ -> failwith "no exceptions possible"
                            |> sb.AppendLine
                            |> ignore<StringBuilder>

                        failwithf $"should not call: %O{sb}"
                }

            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
  [☐]   ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
   ☐   [☐] ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
   ☐    ☐ [☐] ☐ |
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
   ☐    ☐  ☐ [☐]|
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
  [☐]   ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }
        }
