namespace WoofWare.Zoomies.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCheckbox =
    [<Test>]
    let ``Checkbox with focus does not write brackets when Height is 0`` () =
        task {
            // Regression test for: "Checkbox focus brackets can write with Height=0"
            // The guard should check bounds.Width >= 3 && bounds.Height > 0 before drawing brackets
            let terminalOps = ResizeArray<TerminalOp> ()

            let console =
                { IConsole.defaultForTests with
                    Execute = fun x -> terminalOps.Add x
                    WindowWidth = fun _ -> 10
                    WindowHeight = fun _ -> 5
                }

            let renderState = MockTime.makeRenderStateStatic<unit> console None

            let checkboxKey = NodeKey.make "checkbox"

            // Create a vdom where the checkbox has focus and is allocated bounds with Height=0
            // We use an absolute split to force the checkbox into a zero-height allocation
            let vdom (vdomContext : IVdomContext<_>) (_ : FakeUnit) =
                let topContent = Vdom.textContent "top"

                let checkbox = Components.Checkbox.make (vdomContext, checkboxKey, false)

                // Give the checkbox 0 rows (split at row 5 in a 5-row terminal)
                Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 5, topContent, checkbox)

            let processWorld = WorldProcessor.passthrough

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            // Render without focus
            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<FakeUnit>

            terminalOps.Clear ()

            // Tab to give focus to the checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            App.pumpOnce
                worldFreezer
                (FakeUnit.fake ())
                (fun _ -> true)
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<FakeUnit>

            // Verify focus actually moved to the checkbox
            RenderState.focusedKey renderState |> shouldEqual (Some checkboxKey)

            // Check that the checkbox has bounds with Height=0
            let checkboxLayout = RenderState.layoutOf checkboxKey renderState
            checkboxLayout.IsSome |> shouldEqual true
            checkboxLayout.Value.Height |> shouldEqual 0

            // The bug would cause bracket characters '[' or ']' to be written at invalid positions
            // With the fix, no brackets should be written when Height=0
            let hasBrackets =
                terminalOps
                |> Seq.exists (
                    function
                    | TerminalOp.WriteRun (text, _, _, _) when text.Contains '[' || text.Contains ']' -> true
                    | _ -> false
                )

            hasBrackets |> shouldEqual false
        }
