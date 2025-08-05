namespace WoofWare.Zoomies.Test

open System
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies

[<TestFixture>]
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
        let console, terminal = ConsoleHarness.make' (fun () -> 16) (fun () -> 1)

        let world = MockWorld.make ()

        let worldFreezer = WorldFreezer.listen' world.KeyAvailable world.ReadKey

        let renderState = RenderState.make' console
        let state = ref 0
        let haveFrameworkHandleFocus _ = true

        let processWorld (inputs : WorldStateChange seq) _ =
            inputs
            |> Seq.map (fun s ->
                match s with
                | WorldStateChange.Keystroke c -> string c.Key
            )
            |> String.concat "\n"
            |> failwithf "should not call: %s"

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
