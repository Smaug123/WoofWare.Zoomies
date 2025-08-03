namespace WoofWare.Zoomies.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies

type FocusedElement =
    | Toggle1
    | Toggle2

type State =
    {
        mutable IsToggle1Checked : bool
        mutable IsToggle2Checked : bool
        mutable FocusedElement : FocusedElement
    }

    static member Empty () : State =
        {
            IsToggle1Checked = false
            IsToggle2Checked = false
            FocusedElement = FocusedElement.Toggle1
        }

[<TestFixture>]
module TestRender =
    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    let vdom (state : State) : Vdom =
        let left =
            Vdom.textContent
                None
                "not praising the praiseworthy keeps people uncompetitive; not prizing rare treasures keeps people from stealing; not looking at the desirable keeps the mind quiet"
            |> Vdom.bordered

        let right =
            Vdom.textContent
                None
                "errybody wants to be a bodybuilder, but don't nobody want to lift no heavy-ass weights"
            |> Vdom.bordered

        let topHalf = Vdom.panelSplitProportion Direction.Vertical 0.5 left right

        let bottomHalf =
            Vdom.labelledCheckbox
                (fun () -> state.FocusedElement <- FocusedElement.Toggle1)
                state.FocusedElement.IsToggle1
                state.IsToggle1Checked
                "Press Space to toggle"

        let vdom = Vdom.panelSplitAbsolute Direction.Horizontal -3 topHalf bottomHalf

        if state.IsToggle1Checked then
            Vdom.panelSplitProportion
                Direction.Vertical
                0.5
                (Vdom.textContent None "This gets displayed when the thing is checked; left")
                (Vdom.labelledCheckbox
                    (fun () -> state.FocusedElement <- FocusedElement.Toggle2)
                    state.FocusedElement.IsToggle2
                    state.IsToggle2Checked
                    "this one is focusable! space to toggle")
            |> Vdom.panelSplitProportion Direction.Horizontal 0.7 vdom
        else
            vdom

    let processWorld (worldChanges : WorldStateChange seq) (state : State) : unit =
        for change in worldChanges do
            match change with
            | Keystroke c when c.KeyChar = ' ' ->
                match state.FocusedElement with
                | FocusedElement.Toggle1 -> state.IsToggle1Checked <- not state.IsToggle1Checked
                | FocusedElement.Toggle2 -> state.IsToggle2Checked <- not state.IsToggle2Checked
            | Keystroke _ -> ()

    [<Test>]
    let ``there is no rerender if nothing changes`` () =
        let terminalOps = ResizeArray ()

        let console =
            { IConsole.defaultForTests with
                Execute = terminalOps.Add
            }

        let state = State.Empty ()

        let renderState = RenderState.make' console

        Render.oneStep renderState state vdom

        terminalOps.Clear ()

        Render.oneStep renderState state vdom

        terminalOps |> shouldBeEmpty

    [<Test>]
    let ``example 1`` () =
        let console, terminal = ConsoleHarness.make ()

        let keyAvailable, readKey, sendKey = WorldFreezerInputs.make ()

        use worldFreezer = WorldFreezer.listen' keyAvailable readKey

        let state = State.Empty ()

        let renderState = RenderState.make' console

        App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

        expect {
            snapshot
                @"
┌──────────────────────────────────────┐┌──────────────────────────────────────┐|
│not praising the praiseworthy keeps pe││errybody wants to be a bodybuilder, bu│|
│ople uncompetitive; not prizing rare t││t don't nobody want to lift no heavy-a│|
│reasures keeps people from stealing; n││ss weights                            │|
│ot looking at the desirable keeps the ││                                      │|
│mind quiet                            ││                                      │|
└──────────────────────────────────────┘└──────────────────────────────────────┘|
   Press Space to toggle                                                        |
[☐]                                                                             |
                                                                                |
"

            return ConsoleHarness.toString terminal
        }

        // Switching focus does nothing, because there's only one focus element
        sendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

        App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

        expect {
            snapshot
                @"
┌──────────────────────────────────────┐┌──────────────────────────────────────┐|
│not praising the praiseworthy keeps pe││errybody wants to be a bodybuilder, bu│|
│ople uncompetitive; not prizing rare t││t don't nobody want to lift no heavy-a│|
│reasures keeps people from stealing; n││ss weights                            │|
│ot looking at the desirable keeps the ││                                      │|
│mind quiet                            ││                                      │|
└──────────────────────────────────────┘└──────────────────────────────────────┘|
   Press Space to toggle                                                        |
[☐]                                                                             |
                                                                                |
"

            return ConsoleHarness.toString terminal
        }

        // Turn on the toggle, revealing a new interface element!
        sendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
        App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

        expect {
            snapshot
                @"
┌──────────────────────────────────────┐┌──────────────────────────────────────┐|
│not praising the praiseworthy keeps pe││errybody wants to be a bodybuilder, bu│|
│ople uncompetitive; not prizing rare t││t don't nobody want to lift no heavy-a│|
└──────────────────────────────────────┘└──────────────────────────────────────┘|
   Press Space to toggle                                                        |
[☑]                                                                             |
                                                                                |
This gets displayed when the thing is ch   this one is focusable! space to toggl|
ecked; left                              ☐ e                                    |
                                                                                |
"

            return ConsoleHarness.toString terminal
        }

        // Switch to the other checkbox
        sendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
        App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

        expect {
            snapshot
                @"
┌──────────────────────────────────────┐┌──────────────────────────────────────┐|
│not praising the praiseworthy keeps pe││errybody wants to be a bodybuilder, bu│|
│ople uncompetitive; not prizing rare t││t don't nobody want to lift no heavy-a│|
└──────────────────────────────────────┘└──────────────────────────────────────┘|
   Press Space to toggle                                                        |
 ☑                                                                              |
                                                                                |
This gets displayed when the thing is ch   this one is focusable! space to toggl|
ecked; left                             [☐]e                                    |
                                                                                |
"

            return ConsoleHarness.toString terminal
        }

        // Toggle the other one on
        sendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
        App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

        expect {
            snapshot
                @"
┌──────────────────────────────────────┐┌──────────────────────────────────────┐|
│not praising the praiseworthy keeps pe││errybody wants to be a bodybuilder, bu│|
│ople uncompetitive; not prizing rare t││t don't nobody want to lift no heavy-a│|
└──────────────────────────────────────┘└──────────────────────────────────────┘|
   Press Space to toggle                                                        |
 ☑                                                                              |
                                                                                |
This gets displayed when the thing is ch   this one is focusable! space to toggl|
ecked; left                             [☑]e                                    |
                                                                                |
"

            return ConsoleHarness.toString terminal
        }

        // Switch back to the first one
        sendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
        App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

        expect {
            snapshot
                @"
┌──────────────────────────────────────┐┌──────────────────────────────────────┐|
│not praising the praiseworthy keeps pe││errybody wants to be a bodybuilder, bu│|
│ople uncompetitive; not prizing rare t││t don't nobody want to lift no heavy-a│|
└──────────────────────────────────────┘└──────────────────────────────────────┘|
   Press Space to toggle                                                        |
[☑]                                                                             |
                                                                                |
This gets displayed when the thing is ch   this one is focusable! space to toggl|
ecked; left                              ☑ e                                    |
                                                                                |
"

            return ConsoleHarness.toString terminal
        }

        // Disable it again
        sendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
        App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

        expect {
            snapshot
                @"
┌──────────────────────────────────────┐┌──────────────────────────────────────┐|
│not praising the praiseworthy keeps pe││errybody wants to be a bodybuilder, bu│|
│ople uncompetitive; not prizing rare t││t don't nobody want to lift no heavy-a│|
│reasures keeps people from stealing; n││ss weights                            │|
│ot looking at the desirable keeps the ││                                      │|
│mind quiet                            ││                                      │|
└──────────────────────────────────────┘└──────────────────────────────────────┘|
   Press Space to toggle                                                        |
[☐]                                                                             |
                                                                                |
"

            return ConsoleHarness.toString terminal
        }

        // Re-enable; it remembered its state
        sendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
        App.pumpOnce worldFreezer state (fun _ -> true) renderState processWorld vdom

        expect {
            snapshot
                @"
┌──────────────────────────────────────┐┌──────────────────────────────────────┐|
│not praising the praiseworthy keeps pe││errybody wants to be a bodybuilder, bu│|
│ople uncompetitive; not prizing rare t││t don't nobody want to lift no heavy-a│|
└──────────────────────────────────────┘└──────────────────────────────────────┘|
   Press Space to toggle                                                        |
[☑]                                                                             |
                                                                                |
This gets displayed when the thing is ch   this one is focusable! space to toggl|
ecked; left                              ☑ e                                    |
                                                                                |
"

            return ConsoleHarness.toString terminal
        }
