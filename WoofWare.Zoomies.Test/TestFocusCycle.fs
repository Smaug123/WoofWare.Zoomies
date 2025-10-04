namespace WoofWare.Zoomies.Test

open System
open FsUnitTyped
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

    let vdom (previousTickRenderState : RenderState) (checkboxes : bool[]) =
        let currentFocus = RenderState.focusedKey previousTickRenderState

        List.init
            4
            (fun i ->
                let key = NodeKey.make $"checkbox%i{i}"

                Vdom.checkbox (currentFocus = Some key) (Array.get checkboxes i)
                |> Vdom.withKey key
                |> Vdom.withFocusTracking
            )
        |> List.reduce (fun x y -> Vdom.panelSplitAbsolute (SplitDirection.Vertical, -3, x, y))

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
                                            Array.set checkboxes key (Array.get checkboxes key |> not)
                                        else
                                            failwith "unexpected key"
                                else
                                    failwith "unexpected key char"
                            | WorldStateChange.MouseEvent _ -> failwith "no mouse events"
                            | WorldStateChange.ApplicationEvent () -> failwith "no app events"
                            | WorldStateChange.KeyboardEvent _ -> failwith "no keyboard events"
                            | WorldStateChange.ApplicationEventException _ -> failwith "no exceptions possible"
                }

            let renderState = RenderState.make' console
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

    [<Test>]
    let ``shift+tab cycles backward`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 16) (fun () -> 1)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

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
                                    | None -> ()
                                    | Some focused ->
                                        let key = NodeKey.toString focused
                                        let prefix = "checkbox"

                                        if key.StartsWith (prefix, StringComparison.Ordinal) then
                                            let key = key.Substring prefix.Length |> Int32.Parse
                                            Array.set checkboxes key (Array.get checkboxes key |> not)
                                        else
                                            failwith "unexpected key"
                                else
                                    failwith "unexpected key char"
                            | WorldStateChange.MouseEvent _ -> failwith "no mouse events"
                            | WorldStateChange.ApplicationEvent () -> failwith "no app events"
                            | WorldStateChange.KeyboardEvent _ -> failwith "no keyboard events"
                            | WorldStateChange.ApplicationEventException _ -> failwith "no exceptions possible"
                }

            let renderState = RenderState.make' console
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
   ☐    ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            // Tab to focus first checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
  [☐]   ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            // Tab to focus second checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
   ☐   [☐] ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            // Shift+Tab to go back to first checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, true, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
  [☐]   ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            // Shift+Tab from first should wrap to last
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, true, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
   ☐    ☐  ☐ [☐]|
"

                return ConsoleHarness.toString terminal
            }

            // Check the last checkbox
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
   ☐    ☐  ☐ [☑]|
"

                return ConsoleHarness.toString terminal
            }

            // Shift+Tab to third checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, true, false, false))
            App.pumpOnce worldFreezer state haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
   ☐    ☐ [☐] ☑ |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``focus tracks the key when node keys are reassigned, not the element`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 16) (fun () -> 1)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            // State tracks which element to render at a given key
            let haveFrameworkHandleFocus _ = true

            let vdom (previousTickRenderState : RenderState) (renderCheckbox1 : bool ref) =
                let currentFocus = RenderState.focusedKey previousTickRenderState
                let sharedKey = NodeKey.make "shared-key"

                if renderCheckbox1.Value then
                    // First frame: checkbox at shared-key
                    let checkbox1 =
                        Vdom.checkbox (currentFocus = Some sharedKey) false
                        |> Vdom.withKey sharedKey
                        |> Vdom.withFocusTracking

                    let checkbox2 = Vdom.checkbox false false

                    Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, checkbox1, checkbox2)
                else
                    // Second frame: different checkbox at shared-key
                    let checkbox1 = Vdom.checkbox false false

                    let checkbox2 =
                        Vdom.checkbox (currentFocus = Some sharedKey) false
                        |> Vdom.withKey sharedKey
                        |> Vdom.withFocusTracking

                    Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, checkbox1, checkbox2)

            let processWorld =
                { new WorldProcessor<_, bool ref> with
                    member _.ProcessWorld (inputs, _, _) =
                        for s in inputs do
                            match s with
                            | WorldStateChange.Keystroke _ -> ()
                            | WorldStateChange.MouseEvent _ -> failwith "no mouse events"
                            | WorldStateChange.ApplicationEvent () -> failwith "no app events"
                            | WorldStateChange.KeyboardEvent _ -> failwith "no keyboard events"
                            | WorldStateChange.ApplicationEventException _ -> failwith "no exceptions possible"
                }

            let renderState = RenderState.make' console
            let renderCheckbox1 = ref true

            App.pumpOnce worldFreezer renderCheckbox1 haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
    ☐       ☐   |
"

                return ConsoleHarness.toString terminal
            }

            // Tab to focus the checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            App.pumpOnce worldFreezer renderCheckbox1 haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
   [☐]      ☐   |
"

                return ConsoleHarness.toString terminal
            }

            // Now reassign the key to a different element
            renderCheckbox1.Value <- false
            App.pumpOnce worldFreezer renderCheckbox1 haveFrameworkHandleFocus renderState processWorld vdom

            // Focus should remain on the element with shared-key, even though it's a different element
            expect {
                snapshot
                    @"
    ☐      [☐]  |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``key reassignment to non-focusable element loses focus`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 16) (fun () -> 1)

            let world = MockWorld.make ()

            let vdom (previousTickRenderState : RenderState) (tick : int ref) =
                let currentFocus = RenderState.focusedKey previousTickRenderState
                let sharedKey = NodeKey.make "shared-key"

                match tick.Value with
                | 0 ->
                    // First frame: focusable checkbox at shared-key
                    Vdom.checkbox (currentFocus = Some sharedKey) false
                    |> Vdom.withKey sharedKey
                    |> Vdom.withFocusTracking
                | 1 ->
                    // Second frame: non-focusable element.
                    // The previous render had focus on the key `sharedKey`.
                    let nonFocusable =
                        Vdom.checkbox (currentFocus = Some sharedKey) false |> Vdom.withKey sharedKey

                    Vdom.panelSplitProportion (
                        SplitDirection.Vertical,
                        0.5,
                        Vdom.textContent false "more",
                        nonFocusable
                    )
                | 2 ->
                    // Third frame: nothing should now be focused, because the previous frame had no focusable elements.
                    RenderState.focusedKey previousTickRenderState |> shouldEqual None
                    Vdom.textContent false ""
                | _ -> failwith "unexpected"

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let processWorld =
                { new WorldProcessor<_, int ref> with
                    member _.ProcessWorld (inputs, _, state) =
                        for s in inputs do
                            match s with
                            | WorldStateChange.Keystroke _ -> state.Value <- state.Value + 1
                            | WorldStateChange.MouseEvent _ -> failwith "no mouse events"
                            | WorldStateChange.ApplicationEvent () -> failwith "no app events"
                            | WorldStateChange.KeyboardEvent _ -> failwith "no keyboard events"
                            | WorldStateChange.ApplicationEventException _ -> failwith "no exceptions possible"
                }

            let renderFocusable = ref 0
            let renderState = RenderState.make' console

            App.pumpOnce worldFreezer renderFocusable haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
        ☐       |
"

                return ConsoleHarness.toString terminal
            }

            // Tab to focus the checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            App.pumpOnce worldFreezer renderFocusable haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
       [☐]      |
"

                return ConsoleHarness.toString terminal
            }

            // Now reassign the key to a non-focusable element
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            App.pumpOnce worldFreezer renderFocusable haveFrameworkHandleFocus renderState processWorld vdom

            // The element is no longer in the focusable list.
            // Vdom construction sees that on the previous tick, that element was focused, so it displays as focused.
            expect {
                snapshot
                    @"
more       [☐]  |
"

                return ConsoleHarness.toString terminal
            }

            // Give us a rerender and observe that on the previous tick, nothing was focused according to the framework
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))
            App.pumpOnce worldFreezer renderFocusable haveFrameworkHandleFocus renderState processWorld vdom

            expect {
                snapshot
                    @"
                |
"

                return ConsoleHarness.toString terminal
            }
        }
