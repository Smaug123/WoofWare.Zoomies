namespace WoofWare.Zoomies.Test

open System
open System.Collections.Immutable
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

    // ImmutableArray has sad equality semantics so we do this longhand.
    // I could have done this with lists instead, I guess.
    [<NoComparison>]
    [<CustomEquality>]
    type State =
        {
            Checkboxes : bool ImmutableArray
        }

        override this.GetHashCode () = this.Checkboxes.GetHashCode ()

        override this.Equals (other : obj) =
            match other with
            | :? State as other ->
                if this.Checkboxes.Length <> other.Checkboxes.Length then
                    false
                else
                    Seq.zip this.Checkboxes other.Checkboxes |> Seq.forall (fun (x, y) -> x = y)
            | _ -> failwith "bad"

    let vdom (vdomContext : VdomContext) (state : State) =
        List.init
            4
            (fun i ->
                let key = NodeKey.make $"checkbox%i{i}"
                Components.Checkbox.make (vdomContext, key, state.Checkboxes.[i])
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

            let state =
                {
                    Checkboxes = ImmutableArray.Create<bool> [| false ; false ; false ; false |]
                }

            let haveFrameworkHandleFocus _ = true

            let processWorld =
                { new WorldProcessor<_, State> with
                    member _.ProcessWorld (inputs, renderState, state) =
                        let mutable newCheckboxes = state.Checkboxes

                        for s in inputs do
                            match s with
                            | WorldStateChange.Keystroke c ->
                                if c.KeyChar = ' ' then
                                    match VdomContext.focusedKey renderState with
                                    | None ->
                                        // pressed space while nothing focused
                                        ()
                                    | Some focused ->
                                        let key = NodeKey.toString focused
                                        let prefix = "checkbox"

                                        if key.StartsWith (prefix, StringComparison.Ordinal) then
                                            let key = key.Substring prefix.Length |> Int32.Parse
                                            newCheckboxes <- newCheckboxes.SetItem (key, not newCheckboxes.[key])
                                        else
                                            failwith "unexpected key"
                                else
                                    failwith "unexpected key char"
                            | WorldStateChange.MouseEvent _ -> failwith "no mouse events"
                            | WorldStateChange.ApplicationEvent () -> failwith "no app events"
                            | WorldStateChange.KeyboardEvent _ -> failwith "no keyboard events"
                            | WorldStateChange.ApplicationEventException _ -> failwith "no exceptions possible"

                        ProcessWorldResult.make
                            {
                                Checkboxes = newCheckboxes
                            }
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None
            let mutable currentState = state

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
   ☐    ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            // Nothing focused, so space does nothing
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
   ☐    ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            // Move focus to the first focusable element
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
  [☐]   ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

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
  [☑]   ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

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
   ☑   [☐] ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

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
   ☑   [☑] ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

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
   ☑    ☑ [☐] ☐ |
"

                return ConsoleHarness.toString terminal
            }

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
   ☑    ☑ [☑] ☐ |
"

                return ConsoleHarness.toString terminal
            }

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
   ☑    ☑  ☑ [☐]|
"

                return ConsoleHarness.toString terminal
            }

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
   ☑    ☑  ☑ [☑]|
"

                return ConsoleHarness.toString terminal
            }

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

            let state =
                {
                    Checkboxes = ImmutableArray.Create<bool> [| false ; false ; false ; false |]
                }

            let haveFrameworkHandleFocus _ = true

            let processWorld =
                { new WorldProcessor<_, State> with
                    member _.ProcessWorld (inputs, renderState, checkboxes) =
                        let mutable newCheckboxes = checkboxes.Checkboxes

                        for s in inputs do
                            match s with
                            | WorldStateChange.Keystroke c ->
                                if c.KeyChar = ' ' then
                                    match VdomContext.focusedKey renderState with
                                    | None -> ()
                                    | Some focused ->
                                        let key = NodeKey.toString focused
                                        let prefix = "checkbox"

                                        if key.StartsWith (prefix, StringComparison.Ordinal) then
                                            let key = key.Substring prefix.Length |> Int32.Parse
                                            newCheckboxes <- newCheckboxes.SetItem (key, not newCheckboxes.[key])
                                        else
                                            failwith "unexpected key"
                                else
                                    failwith "unexpected key char"
                            | WorldStateChange.MouseEvent _ -> failwith "no mouse events"
                            | WorldStateChange.ApplicationEvent () -> failwith "no app events"
                            | WorldStateChange.KeyboardEvent _ -> failwith "no keyboard events"
                            | WorldStateChange.ApplicationEventException _ -> failwith "no exceptions possible"

                        ProcessWorldResult.make
                            {
                                Checkboxes = newCheckboxes
                            }
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None
            let mutable currentState = state

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
   ☐    ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            // Tab to focus first checkbox
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
  [☐]   ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            // Tab to focus second checkbox
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
   ☐   [☐] ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            // Shift+Tab to go back to first checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, true, false, false))

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
  [☐]   ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            // Shift+Tab from first should wrap to last
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, true, false, false))

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
   ☐    ☐  ☐ [☐]|
"

                return ConsoleHarness.toString terminal
            }

            // Check the last checkbox
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
   ☐    ☐  ☐ [☑]|
"

                return ConsoleHarness.toString terminal
            }

            // Shift+Tab to third checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, true, false, false))

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

            let vdom (vdomContext : VdomContext) (renderCheckbox1 : bool) =
                let sharedKey = NodeKey.make "shared-key"
                let unsharedKey = NodeKey.make "unshared-key"

                if renderCheckbox1 then
                    // First frame: checkbox at shared-key
                    let checkbox1 = Components.Checkbox.make (vdomContext, sharedKey, isChecked = false)

                    let checkbox2 =
                        Components.Checkbox.make (vdomContext, unsharedKey, isChecked = false)

                    Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, checkbox1, checkbox2)
                else
                    // Second frame: different checkbox at shared-key
                    let checkbox1 =
                        Components.Checkbox.make (vdomContext, unsharedKey, isChecked = false)

                    let checkbox2 = Components.Checkbox.make (vdomContext, sharedKey, isChecked = false)

                    Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, checkbox1, checkbox2)

            let processWorld =
                { new WorldProcessor<_, bool> with
                    member _.ProcessWorld (inputs, _, renderCheckbox1) =
                        let mutable renderCheckbox1 = renderCheckbox1

                        for s in inputs do
                            match s with
                            | WorldStateChange.Keystroke _ -> renderCheckbox1 <- not renderCheckbox1
                            | WorldStateChange.MouseEvent _ -> failwith "no mouse events"
                            | WorldStateChange.ApplicationEvent () -> failwith "no app events"
                            | WorldStateChange.KeyboardEvent _ -> failwith "no keyboard events"
                            | WorldStateChange.ApplicationEventException _ -> failwith "no exceptions possible"

                        ProcessWorldResult.make renderCheckbox1
                }

            let renderState = RenderState.make console MockTime.getStaticUtcNow None
            let mutable renderCheckbox1 = true

            renderCheckbox1 <-
                App.pumpOnce
                    worldFreezer
                    renderCheckbox1
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none

            expect {
                snapshot
                    @"
   ☐       ☐    |
"

                return ConsoleHarness.toString terminal
            }

            // Tab to focus the checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            renderCheckbox1 <-
                App.pumpOnce
                    worldFreezer
                    renderCheckbox1
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none

            expect {
                snapshot
                    @"
  [☐]      ☐    |
"

                return ConsoleHarness.toString terminal
            }

            // Now reassign the key to a different element. Trigger a rerender:
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            renderCheckbox1 <-
                App.pumpOnce
                    worldFreezer
                    renderCheckbox1
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none

            // Focus should remain on the element with shared-key, even though it's a different element
            expect {
                snapshot
                    @"
   ☐      [☐]   |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``key reassignment to non-focusable element loses focus`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 16) (fun () -> 1)

            let world = MockWorld.make ()

            let vdom (vdomContext : VdomContext) (tick : int) =
                let currentFocus = VdomContext.focusedKey vdomContext
                let sharedKey = NodeKey.make "shared-key"

                match tick with
                | 0 ->
                    // First frame: focusable checkbox at shared-key
                    Components.Checkbox.make (vdomContext, sharedKey, false)
                | 1 ->
                    // Second frame: non-focusable element.
                    // The previous render had focus on the key `sharedKey`.
                    // Even though it's not focusable, it should show focus visuals if the context says it's focused.
                    let isFocused = currentFocus = Some sharedKey
                    let content = if isFocused then "[☐]" else " ☐ "

                    let nonFocusable =
                        Vdom.styledText (content, CellStyle.none, ContentAlignment.Centered)
                        |> Vdom.withKey sharedKey

                    Vdom.panelSplitProportion (
                        SplitDirection.Vertical,
                        0.5,
                        Vdom.textContent false "more",
                        nonFocusable
                    )
                | 2 ->
                    // Third frame: nothing should now be focused, because the previous frame had no focusable elements.
                    currentFocus |> shouldEqual None
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
                { new WorldProcessor<_, int> with
                    member _.ProcessWorld (inputs, _, state) =
                        let mutable newState = state

                        for s in inputs do
                            match s with
                            | WorldStateChange.Keystroke _ -> newState <- newState + 1
                            | WorldStateChange.MouseEvent _ -> failwith "no mouse events"
                            | WorldStateChange.ApplicationEvent () -> failwith "no app events"
                            | WorldStateChange.KeyboardEvent _ -> failwith "no keyboard events"
                            | WorldStateChange.ApplicationEventException _ -> failwith "no exceptions possible"

                        ProcessWorldResult.make newState
                }

            let mutable renderFocusable = 0
            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            renderFocusable <-
                App.pumpOnce
                    worldFreezer
                    renderFocusable
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none

            expect {
                snapshot
                    @"
       ☐        |
"

                return ConsoleHarness.toString terminal
            }

            // Tab to focus the checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            renderFocusable <-
                App.pumpOnce
                    worldFreezer
                    renderFocusable
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none

            expect {
                snapshot
                    @"
      [☐]       |
"

                return ConsoleHarness.toString terminal
            }

            // Now reassign the key to a non-focusable element
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            renderFocusable <-
                App.pumpOnce
                    worldFreezer
                    renderFocusable
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none

            // The element is no longer in the focusable list.
            // Vdom construction sees that on the previous tick, that element was focused, so it displays as focused.
            expect {
                snapshot
                    @"
more      [☐]   |
"

                return ConsoleHarness.toString terminal
            }

            // Give us a rerender and observe that on the previous tick, nothing was focused according to the framework
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            renderFocusable <-
                App.pumpOnce
                    worldFreezer
                    renderFocusable
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none

            expect {
                snapshot
                    @"
                |
"

                return ConsoleHarness.toString terminal
            }
        }
