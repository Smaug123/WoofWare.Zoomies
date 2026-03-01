namespace WoofWare.Zoomies.Test

open System
open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Incremental
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

    type CheckboxEvent = | Toggle of int

    let vdom (vdomContext : IVdomContext<_>) (state : State) : Vdom<DesiredBounds> Node =
        let incr = vdomContext.Incr

        let checkboxNodes =
            List.init
                4
                (fun i ->
                    let key = NodeKey.make $"checkbox%i{i}"
                    Components.Checkbox.make (vdomContext, key, state.Checkboxes.[i])
                )

        // Combine all checkbox nodes incrementally
        checkboxNodes
        |> List.reduce (fun nodeX nodeY ->
            incr.Map2
                (fun (x : Vdom<DesiredBounds>) (y : Vdom<DesiredBounds>) ->
                    Vdom.panelSplitAbsolute (SplitDirection.Vertical, -3, x, y)
                )
                nodeX
                nodeY
        )

    let transition (state : State) (event : CheckboxEvent) : State =
        match event with
        | Toggle i ->
            { state with
                Checkboxes = state.Checkboxes.SetItem (i, not state.Checkboxes.[i])
            }

    let activationResolver : ActivationResolver<CheckboxEvent, State> =
        ActivationResolver (fun key keyInfo _state ->
            if keyInfo.KeyChar = ' ' then
                let keyStr = NodeKey.toHumanReadableString key
                let prefix = "checkbox"

                if keyStr.StartsWith (prefix, StringComparison.Ordinal) then
                    let index = keyStr.Substring prefix.Length |> Int32.Parse
                    Some (Toggle index)
                else
                    None
            else
                None
        )

    let makeConfig initial =
        AppConfig.simple initial transition (App.pureViewIncr vdom)
        |> AppConfig.withActivationResolver activationResolver

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

            let initial =
                {
                    Checkboxes = ImmutableArray.Create<bool> [| false ; false ; false ; false |]
                }

            let config = makeConfig initial

            use ctx = IncrTestContext.make console config None

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
   ☐    ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            // Nothing focused, so space does nothing
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
   ☐    ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            // Move focus to the first focusable element
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
  [☐]   ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
  [☑]   ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
   ☑   [☐] ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
   ☑   [☑] ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
   ☑    ☑ [☐] ☐ |
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
   ☑    ☑ [☑] ☐ |
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
   ☑    ☑  ☑ [☐]|
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
   ☑    ☑  ☑ [☑]|
"

                return ConsoleHarness.toString terminal
            }

            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

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

            let initial =
                {
                    Checkboxes = ImmutableArray.Create<bool> [| false ; false ; false ; false |]
                }

            let config = makeConfig initial

            use ctx = IncrTestContext.make console config None

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
   ☐    ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            // Tab to focus first checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
  [☐]   ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            // Tab to focus second checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
   ☐   [☐] ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            // Shift+Tab to go back to first checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, true, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
  [☐]   ☐  ☐  ☐ |
"

                return ConsoleHarness.toString terminal
            }

            // Shift+Tab from first should wrap to last
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, true, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
   ☐    ☐  ☐ [☐]|
"

                return ConsoleHarness.toString terminal
            }

            // Check the last checkbox
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
   ☐    ☐  ☐ [☑]|
"

                return ConsoleHarness.toString terminal
            }

            // Shift+Tab to third checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, true, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
   ☐    ☐ [☐] ☑ |
"

                return ConsoleHarness.toString terminal
            }
        }

    type BoolEvent = | ToggleBool

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
            let vdom (vdomContext : IVdomContext<_>) (renderCheckbox1 : bool) : Vdom<DesiredBounds> Node =
                let incr = vdomContext.Incr
                let sharedKey = NodeKey.make "shared-key"
                let unsharedKey = NodeKey.make "unshared-key"

                if renderCheckbox1 then
                    // First frame: checkbox at shared-key
                    let checkbox1Node =
                        Components.Checkbox.make (vdomContext, sharedKey, isChecked = false)

                    let checkbox2Node =
                        Components.Checkbox.make (vdomContext, unsharedKey, isChecked = false)

                    incr.Map2
                        (fun (checkbox1 : Vdom<DesiredBounds>) (checkbox2 : Vdom<DesiredBounds>) ->
                            Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, checkbox1, checkbox2)
                        )
                        checkbox1Node
                        checkbox2Node
                else
                    // Second frame: different checkbox at shared-key
                    let checkbox1Node =
                        Components.Checkbox.make (vdomContext, unsharedKey, isChecked = false)

                    let checkbox2Node =
                        Components.Checkbox.make (vdomContext, sharedKey, isChecked = false)

                    incr.Map2
                        (fun (checkbox1 : Vdom<DesiredBounds>) (checkbox2 : Vdom<DesiredBounds>) ->
                            Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, checkbox1, checkbox2)
                        )
                        checkbox1Node
                        checkbox2Node

            let transition (renderCheckbox1 : bool) (event : BoolEvent) : bool =
                match event with
                | ToggleBool -> not renderCheckbox1

            let handleInput (change : WorldStateChange<BoolEvent>) : BoolEvent option =
                match change with
                | WorldStateChange.Keystroke _ -> Some ToggleBool
                | _ -> None

            let config : AppConfig<bool, BoolEvent, unit> =
                AppConfig.withInputHandler
                    true
                    transition
                    (App.pureViewIncr vdom)
                    handleInput
                    (fun _ s -> s)
                    ActivationResolver.none

            use ctx = IncrTestContext.make console config None

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
   ☐       ☐    |
"

                return ConsoleHarness.toString terminal
            }

            // Tab to focus the checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
  [☐]      ☐    |
"

                return ConsoleHarness.toString terminal
            }

            // Now reassign the key to a different element. Trigger a rerender:
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // Focus should remain on the element with shared-key, even though it's a different element
            expect {
                snapshot
                    @"
   ☐      [☐]   |
"

                return ConsoleHarness.toString terminal
            }
        }

    type IntEvent = | Increment

    [<Test>]
    let ``key reassignment to non-focusable element loses focus`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 16) (fun () -> 1)

            let world = MockWorld.make ()

            let vdom (vdomContext : IVdomContext<_>) (tick : int) : Vdom<DesiredBounds> Node =
                let sharedKey = NodeKey.make "shared-key"

                match tick with
                | 0 ->
                    // First frame: focusable checkbox at shared-key
                    Components.Checkbox.make (vdomContext, sharedKey, false)
                | 1 ->
                    // Second frame: non-focusable element.
                    // The previous render had focus on the key `sharedKey`.
                    vdomContext.FocusedKey
                    |> vdomContext.Incr.Map (fun currentFocus ->
                        let isFocused = currentFocus = Some sharedKey

                        let nonFocusable =
                            Components.Checkbox.make' (false, isFocused) |> Vdom.withKey sharedKey

                        Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, Vdom.textContent "more", nonFocusable)
                    )
                | 2 ->
                    // Third frame: nothing should now be focused, because the previous frame had no focusable elements.
                    vdomContext.FocusedKey
                    |> vdomContext.Incr.Map (fun currentFocus ->
                        currentFocus |> shouldEqual None
                        Vdom.textContent ""
                    )
                | _ -> failwith "unexpected"

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let transition (tick : int) (event : IntEvent) : int =
                match event with
                | Increment -> tick + 1

            let handleInput (change : WorldStateChange<IntEvent>) : IntEvent option =
                match change with
                | WorldStateChange.Keystroke _ -> Some Increment
                | _ -> None

            let config : AppConfig<int, IntEvent, unit> =
                AppConfig.withInputHandler
                    0
                    transition
                    (App.pureViewIncr vdom)
                    handleInput
                    (fun _ s -> s)
                    ActivationResolver.none

            use ctx = IncrTestContext.make console config None

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
       ☐        |
"

                return ConsoleHarness.toString terminal
            }

            // Tab to focus the checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
      [☐]       |
"

                return ConsoleHarness.toString terminal
            }

            // Now reassign the key to a non-focusable element
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // The element is no longer in the focusable list.
            // Focus is cleared and the change is properly propagated via re-stabilization.
            expect {
                snapshot
                    @"
more       ☐    |
"

                return ConsoleHarness.toString terminal
            }

            // Give us a rerender and observe that on the previous tick, nothing was focused according to the framework
            world.SendKey (ConsoleKeyInfo (' ', ConsoleKey.Spacebar, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
                |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``focusable text content can gain focus`` () =
        task {
            let console, terminal = ConsoleHarness.make ()

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let textKey = NodeKey.make "focusable-text"
            let checkboxKey = NodeKey.make "checkbox"

            let vdom (vdomContext : IVdomContext<_>) (_ : FakeUnit) : Vdom<DesiredBounds> Node =
                let checkboxNode = Components.Checkbox.make (vdomContext, checkboxKey, false)

                vdomContext.Incr.Map2
                    (fun currentFocus (checkbox : Vdom<DesiredBounds>) ->
                        let text =
                            Vdom.textContent ("This is focusable text", isFocused = (currentFocus = Some textKey))
                            |> Vdom.withKey textKey
                            |> Vdom.withFocusTracking

                        Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 3, text, checkbox)
                    )
                    vdomContext.FocusedKey
                    checkboxNode

            let transition (state : FakeUnit) (_ : unit) : FakeUnit = state

            let handleInput (change : WorldStateChange<unit>) : unit option =
                match change with
                | WorldStateChange.Keystroke _ -> None
                | _ -> None

            let config : AppConfig<FakeUnit, unit, unit> =
                AppConfig.withInputHandler
                    (FakeUnit.fake ())
                    transition
                    (App.pureViewIncr vdom)
                    handleInput
                    (fun _ s -> s)
                    ActivationResolver.none

            use ctx = IncrTestContext.make console config None

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
This is focusable text                                                          |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                       ☐                                        |
                                                                                |
                                                                                |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Tab to focus the text
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // Assert that focus moved to the text element
            let renderState = ctx.RenderState
            RenderState.focusedKey renderState |> shouldEqual (Some textKey)

            expect {
                snapshot
                    @"
This is focusable text                                                          |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                       ☐                                        |
                                                                                |
                                                                                |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Tab to focus the checkbox
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // Assert that focus moved to the checkbox
            RenderState.focusedKey renderState |> shouldEqual (Some checkboxKey)

            expect {
                snapshot
                    @"
This is focusable text                                                          |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                      [☐]                                       |
                                                                                |
                                                                                |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Tab back to text
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // Assert that focus moved back to the text element
            RenderState.focusedKey renderState |> shouldEqual (Some textKey)

            expect {
                snapshot
                    @"
This is focusable text                                                          |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                       ☐                                        |
                                                                                |
                                                                                |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``initial focus is respected`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (vdomContext : IVdomContext<_>) (_ : FakeUnit) : Vdom<DesiredBounds> Node =
                let incr = vdomContext.Incr
                let checkbox1Key = NodeKey.make "checkbox1"
                let checkbox2Key = NodeKey.make "checkbox2"
                let checkbox3Key = NodeKey.make "checkbox3"

                let checkbox1Node = Components.Checkbox.make (vdomContext, checkbox1Key, false)

                let checkbox2Node =
                    Components.Checkbox.make (vdomContext, checkbox2Key, false, isFirstToFocus = true)

                let checkbox3Node = Components.Checkbox.make (vdomContext, checkbox3Key, false)

                // Combine checkbox2 and checkbox3 first, then combine with checkbox1
                let checkbox23Node =
                    incr.Map2
                        (fun (checkbox2 : Vdom<DesiredBounds>) (checkbox3 : Vdom<DesiredBounds>) ->
                            Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, checkbox2, checkbox3)
                        )
                        checkbox2Node
                        checkbox3Node

                incr.Map2
                    (fun (checkbox1 : Vdom<DesiredBounds>) (checkbox23 : Vdom<DesiredBounds>) ->
                        Vdom.panelSplitProportion (SplitDirection.Vertical, 0.33, checkbox1, checkbox23)
                    )
                    checkbox1Node
                    checkbox23Node

            let transition (state : FakeUnit) (_ : unit) : FakeUnit = state

            let handleInput (change : WorldStateChange<unit>) : unit option =
                match change with
                | WorldStateChange.Keystroke _ -> None
                | _ -> None

            let config : AppConfig<FakeUnit, unit, unit> =
                AppConfig.withInputHandler
                    (FakeUnit.fake ())
                    transition
                    (App.pureViewIncr vdom)
                    handleInput
                    (fun _ s -> s)
                    ActivationResolver.none

            use ctx = IncrTestContext.make console config None

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
                                                                                |
            ☐                          ☐                          ☐             |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Tab should focus checkbox2 (marked with isFirstToFocus=true), not checkbox1
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
                                                                                |
            ☐                         [☐]                         ☐             |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Tab again should cycle to checkbox3
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
                                                                                |
            ☐                          ☐                         [☐]            |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Tab again should cycle to checkbox1
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
                                                                                |
           [☐]                         ☐                          ☐             |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Tab again should cycle back to checkbox2
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
                                                                                |
            ☐                         [☐]                         ☐             |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }
        }

    [<Test>]
    let ``initially focused element starts with focus`` () =
        task {
            let console, terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 3)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let vdom (vdomContext : IVdomContext<_>) (_ : FakeUnit) : Vdom<DesiredBounds> Node =
                let incr = vdomContext.Incr
                let checkbox1Key = NodeKey.make "checkbox1"
                let checkbox2Key = NodeKey.make "checkbox2"
                let checkbox3Key = NodeKey.make "checkbox3"

                let checkbox1Node = Components.Checkbox.make (vdomContext, checkbox1Key, false)

                let checkbox2Node =
                    Components.Checkbox.make (vdomContext, checkbox2Key, false, isInitiallyFocused = true)

                let checkbox3Node = Components.Checkbox.make (vdomContext, checkbox3Key, false)

                // Combine checkbox2 and checkbox3 first, then combine with checkbox1
                let checkbox23Node =
                    incr.Map2
                        (fun (checkbox2 : Vdom<DesiredBounds>) (checkbox3 : Vdom<DesiredBounds>) ->
                            Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, checkbox2, checkbox3)
                        )
                        checkbox2Node
                        checkbox3Node

                incr.Map2
                    (fun (checkbox1 : Vdom<DesiredBounds>) (checkbox23 : Vdom<DesiredBounds>) ->
                        Vdom.panelSplitProportion (SplitDirection.Vertical, 0.33, checkbox1, checkbox23)
                    )
                    checkbox1Node
                    checkbox23Node

            let transition (state : FakeUnit) (_ : unit) : FakeUnit = state

            let handleInput (change : WorldStateChange<unit>) : unit option =
                match change with
                | WorldStateChange.Keystroke _ -> None
                | _ -> None

            let config : AppConfig<FakeUnit, unit, unit> =
                AppConfig.withInputHandler
                    (FakeUnit.fake ())
                    transition
                    (App.pureViewIncr vdom)
                    handleInput
                    (fun _ s -> s)
                    ActivationResolver.none

            use ctx = IncrTestContext.make console config None

            // First render: checkbox2 should start with focus (marked with isInitiallyFocused=true)
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
                                                                                |
            ☐                         [☐]                         ☐             |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Tab should cycle to checkbox3
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
                                                                                |
            ☐                          ☐                         [☐]            |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Tab again should cycle to checkbox1
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
                                                                                |
           [☐]                         ☐                          ☐             |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Shift+Tab should cycle backwards to checkbox3
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, true, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
                                                                                |
            ☐                          ☐                         [☐]            |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }

            // Shift+Tab again should cycle backwards to checkbox2
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, true, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            expect {
                snapshot
                    @"
                                                                                |
            ☐                         [☐]                         ☐             |
                                                                                |
"

                return ConsoleHarness.toString terminal
            }
        }
