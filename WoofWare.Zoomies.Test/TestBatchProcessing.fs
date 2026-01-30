namespace WoofWare.Zoomies.Test

open System
open System.Collections.Immutable
open System.Threading.Tasks
open FsCheck
open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestBatchProcessing =

    /// Event type for tracking processed keystrokes
    type KeystrokeEvent = | KeystrokeEvent of char

    /// Test helper that processes keystrokes and collects the characters.
    /// The new API processes all events in one go, so batch sizes are no longer configurable.
    let processKeystrokes (haveFrameworkHandleFocus : bool) (keystrokes : ConsoleKeyInfo list) : char list Task =
        task {
            let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 24)
            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            // Send all keystrokes
            for key in keystrokes do
                world.SendKey key

            // State is a list of all processed characters in order
            let initialState = ImmutableArray<char>.Empty

            let transition (state : ImmutableArray<char>) (KeystrokeEvent c) = state.Add c

            let handleInput (change : WorldStateChange<KeystrokeEvent>) : KeystrokeEvent option =
                match change with
                | WorldStateChange.Keystroke c -> Some (KeystrokeEvent c.KeyChar)
                | _ -> None

            let vdom (_vdomContext : IVdomContext<_>) (_state : ImmutableArray<char>) = Vdom.textContent ""

            let focusHandling =
                if haveFrameworkHandleFocus then
                    FocusHandling.FrameworkManaged
                else
                    FocusHandling.UserManaged

            let config : AppConfig<ImmutableArray<char>, KeystrokeEvent, unit> =
                {
                    Initial = initialState
                    Transition = transition
                    View = App.pureView vdom
                    HandleInput = handleInput
                    HandlePostLayout = fun _ s -> s
                    FocusHandling = focusHandling
                    ActivationResolver = ActivationResolver.none
                    OnSetup = fun _ -> ()
                }

            use ctx = IncrTestContext.make console config None

            // Pump once to process all events
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // Return the final processed characters
            return IncrTestContext.currentState ctx |> Seq.toList
        }

    [<Test>]
    let ``batch processing handles all events in order`` () =
        // This test runs with frameworkHandleFocus=false (manual mode).
        // Processing tabs with frameworkHandleFocus=true requires focusable elements in the vdom;
        // see ``batch processing with framework focus intercepts tabs`` for that case.
        let property (keyChars : char list) =
            task {
                if List.isEmpty keyChars then
                    return ()
                else
                    let keystrokes =
                        keyChars
                        |> List.map (fun c ->
                            if c = '\t' then
                                ConsoleKeyInfo (c, ConsoleKey.Tab, false, false, false)
                            else
                                ConsoleKeyInfo (c, ConsoleKey.NoName, false, false, false)
                        )

                    let! result = processKeystrokes false keystrokes

                    // In manual mode, all characters including tabs pass through
                    return result |> shouldEqual keyChars
            }

        Check.One (propConfig, property)

    [<Test>]
    let ``single event processing eventually processes everything`` () =
        // The new API processes all events at once, so this test just verifies
        // that all events are processed correctly.
        // Note: We filter out tabs because processKeystrokes uses a vdom without
        // focusable elements. Tab interception with focus is tested separately.
        let property (keyChars : char list) =
            task {
                let keyChars = keyChars |> List.filter (fun c -> c <> '\t')

                if List.isEmpty keyChars then
                    return ()
                else
                    let keystrokes =
                        keyChars
                        |> List.map (fun c -> ConsoleKeyInfo (c, ConsoleKey.NoName, false, false, false))

                    let! result = processKeystrokes false keystrokes

                    return result |> shouldEqual keyChars
            }

        Check.One (propConfig, property)

    [<Test>]
    let ``large batch processing eventually processes everything`` () =
        // The new API processes all events at once.
        // Note: We filter out tabs because processKeystrokes uses a vdom without
        // focusable elements. Tab interception with focus is tested separately.
        let property (keyChars : char list) =
            task {
                let keyChars = keyChars |> List.filter (fun c -> c <> '\t')

                if List.isEmpty keyChars then
                    return ()
                else
                    let keystrokes =
                        keyChars
                        |> List.map (fun c -> ConsoleKeyInfo (c, ConsoleKey.NoName, false, false, false))

                    let! result = processKeystrokes false keystrokes

                    return result |> shouldEqual keyChars
            }

        Check.One (propConfig, property)

    /// Test helper that processes events with framework focus handling enabled,
    /// using a vdom with focusable elements so tabs are properly intercepted.
    let processKeystrokesWithFocus (keystrokes : ConsoleKeyInfo list) : char list Task =
        task {
            let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 24)
            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            // Send all keystrokes
            for key in keystrokes do
                world.SendKey key

            // State is a list of all processed characters in order
            let initialState = ImmutableArray<char>.Empty

            let transition (state : ImmutableArray<char>) (KeystrokeEvent c) = state.Add c

            let handleInput (change : WorldStateChange<KeystrokeEvent>) : KeystrokeEvent option =
                match change with
                | WorldStateChange.Keystroke c -> Some (KeystrokeEvent c.KeyChar)
                | _ -> None

            // Use a vdom with focusable elements so the framework can intercept tabs
            let vdom (vdomContext : IVdomContext<_>) (_state : ImmutableArray<char>) =
                let checkbox0 =
                    Components.Checkbox.make (vdomContext, NodeKey.make "checkbox0", false)

                let checkbox1 =
                    Components.Checkbox.make (vdomContext, NodeKey.make "checkbox1", false)

                Vdom.panelSplitAbsolute (SplitDirection.Vertical, -3, checkbox0, checkbox1)

            let config : AppConfig<ImmutableArray<char>, KeystrokeEvent, unit> =
                {
                    Initial = initialState
                    Transition = transition
                    View = App.pureView vdom
                    HandleInput = handleInput
                    HandlePostLayout = fun _ s -> s
                    FocusHandling = FocusHandling.FrameworkManaged
                    ActivationResolver = ActivationResolver.none
                    OnSetup = fun _ -> ()
                }

            use ctx = IncrTestContext.make console config None

            // Pump once to process all events
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // Return the final processed characters
            return IncrTestContext.currentState ctx |> Seq.toList
        }

    [<Test>]
    let ``batch processing with framework focus intercepts tabs`` () =
        // This test exercises the focus-aware path by including tabs and
        // verifying they are intercepted by the framework (not passed through to the processor).
        // We inject tabs into the input to guarantee coverage of the tab-interception path.
        // Note: The new API processes all events at once, so batch size parameters are no longer used.
        let property (nonTabChar : char) (nonTabChars : char list) (tabPositions : int list) =
            task {
                // Construct a non-empty list of non-tab characters
                let baseChars =
                    (nonTabChar :: nonTabChars) |> List.map (fun c -> if c = '\t' then 'X' else c)

                // Inject tabs at various positions to guarantee tab coverage
                let mutable keyChars = baseChars

                for pos in tabPositions do
                    let insertPos = abs pos % (keyChars.Length + 1)

                    keyChars <- List.take insertPos keyChars @ [ '\t' ] @ List.skip insertPos keyChars

                let keystrokes =
                    keyChars
                    |> List.map (fun c ->
                        if c = '\t' then
                            ConsoleKeyInfo (c, ConsoleKey.Tab, false, false, false)
                        else
                            ConsoleKeyInfo (c, ConsoleKey.NoName, false, false, false)
                    )

                let! result = processKeystrokesWithFocus keystrokes

                // In framework focus mode, tabs are intercepted for focus cycling
                // and don't appear in the processed output
                let expected = keyChars |> List.filter (fun c -> c <> '\t')

                return result |> shouldEqual expected
            }

        Check.One (propConfig, property)

    /// Test that user-managed focus passes tabs through as regular keystrokes.
    [<Test>]
    let ``user managed focus passes tabs through`` () =
        task {
            let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 24)
            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let initialState = ImmutableArray<char>.Empty

            let transition (state : ImmutableArray<char>) (KeystrokeEvent c) = state.Add c

            let handleInput (change : WorldStateChange<KeystrokeEvent>) : KeystrokeEvent option =
                match change with
                | WorldStateChange.Keystroke c -> Some (KeystrokeEvent c.KeyChar)
                | _ -> None

            // Use a vdom with focusable elements
            let vdom (vdomContext : IVdomContext<_>) (_state : ImmutableArray<char>) =
                let checkbox0 =
                    Components.Checkbox.make (vdomContext, NodeKey.make "checkbox0", false)

                let checkbox1 =
                    Components.Checkbox.make (vdomContext, NodeKey.make "checkbox1", false)

                Vdom.panelSplitAbsolute (SplitDirection.Vertical, -3, checkbox0, checkbox1)

            let config : AppConfig<ImmutableArray<char>, KeystrokeEvent, unit> =
                {
                    Initial = initialState
                    Transition = transition
                    View = App.pureView vdom
                    HandleInput = handleInput
                    HandlePostLayout = fun _ s -> s
                    FocusHandling = FocusHandling.UserManaged
                    ActivationResolver = ActivationResolver.none
                    OnSetup = fun _ -> ()
                }

            use ctx = IncrTestContext.make console config None

            // Initial pump
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // Send some keys including tabs
            world.SendKey (ConsoleKeyInfo ('a', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            world.SendKey (ConsoleKeyInfo ('b', ConsoleKey.NoName, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // In user managed mode, tabs pass through
            let result = IncrTestContext.currentState ctx |> Seq.toList
            result |> shouldEqual [ 'a' ; '\t' ; 'b' ]
        }

    /// Test that framework-managed focus intercepts tabs for focus cycling.
    [<Test>]
    let ``framework managed focus intercepts tabs`` () =
        task {
            let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 24)
            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let initialState = ImmutableArray<char>.Empty

            let transition (state : ImmutableArray<char>) (KeystrokeEvent c) = state.Add c

            let handleInput (change : WorldStateChange<KeystrokeEvent>) : KeystrokeEvent option =
                match change with
                | WorldStateChange.Keystroke c -> Some (KeystrokeEvent c.KeyChar)
                | _ -> None

            // Use a vdom with focusable elements
            let vdom (vdomContext : IVdomContext<_>) (_state : ImmutableArray<char>) =
                let checkbox0 =
                    Components.Checkbox.make (vdomContext, NodeKey.make "checkbox0", false)

                let checkbox1 =
                    Components.Checkbox.make (vdomContext, NodeKey.make "checkbox1", false)

                Vdom.panelSplitAbsolute (SplitDirection.Vertical, -3, checkbox0, checkbox1)

            let config : AppConfig<ImmutableArray<char>, KeystrokeEvent, unit> =
                {
                    Initial = initialState
                    Transition = transition
                    View = App.pureView vdom
                    HandleInput = handleInput
                    HandlePostLayout = fun _ s -> s
                    FocusHandling = FocusHandling.FrameworkManaged
                    ActivationResolver = ActivationResolver.none
                    OnSetup = fun _ -> ()
                }

            use ctx = IncrTestContext.make console config None

            // Initial pump
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // Send some keys including tabs
            world.SendKey (ConsoleKeyInfo ('a', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            world.SendKey (ConsoleKeyInfo ('b', ConsoleKey.NoName, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // In framework managed mode, tabs are intercepted for focus cycling
            let result = IncrTestContext.currentState ctx |> Seq.toList
            result |> shouldEqual [ 'a' ; 'b' ]

            // Tab should have moved focus to checkbox0
            RenderState.focusedKey ctx.RenderState
            |> shouldEqual (Some (NodeKey.make "checkbox0"))
        }

    /// Test that all events in a batch are processed in order.
    [<Test>]
    let ``all events processed in order`` () =
        task {
            let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 24)
            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let initialState : char list = []

            let transition (state : char list) (c : char) = state @ [ c ]

            let handleInput (change : WorldStateChange<char>) : char option =
                match change with
                | WorldStateChange.Keystroke c -> Some c.KeyChar
                | _ -> None

            let vdom (_vdomContext : IVdomContext<_>) (_state : char list) = Vdom.textContent ""

            let config : AppConfig<char list, char, unit> =
                {
                    Initial = initialState
                    Transition = transition
                    View = App.pureView vdom
                    HandleInput = handleInput
                    HandlePostLayout = fun _ s -> s
                    FocusHandling = FocusHandling.UserManaged
                    ActivationResolver = ActivationResolver.none
                    OnSetup = fun _ -> ()
                }

            use ctx = IncrTestContext.make console config None

            // Initial pump
            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // Send a batch of keystrokes
            let totalKeystrokes = 10

            for i in 0 .. totalKeystrokes - 1 do
                world.SendKey (ConsoleKeyInfo (char (int 'a' + i), ConsoleKey.NoName, false, false, false))

            IncrTestContext.pumpOnce worldFreezer config ctx |> ignore

            // Verify all events were processed in order
            let expectedChars = [ 'a' .. char (int 'a' + totalKeystrokes - 1) ]
            let result = IncrTestContext.currentState ctx
            result |> shouldEqual expectedChars
        }
