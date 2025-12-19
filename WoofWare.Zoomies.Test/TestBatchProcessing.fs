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

    /// Test helper that simulates processing events with varying batch sizes
    let processWithBatchStrategy
        (haveFrameworkHandleFocus : bool)
        (keystrokes : ConsoleKeyInfo list)
        (batchSizes : int list)
        : char list Task
        =
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
            let initialState = ImmutableArray.Empty

            // Cycle through batch sizes
            let mutable batchSizeIndex = 0
            let mutable totalProcessed = 0

            let processWorld =
                { new WorldProcessor<unit, unit, ImmutableArray<char>> with
                    member _.ProcessWorld (inputs, _renderState, state) =
                        let batchSize =
                            if List.isEmpty batchSizes then
                                1
                            else
                                List.item (batchSizeIndex % batchSizes.Length) batchSizes

                        batchSizeIndex <- batchSizeIndex + 1

                        // Process up to batchSize events
                        let toProcess = min batchSize inputs.Length
                        let mutable newState = state

                        for i = 0 to toProcess - 1 do
                            match inputs.[i] with
                            | WorldStateChange.Keystroke c -> newState <- newState.Add c.KeyChar
                            | WorldStateChange.MouseEvent _ -> ()
                            | WorldStateChange.ApplicationEvent _ -> ()
                            | WorldStateChange.Paste _ -> ()
                            | WorldStateChange.ApplicationEventException _ -> ()

                        totalProcessed <- totalProcessed + toProcess

                        if toProcess >= inputs.Length then
                            // Processed everything in this batch
                            ProcessWorldResult.make newState
                        else
                            // Request a new batch after processing 'toProcess' items
                            ProcessWorldResult.make newState
                            |> ProcessWorldResult.withRerender (toProcess - 1)

                    member _.ProcessPostLayoutEvents (_events, _ctx, state) = state
                }

            let vdom (_vdomContext : IVdomContext<_>) (_state : ImmutableArray<char>) = Vdom.textContent ""

            let renderState = RenderState.make console MockTime.getStaticUtcNow None
            let mutable currentState = initialState

            // Keep pumping until all events are processed
            let mutable iterations = 0
            let maxIterations = keystrokes.Length * 10 // Safety limit

            while totalProcessed < keystrokes.Length && iterations < maxIterations do
                currentState <-
                    App.pumpOnce
                        worldFreezer
                        currentState
                        (fun _ -> haveFrameworkHandleFocus)
                        renderState
                        processWorld
                        vdom
                        ActivationResolver.none
                        (fun () -> false)

                iterations <- iterations + 1

            // Return the final processed characters
            return currentState |> Seq.toList
        }

    [<Test>]
    let ``batch processing handles all events in order`` () =
        // This test runs with frameworkHandleFocus=false (manual mode).
        // Processing tabs with frameworkHandleFocus=true requires focusable elements in the vdom;
        // see ``batch processing with framework focus intercepts tabs`` for that case.
        let property (batchSize1 : int) (batchSizes : int list) (keyChars : char list) =
            task {
                let batchSizes = (batchSize1 :: batchSizes) |> List.map (fun i -> abs i + 1)

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

                    let! result = processWithBatchStrategy false keystrokes batchSizes

                    // In manual mode, all characters including tabs pass through
                    return result |> shouldEqual keyChars
            }

        Check.One (propConfig, property)

    [<Test>]
    let ``single event processing eventually processes everything`` () =
        // Edge case: always process exactly one event per batch
        // Note: We filter out tabs because processWithBatchStrategy uses a vdom without
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

                    let! result = processWithBatchStrategy false keystrokes [ 1 ]

                    return result |> shouldEqual keyChars
            }

        Check.One (propConfig, property)

    [<Test>]
    let ``large batch processing eventually processes everything`` () =
        // Edge case: try to process all events in one go (but framework may split)
        // Note: We filter out tabs because processWithBatchStrategy uses a vdom without
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

                    let! result = processWithBatchStrategy false keystrokes [ 1000 ]

                    return result |> shouldEqual keyChars
            }

        Check.One (propConfig, property)

    /// Test helper that simulates processing events with framework focus handling enabled,
    /// using a vdom with focusable elements so tabs are properly intercepted.
    let processWithBatchStrategyAndFocus (keystrokes : ConsoleKeyInfo list) (batchSizes : int list) : char list Task =
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
            let initialState = ImmutableArray.Empty

            // Cycle through batch sizes
            let mutable batchSizeIndex = 0
            let mutable totalProcessed = 0
            // Track how many tabs we expect to be intercepted
            let tabCount =
                keystrokes |> List.filter (fun k -> k.Key = ConsoleKey.Tab) |> List.length

            let processWorld =
                { new WorldProcessor<unit, unit, ImmutableArray<char>> with
                    member _.ProcessWorld (inputs, _renderState, state) =
                        let batchSize =
                            if List.isEmpty batchSizes then
                                1
                            else
                                List.item (batchSizeIndex % batchSizes.Length) batchSizes

                        batchSizeIndex <- batchSizeIndex + 1

                        // Process up to batchSize events
                        let toProcess = min batchSize inputs.Length
                        let mutable newState = state

                        for i = 0 to toProcess - 1 do
                            match inputs.[i] with
                            | WorldStateChange.Keystroke c -> newState <- newState.Add c.KeyChar
                            | WorldStateChange.MouseEvent _ -> ()
                            | WorldStateChange.ApplicationEvent _ -> ()
                            | WorldStateChange.Paste _ -> ()
                            | WorldStateChange.ApplicationEventException _ -> ()

                        totalProcessed <- totalProcessed + toProcess

                        if toProcess >= inputs.Length then
                            // Processed everything in this batch
                            ProcessWorldResult.make newState
                        else
                            // Request a new batch after processing 'toProcess' items
                            ProcessWorldResult.make newState
                            |> ProcessWorldResult.withRerender (toProcess - 1)

                    member _.ProcessPostLayoutEvents (_events, _ctx, state) = state
                }

            // Use a vdom with focusable elements so the framework can intercept tabs
            let vdom (vdomContext : IVdomContext<_>) (_state : ImmutableArray<char>) =
                let checkbox0 =
                    Components.Checkbox.make (vdomContext, NodeKey.make "checkbox0", false)

                let checkbox1 =
                    Components.Checkbox.make (vdomContext, NodeKey.make "checkbox1", false)

                Vdom.panelSplitAbsolute (SplitDirection.Vertical, -3, checkbox0, checkbox1)

            let renderState = RenderState.make console MockTime.getStaticUtcNow None
            let mutable currentState = initialState

            // Keep pumping until all events are processed
            // (tabs are intercepted by framework, so we expect keystrokes.Length - tabCount to be processed)
            let expectedProcessed = keystrokes.Length - tabCount
            let mutable iterations = 0
            let maxIterations = keystrokes.Length * 10 + 10 // Safety limit

            while totalProcessed < expectedProcessed && iterations < maxIterations do
                currentState <-
                    App.pumpOnce
                        worldFreezer
                        currentState
                        (fun _ -> true) // Framework handles focus
                        renderState
                        processWorld
                        vdom
                        ActivationResolver.none
                        (fun () -> false)

                iterations <- iterations + 1

            // Return the final processed characters
            return currentState |> Seq.toList
        }

    [<Test>]
    let ``batch processing with framework focus intercepts tabs`` () =
        // This test exercises the focus-aware batching path by including tabs and
        // verifying they are intercepted by the framework (not passed through to the processor).
        // We inject tabs into the input to guarantee coverage of the tab-interception path.
        let property
            (batchSize1 : int)
            (batchSizes : int list)
            (nonTabChar : char)
            (nonTabChars : char list)
            (tabPositions : int list)
            =
            task {
                let batchSizes = (batchSize1 :: batchSizes) |> List.map (fun i -> abs i + 1)

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

                let! result = processWithBatchStrategyAndFocus keystrokes batchSizes

                // In framework focus mode, tabs are intercepted for focus cycling
                // and don't appear in the processed output
                let expected = keyChars |> List.filter (fun c -> c <> '\t')

                return result |> shouldEqual expected
            }

        Check.One (propConfig, property)

    [<NoComparison>]
    type ModeSwitchingState =
        {
            ProcessedChars : char list
            UseFrameworkFocus : bool
            LastFocusedKey : NodeKey option
        }

    [<Test>]
    let ``switching between manual and framework tab handling works correctly`` () =
        task {
            let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 24)
            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let initialState =
                {
                    ProcessedChars = []
                    UseFrameworkFocus = false
                    LastFocusedKey = None
                }

            let mutable switchModeAfterNextEvent = false

            let processWorld =
                { new WorldProcessor<unit, unit, ModeSwitchingState> with
                    member _.ProcessWorld (inputs, vdomContext, state) =
                        let mutable newState =
                            { state with
                                LastFocusedKey = vdomContext.FocusedKey
                            }

                        let mutable shouldRerender = false

                        for i = 0 to inputs.Length - 1 do
                            match inputs.[i] with
                            | WorldStateChange.Keystroke c ->
                                // Record all non-tab characters, and tabs when in manual mode
                                if c.Key <> ConsoleKey.Tab || not state.UseFrameworkFocus then
                                    newState <-
                                        { newState with
                                            ProcessedChars = newState.ProcessedChars @ [ c.KeyChar ]
                                        }

                                // Check if we should switch modes
                                if switchModeAfterNextEvent then
                                    switchModeAfterNextEvent <- false

                                    newState <-
                                        { newState with
                                            UseFrameworkFocus = not newState.UseFrameworkFocus
                                        }

                                    shouldRerender <- true
                            | WorldStateChange.MouseEvent _ -> ()
                            | WorldStateChange.ApplicationEvent _ -> ()
                            | WorldStateChange.Paste _ -> ()
                            | WorldStateChange.ApplicationEventException _ -> ()

                        if shouldRerender then
                            ProcessWorldResult.make newState
                            |> ProcessWorldResult.withRerender (inputs.Length - 1)
                        else
                            ProcessWorldResult.make newState

                    member _.ProcessPostLayoutEvents (_events, _ctx, state) = state
                }

            let mutable vdomRenderCount = 0

            let vdom (vdomContext : IVdomContext<_>) (_state : ModeSwitchingState) =
                vdomRenderCount <- vdomRenderCount + 1

                let checkbox0 =
                    Components.Checkbox.make (vdomContext, NodeKey.make "checkbox0", false)

                let checkbox1 =
                    Components.Checkbox.make (vdomContext, NodeKey.make "checkbox1", false)

                Vdom.panelSplitAbsolute (SplitDirection.Vertical, -3, checkbox0, checkbox1)

            let renderState = RenderState.make console MockTime.getStaticUtcNow None
            let mutable currentState = initialState

            // Initial render
            currentState <-
                App.pumpOnce
                    worldFreezer
                    currentState
                    (fun s -> s.UseFrameworkFocus)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            vdomRenderCount |> shouldEqual 1

            // Phase 1: Manual mode - tabs should pass through
            world.SendKey (ConsoleKeyInfo ('a', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            world.SendKey (ConsoleKeyInfo ('b', ConsoleKey.NoName, false, false, false))

            currentState <-
                App.pumpOnce
                    worldFreezer
                    currentState
                    (fun s -> s.UseFrameworkFocus)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            vdomRenderCount |> shouldEqual 2

            currentState.ProcessedChars |> shouldEqual [ 'a' ; '\t' ; 'b' ]
            currentState.UseFrameworkFocus |> shouldEqual false
            currentState.LastFocusedKey |> shouldEqual None

            // Switch to framework mode
            switchModeAfterNextEvent <- true
            world.SendKey (ConsoleKeyInfo ('X', ConsoleKey.NoName, false, false, false))

            currentState <-
                App.pumpOnce
                    worldFreezer
                    currentState
                    (fun s -> s.UseFrameworkFocus)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            vdomRenderCount |> shouldEqual 3

            currentState.ProcessedChars |> shouldEqual [ 'a' ; '\t' ; 'b' ; 'X' ]
            currentState.UseFrameworkFocus |> shouldEqual true

            // Phase 2: Framework mode - first tab should advance focus to checkbox0
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            world.SendKey (ConsoleKeyInfo ('Z', ConsoleKey.NoName, false, false, false))

            currentState <-
                App.pumpOnce
                    worldFreezer
                    currentState
                    (fun s -> s.UseFrameworkFocus)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            vdomRenderCount |> shouldEqual 4

            // Tab was intercepted by framework, so shouldn't appear in processed chars
            currentState.ProcessedChars |> shouldEqual [ 'a' ; '\t' ; 'b' ; 'X' ; 'Z' ]
            // Focus should now be on checkbox0 (captured when processing 'Z')
            currentState.LastFocusedKey |> shouldEqual (Some (NodeKey.make "checkbox0"))

            // Phase 3: Framework mode - second tab should advance focus to checkbox1
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            world.SendKey (ConsoleKeyInfo ('c', ConsoleKey.NoName, false, false, false))

            currentState <-
                App.pumpOnce
                    worldFreezer
                    currentState
                    (fun s -> s.UseFrameworkFocus)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            vdomRenderCount |> shouldEqual 5

            currentState.ProcessedChars
            |> shouldEqual [ 'a' ; '\t' ; 'b' ; 'X' ; 'Z' ; 'c' ]

            currentState.LastFocusedKey |> shouldEqual (Some (NodeKey.make "checkbox1"))

            // Switch back to manual mode
            switchModeAfterNextEvent <- true
            world.SendKey (ConsoleKeyInfo ('Y', ConsoleKey.NoName, false, false, false))

            currentState <-
                App.pumpOnce
                    worldFreezer
                    currentState
                    (fun s -> s.UseFrameworkFocus)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            vdomRenderCount |> shouldEqual 6

            currentState.ProcessedChars
            |> shouldEqual [ 'a' ; '\t' ; 'b' ; 'X' ; 'Z' ; 'c' ; 'Y' ]

            currentState.UseFrameworkFocus |> shouldEqual false

            // Phase 4: Back in manual mode - tabs should pass through again
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            world.SendKey (ConsoleKeyInfo ('d', ConsoleKey.NoName, false, false, false))

            currentState <-
                App.pumpOnce
                    worldFreezer
                    currentState
                    (fun s -> s.UseFrameworkFocus)
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            vdomRenderCount |> shouldEqual 7

            currentState.ProcessedChars
            |> shouldEqual [ 'a' ; '\t' ; 'b' ; 'X' ; 'Z' ; 'c' ; 'Y' ; '\t' ; 'd' ]
            // Focus should stay at checkbox1 (framework no longer managing it)
            currentState.LastFocusedKey |> shouldEqual (Some (NodeKey.make "checkbox1"))
        }

    [<Test>]
    let ``Rerender forces vdom evaluation and reprocesses remaining batch`` () =
        let property (batchSize1 : int) (batchSizes : int list) =
            task {
                // Generate batch sizes - how many events we process before requesting rerender
                let batchSizes =
                    (batchSize1 :: batchSizes)
                    |> List.map (fun i -> (abs i % 5) + 1) // Between 1 and 5 events per batch
                    |> List.truncate 10

                let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 24)
                let world = MockWorld.make ()

                use worldFreezer =
                    WorldFreezer.listen'
                        UnrecognisedEscapeCodeBehaviour.Throw
                        StopwatchMock.Empty
                        world.KeyAvailable
                        world.ReadKey

                let mutable vdomRenderCount = 0
                let mutable batchIndex = 0

                let processWorld =
                    { new WorldProcessor<unit, unit, char list> with
                        member _.ProcessWorld (inputs, _vdomContext, state) =
                            // Process only batchSize events from this batch, then request Rerender
                            let batchSize =
                                if batchIndex < batchSizes.Length then
                                    batchSizes.[batchIndex]
                                else
                                    inputs.Length // Process everything

                            batchIndex <- batchIndex + 1

                            let toProcess = min batchSize inputs.Length
                            let mutable newState = state

                            // Process the events
                            for i = 0 to toProcess - 1 do
                                match inputs.[i] with
                                | WorldStateChange.Keystroke c -> newState <- newState @ [ c.KeyChar ]
                                | _ -> ()

                            if toProcess < inputs.Length then
                                // We didn't process everything, request Rerender to split the batch
                                ProcessWorldResult.make newState
                                |> ProcessWorldResult.withRerender (toProcess - 1)
                            else
                                ProcessWorldResult.make newState

                        member _.ProcessPostLayoutEvents (_events, _ctx, state) = state
                    }

                let vdom (_vdomContext : IVdomContext<_>) (_state : char list) =
                    vdomRenderCount <- vdomRenderCount + 1
                    Vdom.textContent ""

                let renderState = RenderState.make console MockTime.getStaticUtcNow None
                let mutable currentState = []

                // Initial render
                currentState <-
                    App.pumpOnce
                        worldFreezer
                        currentState
                        (fun _ -> false)
                        renderState
                        processWorld
                        vdom
                        ActivationResolver.none
                        (fun () -> false)

                let initialRenderCount = vdomRenderCount

                // Send a batch of keystrokes
                let totalKeystrokes = 10

                for i in 0 .. totalKeystrokes - 1 do
                    world.SendKey (ConsoleKeyInfo (char (int 'a' + i), ConsoleKey.NoName, false, false, false))

                currentState <-
                    App.pumpOnce
                        worldFreezer
                        currentState
                        (fun _ -> false)
                        renderState
                        processWorld
                        vdom
                        ActivationResolver.none
                        (fun () -> false)

                // Verify all events were processed in order
                let expectedChars = [ 'a' .. char (int 'a' + totalKeystrokes - 1) ]
                currentState |> shouldEqual expectedChars

                // Verify that vdom was rendered for each Rerender request
                // Count how many times we split the batch (didn't process everything)
                let mutable expectedSplits = 0
                let mutable remaining = totalKeystrokes

                for size in batchSizes do
                    if remaining > size then
                        expectedSplits <- expectedSplits + 1
                        remaining <- remaining - size
                    else
                        remaining <- 0

                // Expected renders = initial + state changes (1 for the pump) + rerender requests (splits)
                let expectedRenderCount = initialRenderCount + 1 + expectedSplits

                return vdomRenderCount |> shouldEqual expectedRenderCount
            }

        Check.One (propConfig, property)
