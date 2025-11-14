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
                { new WorldProcessor<obj, ImmutableArray<char>> with
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
                            | WorldStateChange.KeyboardEvent _ -> ()
                            | WorldStateChange.ApplicationEventException _ -> ()

                        totalProcessed <- totalProcessed + toProcess

                        if toProcess >= inputs.Length then
                            // Processed everything in this batch
                            ProcessWorldResult.make newState
                        else
                            // Request a new batch after processing 'toProcess' items
                            ProcessWorldResult.make newState
                            |> ProcessWorldResult.withRerender (toProcess - 1)
                }

            let vdom (_vdomContext : VdomContext) (_state : ImmutableArray<char>) = Vdom.textContent false ""

            let renderState = RenderState.make' console None
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

                iterations <- iterations + 1

            // Return the final processed characters
            return currentState |> Seq.toList
        }

    [<TestCase true>]
    [<TestCase false>]
    let ``batch processing handles all events in order`` (frameworkHandleFocus : bool) =
        let property (batchSize1 : int) (batchSizes : int list) (keyChars : char list) =
            task {
                let batchSizes = (batchSize1 :: batchSizes) |> List.map (fun i -> abs i + 1)

                let keyChars =
                    if frameworkHandleFocus then
                        // Filter out tab characters to avoid focus cycling interference
                        keyChars |> List.filter (fun c -> c <> '\t')
                    else
                        keyChars

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

                    let! result = processWithBatchStrategy frameworkHandleFocus keystrokes batchSizes

                    let expected =
                        if frameworkHandleFocus then
                            // Tabs are handled by the framework when haveFrameworkHandleFocus is true,
                            // so they won't appear in the processed list
                            keyChars |> List.filter (fun c -> c <> '\t')
                        else
                            keyChars

                    return result |> shouldEqual expected
            }

        Check.One (propConfig, property)

    [<TestCase true>]
    [<TestCase false>]
    let ``single event processing eventually processes everything`` (frameworkHandlesFocus : bool) =
        // Edge case: always process exactly one event per batch
        let property (keyChars : char list) =
            task {
                let keyChars = keyChars |> List.filter (fun c -> c <> '\t')

                if List.isEmpty keyChars then
                    return ()
                else
                    let keystrokes =
                        keyChars
                        |> List.map (fun c -> ConsoleKeyInfo (c, ConsoleKey.NoName, false, false, false))

                    let! result = processWithBatchStrategy frameworkHandlesFocus keystrokes [ 1 ]
                    let expected = keyChars

                    return result |> shouldEqual expected
            }

        Check.One (propConfig, property)

    [<TestCase true>]
    [<TestCase false>]
    let ``large batch processing eventually processes everything`` (frameworkHandlesFocus : bool) =
        // Edge case: try to process all events in one go (but framework may split)
        let property (keyChars : char list) =
            task {
                let keyChars = keyChars |> List.filter (fun c -> c <> '\t')

                if List.isEmpty keyChars then
                    return ()
                else
                    let keystrokes =
                        keyChars
                        |> List.map (fun c -> ConsoleKeyInfo (c, ConsoleKey.NoName, false, false, false))

                    let! result = processWithBatchStrategy frameworkHandlesFocus keystrokes [ 1000 ]
                    let expected = keyChars

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
                { new WorldProcessor<obj, ModeSwitchingState> with
                    member _.ProcessWorld (inputs, vdomContext, state) =
                        let mutable newState =
                            { state with
                                LastFocusedKey = VdomContext.focusedKey vdomContext
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
                            | WorldStateChange.KeyboardEvent _ -> ()
                            | WorldStateChange.ApplicationEventException _ -> ()

                        if shouldRerender then
                            ProcessWorldResult.make newState
                            |> ProcessWorldResult.withRerender (inputs.Length - 1)
                        else
                            ProcessWorldResult.make newState
                }

            let mutable vdomRenderCount = 0

            let vdom (vdomContext : VdomContext) (_state : ModeSwitchingState) =
                vdomRenderCount <- vdomRenderCount + 1
                let currentFocus = VdomContext.focusedKey vdomContext

                let checkbox0 =
                    Vdom.checkbox (currentFocus = Some (NodeKey.make "checkbox0")) false
                    |> Vdom.withKey (NodeKey.make "checkbox0")
                    |> Vdom.withFocusTracking

                let checkbox1 =
                    Vdom.checkbox (currentFocus = Some (NodeKey.make "checkbox1")) false
                    |> Vdom.withKey (NodeKey.make "checkbox1")
                    |> Vdom.withFocusTracking

                Vdom.panelSplitAbsolute (SplitDirection.Vertical, -3, checkbox0, checkbox1)

            let renderState = RenderState.make' console None
            let mutable currentState = initialState

            // Initial render
            currentState <-
                App.pumpOnce worldFreezer currentState (fun s -> s.UseFrameworkFocus) renderState processWorld vdom

            vdomRenderCount |> shouldEqual 1

            // Phase 1: Manual mode - tabs should pass through
            world.SendKey (ConsoleKeyInfo ('a', ConsoleKey.NoName, false, false, false))
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            world.SendKey (ConsoleKeyInfo ('b', ConsoleKey.NoName, false, false, false))

            currentState <-
                App.pumpOnce worldFreezer currentState (fun s -> s.UseFrameworkFocus) renderState processWorld vdom

            vdomRenderCount |> shouldEqual 2

            currentState.ProcessedChars |> shouldEqual [ 'a' ; '\t' ; 'b' ]
            currentState.UseFrameworkFocus |> shouldEqual false
            currentState.LastFocusedKey |> shouldEqual None

            // Switch to framework mode
            switchModeAfterNextEvent <- true
            world.SendKey (ConsoleKeyInfo ('X', ConsoleKey.NoName, false, false, false))

            currentState <-
                App.pumpOnce worldFreezer currentState (fun s -> s.UseFrameworkFocus) renderState processWorld vdom

            vdomRenderCount |> shouldEqual 3

            currentState.ProcessedChars |> shouldEqual [ 'a' ; '\t' ; 'b' ; 'X' ]
            currentState.UseFrameworkFocus |> shouldEqual true

            // Phase 2: Framework mode - first tab should advance focus to checkbox0
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            world.SendKey (ConsoleKeyInfo ('Z', ConsoleKey.NoName, false, false, false))

            currentState <-
                App.pumpOnce worldFreezer currentState (fun s -> s.UseFrameworkFocus) renderState processWorld vdom

            vdomRenderCount |> shouldEqual 4

            // Tab was intercepted by framework, so shouldn't appear in processed chars
            currentState.ProcessedChars |> shouldEqual [ 'a' ; '\t' ; 'b' ; 'X' ; 'Z' ]
            // Focus should now be on checkbox0 (captured when processing 'Z')
            currentState.LastFocusedKey |> shouldEqual (Some (NodeKey.make "checkbox0"))

            // Phase 3: Framework mode - second tab should advance focus to checkbox1
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            world.SendKey (ConsoleKeyInfo ('c', ConsoleKey.NoName, false, false, false))

            currentState <-
                App.pumpOnce worldFreezer currentState (fun s -> s.UseFrameworkFocus) renderState processWorld vdom

            vdomRenderCount |> shouldEqual 5

            currentState.ProcessedChars
            |> shouldEqual [ 'a' ; '\t' ; 'b' ; 'X' ; 'Z' ; 'c' ]

            currentState.LastFocusedKey |> shouldEqual (Some (NodeKey.make "checkbox1"))

            // Switch back to manual mode
            switchModeAfterNextEvent <- true
            world.SendKey (ConsoleKeyInfo ('Y', ConsoleKey.NoName, false, false, false))

            currentState <-
                App.pumpOnce worldFreezer currentState (fun s -> s.UseFrameworkFocus) renderState processWorld vdom

            vdomRenderCount |> shouldEqual 6

            currentState.ProcessedChars
            |> shouldEqual [ 'a' ; '\t' ; 'b' ; 'X' ; 'Z' ; 'c' ; 'Y' ]

            currentState.UseFrameworkFocus |> shouldEqual false

            // Phase 4: Back in manual mode - tabs should pass through again
            world.SendKey (ConsoleKeyInfo ('\t', ConsoleKey.Tab, false, false, false))
            world.SendKey (ConsoleKeyInfo ('d', ConsoleKey.NoName, false, false, false))

            currentState <-
                App.pumpOnce worldFreezer currentState (fun s -> s.UseFrameworkFocus) renderState processWorld vdom

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
                    { new WorldProcessor<obj, char list> with
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
                    }

                let vdom (_vdomContext : VdomContext) (_state : char list) =
                    vdomRenderCount <- vdomRenderCount + 1
                    Vdom.textContent false ""

                let renderState = RenderState.make' console None
                let mutable currentState = []

                // Initial render
                currentState <- App.pumpOnce worldFreezer currentState (fun _ -> false) renderState processWorld vdom

                let initialRenderCount = vdomRenderCount

                // Send a batch of keystrokes
                let totalKeystrokes = 10

                for i in 0 .. totalKeystrokes - 1 do
                    world.SendKey (ConsoleKeyInfo (char (int 'a' + i), ConsoleKey.NoName, false, false, false))

                currentState <- App.pumpOnce worldFreezer currentState (fun _ -> false) renderState processWorld vdom

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
