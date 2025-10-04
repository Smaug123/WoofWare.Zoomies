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
                            {
                                NewState = newState
                                RequestRerender = RerenderRequest.NewBatch (toProcess - 1)
                            }
                }

            let vdom (_vdomContext : VdomContext) (_state : ImmutableArray<char>) = Vdom.textContent false ""

            let renderState = RenderState.make' console
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
