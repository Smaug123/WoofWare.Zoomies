namespace WoofWare.Zoomies.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies

type PostLayoutEvent = | ViewportHeightReported of height : int

type PostLayoutState =
    {
        ReportedViewportHeight : int option
    }

    static member Initial =
        {
            ReportedViewportHeight = None
        }

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPostLayoutEvents =

    [<Test>]
    let ``component can post layout event during render`` () =
        task {
            // A component that reports its viewport height via a post-layout event
            let viewportReporter (ctx : IVdomContext<PostLayoutEvent>) : Vdom<DesiredBounds> =
                let measure (constraints : MeasureConstraints) =
                    {
                        MinWidth = 10
                        PreferredWidth = 80
                        MaxWidth = None
                        MinHeightForWidth = fun _ -> 1
                        PreferredHeightForWidth = fun _ -> 10
                        MaxHeightForWidth = fun _ -> None
                    }

                let render (bounds : Rectangle) =
                    // Post the viewport height as an event
                    ctx.PostLayoutEvent (ViewportHeightReported bounds.Height)
                    Vdom.textContent $"Height: %d{bounds.Height}"

                Vdom.flexibleContent measure render

            let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 24)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<PostLayoutEvent, PostLayoutState> with
                    member _.ProcessWorld (inputs, _renderState, state) =
                        let mutable newState = state

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent (ViewportHeightReported h) ->
                                newState <-
                                    { newState with
                                        ReportedViewportHeight = Some h
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState
                }

            let vdom (ctx : IVdomContext<PostLayoutEvent>) (_state : PostLayoutState) : Vdom<DesiredBounds> =
                viewportReporter ctx

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            // Run one pump cycle
            let finalState =
                App.pumpOnce
                    worldFreezer
                    PostLayoutState.Initial
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            // The viewport height should have been reported via the post-layout event
            finalState.ReportedViewportHeight |> shouldEqual (Some 24)
        }

    [<Test>]
    let ``multiple post-layout events are all processed`` () =
        task {
            // A component that posts multiple events
            let multiEventComponent (ctx : IVdomContext<PostLayoutEvent>) : Vdom<DesiredBounds> =
                let measure (constraints : MeasureConstraints) =
                    {
                        MinWidth = 10
                        PreferredWidth = 80
                        MaxWidth = None
                        MinHeightForWidth = fun _ -> 1
                        PreferredHeightForWidth = fun _ -> 10
                        MaxHeightForWidth = fun _ -> None
                    }

                let render (bounds : Rectangle) =
                    // Post multiple events
                    ctx.PostLayoutEvent (ViewportHeightReported bounds.Height)
                    ctx.PostLayoutEvent (ViewportHeightReported (bounds.Height * 2))
                    Vdom.textContent "Multi-event"

                Vdom.flexibleContent measure render

            let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 10)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let receivedHeights = ResizeArray<int> ()

            let processWorld =
                { new WorldProcessor<PostLayoutEvent, PostLayoutState> with
                    member _.ProcessWorld (inputs, _renderState, state) =
                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent (ViewportHeightReported h) -> receivedHeights.Add h
                            | _ -> ()

                        ProcessWorldResult.make state
                }

            let vdom (ctx : IVdomContext<PostLayoutEvent>) (_state : PostLayoutState) : Vdom<DesiredBounds> =
                multiEventComponent ctx

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.pumpOnce
                worldFreezer
                PostLayoutState.Initial
                haveFrameworkHandleFocus
                renderState
                processWorld
                vdom
                ActivationResolver.none
                (fun () -> false)
            |> ignore<PostLayoutState>

            // Both events should have been received
            receivedHeights.Count |> shouldEqual 2
            receivedHeights.[0] |> shouldEqual 10
            receivedHeights.[1] |> shouldEqual 20
        }

    [<Test>]
    let ``post-layout events work with processNoChanges`` () =
        task {
            // Test that post-layout events work even when there are no incoming world changes
            let viewportReporter (ctx : IVdomContext<PostLayoutEvent>) : Vdom<DesiredBounds> =
                let measure (constraints : MeasureConstraints) =
                    {
                        MinWidth = 10
                        PreferredWidth = 80
                        MaxWidth = None
                        MinHeightForWidth = fun _ -> 1
                        PreferredHeightForWidth = fun _ -> 10
                        MaxHeightForWidth = fun _ -> None
                    }

                let render (bounds : Rectangle) =
                    ctx.PostLayoutEvent (ViewportHeightReported bounds.Height)
                    Vdom.textContent $"Height: %d{bounds.Height}"

                Vdom.flexibleContent measure render

            let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 15)

            let processWorld =
                { new WorldProcessor<PostLayoutEvent, PostLayoutState> with
                    member _.ProcessWorld (inputs, _renderState, state) =
                        let mutable newState = state

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent (ViewportHeightReported h) ->
                                newState <-
                                    { newState with
                                        ReportedViewportHeight = Some h
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState
                }

            let vdom (ctx : IVdomContext<PostLayoutEvent>) (_state : PostLayoutState) : Vdom<DesiredBounds> =
                viewportReporter ctx

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            // Use processNoChanges directly (simulating no incoming events)
            let finalState =
                App.processNoChanges PostLayoutState.Initial renderState processWorld vdom

            // The viewport height should have been reported
            finalState.ReportedViewportHeight |> shouldEqual (Some 15)
        }

    type ChainedStabilizationState =
        {
            Counter : int
            StabilizationComplete : bool
        }

        static member Initial =
            {
                Counter = 0
                StabilizationComplete = false
            }

    type ChainedEvent =
        | Increment
        | MarkComplete

    [<Test>]
    let ``post-layout events that trigger state changes cause re-renders`` () =
        task {
            // A component that posts Increment events until counter reaches target, then MarkComplete
            let chainedComponent
                (targetCount : int)
                (ctx : IVdomContext<ChainedEvent>)
                (state : ChainedStabilizationState)
                : Vdom<DesiredBounds>
                =
                let measure (_constraints : MeasureConstraints) =
                    {
                        MinWidth = 10
                        PreferredWidth = 80
                        MaxWidth = None
                        MinHeightForWidth = fun _ -> 1
                        PreferredHeightForWidth = fun _ -> 10
                        MaxHeightForWidth = fun _ -> None
                    }

                let render (_bounds : Rectangle) =
                    // Post events based on current state
                    if state.Counter < targetCount then
                        ctx.PostLayoutEvent Increment
                    elif not state.StabilizationComplete then
                        ctx.PostLayoutEvent MarkComplete

                    Vdom.textContent $"Counter: %d{state.Counter}"

                Vdom.flexibleContent measure render

            let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 24)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<ChainedEvent, ChainedStabilizationState> with
                    member _.ProcessWorld (inputs, _renderState, state) =
                        let mutable newState = state

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent Increment ->
                                newState <-
                                    { newState with
                                        Counter = newState.Counter + 1
                                    }
                            | WorldStateChange.ApplicationEvent MarkComplete ->
                                newState <-
                                    { newState with
                                        StabilizationComplete = true
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState
                }

            let targetCount = 5

            let vdom (ctx : IVdomContext<ChainedEvent>) (state : ChainedStabilizationState) : Vdom<DesiredBounds> =
                chainedComponent targetCount ctx state

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            let finalState =
                App.pumpOnce
                    worldFreezer
                    ChainedStabilizationState.Initial
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            // The counter should have reached the target through chained stabilization
            finalState.Counter |> shouldEqual targetCount
            // And the completion event should have been processed
            finalState.StabilizationComplete |> shouldEqual true
        }

    type InfiniteLoopState =
        {
            IterationCount : int
        }

        static member Initial =
            {
                IterationCount = 0
            }

    type InfiniteLoopEvent = | KeepGoing

    [<Test>]
    let ``stabilization loop respects max iteration limit`` () =
        task {
            // A component that always posts events, creating an infinite loop
            let infiniteComponent (ctx : IVdomContext<InfiniteLoopEvent>) : Vdom<DesiredBounds> =
                let measure (_constraints : MeasureConstraints) =
                    {
                        MinWidth = 10
                        PreferredWidth = 80
                        MaxWidth = None
                        MinHeightForWidth = fun _ -> 1
                        PreferredHeightForWidth = fun _ -> 10
                        MaxHeightForWidth = fun _ -> None
                    }

                let render (_bounds : Rectangle) =
                    // Always post an event - this would loop forever without the max iterations limit
                    ctx.PostLayoutEvent KeepGoing
                    Vdom.textContent "Infinite"

                Vdom.flexibleContent measure render

            let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 24)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<InfiniteLoopEvent, InfiniteLoopState> with
                    member _.ProcessWorld (inputs, _renderState, state) =
                        let mutable newState = state

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent KeepGoing ->
                                newState <-
                                    { newState with
                                        IterationCount = newState.IterationCount + 1
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState
                }

            let vdom (ctx : IVdomContext<InfiniteLoopEvent>) (_state : InfiniteLoopState) : Vdom<DesiredBounds> =
                infiniteComponent ctx

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            let finalState =
                App.pumpOnce
                    worldFreezer
                    InfiniteLoopState.Initial
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            // The iteration count should be capped at MAX_POST_LAYOUT_ITERATIONS (100)
            finalState.IterationCount |> shouldEqual 100
        }
