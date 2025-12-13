namespace WoofWare.Zoomies.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies

[<Struct>]
type NoState = | NoState

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
                { new WorldProcessor<unit, PostLayoutEvent, PostLayoutState> with
                    member _.ProcessWorld (_inputs, _renderState, state) = ProcessWorldResult.make state

                    member _.ProcessPostLayoutEvents (events, _ctx, state) =
                        let mutable newState = state

                        for event in events do
                            match event with
                            | ViewportHeightReported h ->
                                newState <-
                                    { newState with
                                        ReportedViewportHeight = Some h
                                    }

                        newState
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
                { new WorldProcessor<unit, PostLayoutEvent, PostLayoutState> with
                    member _.ProcessWorld (_inputs, _renderState, state) = ProcessWorldResult.make state

                    member _.ProcessPostLayoutEvents (events, _ctx, state) =
                        for event in events do
                            match event with
                            | ViewportHeightReported h -> receivedHeights.Add h

                        state
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
                { new WorldProcessor<unit, PostLayoutEvent, PostLayoutState> with
                    member _.ProcessWorld (_inputs, _renderState, state) = ProcessWorldResult.make state

                    member _.ProcessPostLayoutEvents (events, _ctx, state) =
                        let mutable newState = state

                        for event in events do
                            match event with
                            | ViewportHeightReported h ->
                                newState <-
                                    { newState with
                                        ReportedViewportHeight = Some h
                                    }

                        newState
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
                { new WorldProcessor<unit, ChainedEvent, ChainedStabilizationState> with
                    member _.ProcessWorld (_inputs, _renderState, state) = ProcessWorldResult.make state

                    member _.ProcessPostLayoutEvents (events, _ctx, state) =
                        let mutable newState = state

                        for event in events do
                            match event with
                            | Increment ->
                                newState <-
                                    { newState with
                                        Counter = newState.Counter + 1
                                    }
                            | MarkComplete ->
                                newState <-
                                    { newState with
                                        StabilizationComplete = true
                                    }

                        newState
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
                { new WorldProcessor<unit, InfiniteLoopEvent, InfiniteLoopState> with
                    member _.ProcessWorld (_inputs, _renderState, state) = ProcessWorldResult.make state

                    member _.ProcessPostLayoutEvents (events, _ctx, state) =
                        let mutable newState = state

                        for event in events do
                            match event with
                            | KeepGoing ->
                                newState <-
                                    { newState with
                                        IterationCount = newState.IterationCount + 1
                                    }

                        newState
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

    // ===================================================================================
    // Tests for event ordering guarantees
    // ===================================================================================

    type OrderingEvent =
        | PostLayoutEvent of id : int
        | UserTriggeredEvent of id : int

    type OrderingState =
        {
            EventLog : OrderingEvent list
            RenderCount : int
        }

        static member Initial =
            {
                EventLog = []
                RenderCount = 0
            }

    [<Test>]
    let ``post-layout events from render are stabilized before continuing`` () =
        task {
            // This test demonstrates that post-layout events generated by a render
            // are fully stabilized (processed and any consequent re-renders complete)
            // before the processing loop continues.
            //
            // We test this by having a component that:
            // 1. Posts a post-layout event on first render after user interaction
            // 2. Processing that event changes state
            // 3. Re-render posts another event (up to a limit)
            // 4. All these are stabilized before returning from pumpOnce

            let mutable renderCount = 0

            let orderingComponent (ctx : IVdomContext<OrderingEvent>) (state : OrderingState) : Vdom<DesiredBounds> =
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
                    renderCount <- renderCount + 1

                    // Post layout events only after user interaction, and only up to 3 times
                    let postLayoutEventCount =
                        state.EventLog
                        |> List.filter (
                            function
                            | PostLayoutEvent _ -> true
                            | _ -> false
                        )
                        |> List.length

                    if state.EventLog.Length > 0 && postLayoutEventCount < 3 then
                        ctx.PostLayoutEvent (PostLayoutEvent renderCount)

                    Vdom.textContent $"Render %d{renderCount}"

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
                { new WorldProcessor<unit, OrderingEvent, OrderingState> with
                    member _.ProcessWorld (inputs, _renderState, state) =
                        let mutable newState = state

                        for input in inputs do
                            match input with
                            | WorldStateChange.Keystroke k when k.Key = ConsoleKey.A ->
                                newState <-
                                    { newState with
                                        EventLog = newState.EventLog @ [ UserTriggeredEvent newState.EventLog.Length ]
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState

                    member _.ProcessPostLayoutEvents (events, _ctx, state) =
                        let mutable newState = state

                        for evt in events do
                            newState <-
                                { newState with
                                    EventLog = newState.EventLog @ [ evt ]
                                }

                        newState
                }

            let vdom (ctx : IVdomContext<OrderingEvent>) (state : OrderingState) : Vdom<DesiredBounds> =
                orderingComponent ctx state

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            // Do initial render to establish baseline
            let stateAfterInit =
                App.processNoChanges OrderingState.Initial renderState processWorld vdom

            stateAfterInit.EventLog |> shouldEqual []

            // Queue a single keystroke
            world.SendKey (ConsoleKeyInfo ('a', ConsoleKey.A, false, false, false))

            let finalState =
                App.pumpOnce
                    worldFreezer
                    stateAfterInit
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            // After processing:
            // 1. User event is processed (batch of 1 keystroke)
            // 2. Render happens (state changed)
            // 3. Post-layout event 1 is posted and stabilized
            // 4. State changes, re-render, post-layout event 2 posted and stabilized
            // 5. State changes, re-render, post-layout event 3 posted and stabilized
            // 6. Component stops posting (limit reached), stabilization completes
            //
            // All post-layout events are processed via stabilization after the batch
            finalState.EventLog.Length |> shouldEqual 4 // 1 user + 3 post-layout
            finalState.EventLog |> List.head |> shouldEqual (UserTriggeredEvent 0)

            // All subsequent events should be PostLayoutEvents
            finalState.EventLog
            |> List.tail
            |> List.forall (
                function
                | PostLayoutEvent _ -> true
                | _ -> false
            )
            |> shouldEqual true
        }

    [<Test>]
    let ``post-layout events preserve order when multiple are posted`` () =
        task {
            // If a component posts events A, B, C in one render, they should be delivered in that order

            let orderedComponent (ctx : IVdomContext<int>) : Vdom<DesiredBounds> =
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
                    // Post events in specific order
                    ctx.PostLayoutEvent 1
                    ctx.PostLayoutEvent 2
                    ctx.PostLayoutEvent 3
                    ctx.PostLayoutEvent 4
                    ctx.PostLayoutEvent 5
                    Vdom.textContent "Ordered"

                Vdom.flexibleContent measure render

            let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 24)

            let receivedEvents = ResizeArray<int> ()

            let processWorld =
                { new WorldProcessor<unit, int, NoState> with
                    member _.ProcessWorld (_inputs, _renderState, state) = ProcessWorldResult.make state

                    member _.ProcessPostLayoutEvents (events, _ctx, state) =
                        for n in events do
                            receivedEvents.Add n

                        state
                }

            let vdom (ctx : IVdomContext<int>) (_state : NoState) : Vdom<DesiredBounds> = orderedComponent ctx

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            App.processNoChanges NoState renderState processWorld vdom |> ignore<NoState>

            // Events should be received in the order they were posted
            receivedEvents |> Seq.toList |> shouldEqual [ 1 ; 2 ; 3 ; 4 ; 5 ]
        }

    type StateUnchangedEvent = | NoOpEvent

    [<Test>]
    let ``stabilization exits when state unchanged even if events posted`` () =
        task {
            // If post-layout events are processed but don't change state, stabilization should exit
            // (no infinite loop of posting events that don't change anything)

            let mutable renderCount = 0
            let mutable eventProcessCount = 0

            let noOpComponent (ctx : IVdomContext<StateUnchangedEvent>) : Vdom<DesiredBounds> =
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
                    renderCount <- renderCount + 1
                    // Always post an event, but processing it won't change state
                    ctx.PostLayoutEvent NoOpEvent
                    Vdom.textContent "NoOp"

                Vdom.flexibleContent measure render

            let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 24)

            let processWorld =
                { new WorldProcessor<unit, StateUnchangedEvent, int> with
                    member _.ProcessWorld (_inputs, _renderState, state) = ProcessWorldResult.make state

                    member _.ProcessPostLayoutEvents (events, _ctx, state) =
                        for event in events do
                            match event with
                            | NoOpEvent -> eventProcessCount <- eventProcessCount + 1

                        // Return the same state - no change
                        state
                }

            let vdom (ctx : IVdomContext<StateUnchangedEvent>) (_state : int) : Vdom<DesiredBounds> = noOpComponent ctx

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            let _finalState = App.processNoChanges 42 renderState processWorld vdom

            // Should have rendered once (initial) and processed the event once
            // But since state didn't change, should NOT re-render
            renderCount |> shouldEqual 1
            eventProcessCount |> shouldEqual 1
        }

    type ActivationEvent =
        | ButtonActivated
        | PostLayoutFromActivation of renderNum : int

    type ActivationState =
        {
            ActivationCount : int
            PostLayoutCount : int
            EventSequence : string list
        }

        static member Initial =
            {
                ActivationCount = 0
                PostLayoutCount = 0
                EventSequence = []
            }

    [<Test>]
    let ``stabilization works during activation path`` () =
        task {
            // When a button is activated, the activation path has its own stabilization.
            // This test verifies that post-layout events generated during activation renders
            // are properly stabilized.

            let activationComponent
                (ctx : IVdomContext<ActivationEvent>)
                (state : ActivationState)
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
                    // Post a layout event that depends on activation state
                    if state.ActivationCount > 0 && state.PostLayoutCount < 3 then
                        ctx.PostLayoutEvent (PostLayoutFromActivation state.PostLayoutCount)

                    Vdom.textContent $"Activated: %d{state.ActivationCount}"

                Vdom.flexibleContent measure render
                |> Vdom.withKey (NodeKey.make "button")
                |> fun keyed -> Vdom.withFocusTracking (keyed, isInitiallyFocused = true)

            let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 24)

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = true

            let processWorld =
                { new WorldProcessor<ActivationEvent, ActivationEvent, ActivationState> with
                    member _.ProcessWorld (inputs, _renderState, state) =
                        let mutable newState = state

                        for input in inputs do
                            match input with
                            | WorldStateChange.ApplicationEvent ButtonActivated ->
                                newState <-
                                    { newState with
                                        ActivationCount = newState.ActivationCount + 1
                                        EventSequence = newState.EventSequence @ [ "activated" ]
                                    }
                            | _ -> ()

                        ProcessWorldResult.make newState

                    member _.ProcessPostLayoutEvents (events, _ctx, state) =
                        let mutable newState = state

                        for event in events do
                            match event with
                            | PostLayoutFromActivation n ->
                                newState <-
                                    { newState with
                                        PostLayoutCount = newState.PostLayoutCount + 1
                                        EventSequence = newState.EventSequence @ [ $"postlayout-%d{n}" ]
                                    }
                            | _ -> ()

                        newState
                }

            let vdom (ctx : IVdomContext<ActivationEvent>) (state : ActivationState) : Vdom<DesiredBounds> =
                activationComponent ctx state

            let buttonKey = NodeKey.make "button"

            let resolveActivation =
                ActivationResolver (fun nodeKey keyInfo _state ->
                    if nodeKey = buttonKey && keyInfo.Key = ConsoleKey.Enter then
                        Some ButtonActivated
                    else
                        None
                )

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            // Do initial render to set up focus
            let stateAfterInit =
                App.processNoChanges ActivationState.Initial renderState processWorld vdom

            // Now send Enter to activate
            world.SendKey (ConsoleKeyInfo ('\r', ConsoleKey.Enter, false, false, false))

            let finalState =
                App.pumpOnce
                    worldFreezer
                    stateAfterInit
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    resolveActivation
                    (fun () -> false)

            // Should have activated once
            finalState.ActivationCount |> shouldEqual 1
            // Should have processed all 3 post-layout events through stabilization
            finalState.PostLayoutCount |> shouldEqual 3
            // Events should be in order: activation first, then all the post-layout stabilization
            finalState.EventSequence
            |> shouldEqual [ "activated" ; "postlayout-0" ; "postlayout-1" ; "postlayout-2" ]
        }

    type RerenderRequestEvent =
        | EventWithRerender
        | EventWithoutRerender

    type RerenderRequestState =
        {
            EventsProcessed : int
            RequestedRerenderDuringPostLayout : bool
        }

        static member Initial =
            {
                EventsProcessed = 0
                RequestedRerenderDuringPostLayout = false
            }

    [<Test>]
    let ``ProcessPostLayoutEvents has no rerender-request mechanism by design`` () =
        task {
            // This test documents that ProcessPostLayoutEvents returns only state, not a ProcessWorldResult.
            // Unlike ProcessWorld (which can return RequestRerender), post-layout event processing
            // has no mechanism to request rerenders - it only returns the new state.
            //
            // The stabilization loop re-renders automatically when state changes, so there's no need
            // for an explicit rerender request. This test verifies that normal stabilization behavior
            // works correctly: state changes trigger re-renders until no more post-layout events are posted.

            let mutable renderCount = 0

            let rerenderComponent
                (ctx : IVdomContext<RerenderRequestEvent>)
                (_state : RerenderRequestState)
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
                    renderCount <- renderCount + 1

                    // Post an event on first render that will cause a state change
                    if renderCount = 1 then
                        ctx.PostLayoutEvent EventWithRerender

                    Vdom.textContent $"Renders: %d{renderCount}"

                Vdom.flexibleContent measure render

            let console, _terminal = ConsoleHarness.make' (fun () -> 80) (fun () -> 24)

            let processWorld =
                { new WorldProcessor<unit, RerenderRequestEvent, RerenderRequestState> with
                    member _.ProcessWorld (_inputs, _renderState, state) = ProcessWorldResult.make state

                    member _.ProcessPostLayoutEvents (events, _ctx, state) =
                        let mutable newState = state

                        for event in events do
                            match event with
                            | EventWithRerender ->
                                newState <-
                                    { newState with
                                        EventsProcessed = newState.EventsProcessed + 1
                                        RequestedRerenderDuringPostLayout = true
                                    }
                            | EventWithoutRerender ->
                                newState <-
                                    { newState with
                                        EventsProcessed = newState.EventsProcessed + 1
                                    }

                        // Note: ProcessPostLayoutEvents returns only 'userState, not ProcessWorldResult.
                        // There is no mechanism to request a rerender; re-renders happen automatically
                        // when the returned state differs from the previous state.
                        newState
                }

            let vdom (ctx : IVdomContext<RerenderRequestEvent>) (state : RerenderRequestState) : Vdom<DesiredBounds> =
                rerenderComponent ctx state

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            let finalState =
                App.processNoChanges RerenderRequestState.Initial renderState processWorld vdom

            // The event was processed
            finalState.EventsProcessed |> shouldEqual 1
            finalState.RequestedRerenderDuringPostLayout |> shouldEqual true

            // State changed, so stabilization triggered a re-render automatically.
            // No more events were posted on the second render, so stabilization exited.
            renderCount |> shouldEqual 2 // Initial render + one stabilization render due to state change
        }

    type IntermediateFrameEvent = | SwitchToFinalState

    type IntermediateFrameState =
        {
            ShowFinal : bool
        }

        static member Initial =
            {
                ShowFinal = false
            }

    [<Test>]
    let ``intermediate frames during stabilization are not painted`` () =
        task {
            // Track what content is flushed to the terminal.
            // We use a double-buffer approach:
            // - pendingBuffer: writes accumulate here during a render
            // - displayBuffer: on Flush, pendingBuffer is copied here (simulating what the user sees)
            // - flushSnapshots: captures displayBuffer at each flush
            //
            // This correctly models how terminals work: writes are buffered until flushed,
            // so the user only sees complete frames.
            let flushSnapshots = ResizeArray<string> ()

            let pendingBuffer = Array2D.create 10 80 ' '
            let mutable cursorX = 0
            let mutable cursorY = 0

            // Create a console that buffers writes until Flush
            let console : IConsole =
                {
                    BackgroundColor = fun () -> ConsoleColor.Black
                    ForegroundColor = fun () -> ConsoleColor.White
                    WindowWidth = fun () -> 80
                    WindowHeight = fun () -> 10
                    ColorMode = ColorMode.Color
                    Execute =
                        fun op ->
                            match op with
                            | TerminalOp.MoveCursor (x, y) ->
                                cursorX <- x
                                cursorY <- y
                            | TerminalOp.WriteRun (text, _, _) ->
                                // Write to pending buffer (not yet visible to user)
                                for ch in text do
                                    if cursorY >= 0 && cursorY < 10 && cursorX >= 0 && cursorX < 80 then
                                        pendingBuffer.[cursorY, cursorX] <- ch
                                        cursorX <- cursorX + 1
                            | TerminalOp.ClearScreen ->
                                for y = 0 to 9 do
                                    for x = 0 to 79 do
                                        pendingBuffer.[y, x] <- ' '
                            | _ -> ()
                    Flush =
                        fun () ->
                            // On flush, snapshot what's being displayed to the user
                            let sb = System.Text.StringBuilder ()

                            for y = 0 to 9 do
                                for x = 0 to 79 do
                                    sb.Append pendingBuffer.[y, x] |> ignore

                            flushSnapshots.Add (sb.ToString ())
                }

            // Component that shows "X" initially, then "Y" after state changes
            let component'
                (ctx : IVdomContext<IntermediateFrameEvent>)
                (state : IntermediateFrameState)
                : Vdom<DesiredBounds>
                =
                let measure (_constraints : MeasureConstraints) =
                    {
                        MinWidth = 1
                        PreferredWidth = 1
                        MaxWidth = None
                        MinHeightForWidth = fun _ -> 1
                        PreferredHeightForWidth = fun _ -> 1
                        MaxHeightForWidth = fun _ -> None
                    }

                let render (_bounds : Rectangle) =
                    if state.ShowFinal then
                        Vdom.textContent "Y"
                    else
                        // First render: show "X" and post event to switch state
                        ctx.PostLayoutEvent SwitchToFinalState
                        Vdom.textContent "X"

                Vdom.flexibleContent measure render

            let world = MockWorld.make ()

            use worldFreezer =
                WorldFreezer.listen'
                    UnrecognisedEscapeCodeBehaviour.Throw
                    StopwatchMock.Empty
                    world.KeyAvailable
                    world.ReadKey

            let haveFrameworkHandleFocus _ = false

            let processWorld =
                { new WorldProcessor<unit, IntermediateFrameEvent, IntermediateFrameState> with
                    member _.ProcessWorld (_inputs, _renderState, state) = ProcessWorldResult.make state

                    member _.ProcessPostLayoutEvents (events, _ctx, state) =
                        let mutable newState = state

                        for event in events do
                            match event with
                            | SwitchToFinalState ->
                                newState <-
                                    {
                                        ShowFinal = true
                                    }

                        newState
                }

            let vdom
                (ctx : IVdomContext<IntermediateFrameEvent>)
                (state : IntermediateFrameState)
                : Vdom<DesiredBounds>
                =
                component' ctx state

            let renderState = RenderState.make console MockTime.getStaticUtcNow None

            // Run one pump cycle - this will:
            // 1. Initial render: component writes "X" to pending buffer, posts SwitchToFinalState event
            // 2. Stabilization: state changes to ShowFinal=true, re-render writes "Y" to pending buffer
            // 3. Only after stabilization completes does Flush copy pending buffer to display
            let _finalState =
                App.pumpOnce
                    worldFreezer
                    IntermediateFrameState.Initial
                    haveFrameworkHandleFocus
                    renderState
                    processWorld
                    vdom
                    ActivationResolver.none
                    (fun () -> false)

            // There should be exactly one flush (after stabilization completes)
            if flushSnapshots.Count <> 1 then
                failwith $"Expected exactly 1 flush but got {flushSnapshots.Count}"

            // The single flushed frame should contain "Y" (final state) but not "X" (intermediate state)
            // This verifies that intermediate renders (before stabilization) are not flushed to the user.
            let flushedFrame = flushSnapshots.[0]

            if flushedFrame.Contains "X" then
                failwith $"Intermediate frame was painted! Flushed content contained 'X': {flushedFrame}"

            if not (flushedFrame.Contains "Y") then
                failwith $"Final frame was not painted! Flushed content did not contain 'Y': {flushedFrame}"
        }
