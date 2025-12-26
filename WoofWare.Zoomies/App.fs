namespace WoofWare.Zoomies

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Threading.Tasks
open WoofWare.Incremental

[<Struct>]
type RerenderRequest =
    /// Don't request a rerender; just let me continue processing the current batch of events.
    | Continue
    /// Truncate the current batch of requests, rerender, and split the rest of the batch into a new iteration of the
    /// render loop.
    /// The index is the last element you processed.
    /// For example, to say you've processed only one element of the batch, you'd return 0 here.
    ///
    /// This feature is here in case you hit a bug in the early cutoff mechanism and need to tell the system to
    /// rerender, but I can't think of any legitimate reason to use this otherwise.
    | Rerender of indexOfLastProcessedEvent : int

/// Make one of these with `ProcessWorldResult.make`.
[<Struct>]
type ProcessWorldResult<'userState> =
    private
        {
            NewState : 'userState
            /// Set this to `Rerender` to request a rerender *now*, rather than continuing to process the rest of the batch
            /// of incoming events.
            /// You might want to do this, for example, if you want WoofWare.Zoomies to ask you again whether you're opted
            /// into automatic focus tracking (which it only does before starting a render).
            RequestRerender : RerenderRequest
        }

[<RequireQualifiedAccess>]
module ProcessWorldResult =
    let make<'userState> (s : 'userState) =
        {
            NewState = s
            RequestRerender = RerenderRequest.Continue
        }

    /// Specify that you have only partially consumed the stream of `WorldStateChange`s (specifically, you have
    /// processed the one at `lastProcessedIndex` but you have not processed any after that).
    ///
    /// If `lastProcessedIndex` is greater than or equal to the length of the input events span, we will correctly
    /// understand that you have processed all entries, but will force a rerender (requesting your vdom again) even if
    /// the cutoff system didn't want to rerender.
    let withRerender (lastProcessedIndex : int) (s : ProcessWorldResult<'userState>) =
        { s with
            RequestRerender = RerenderRequest.Rerender lastProcessedIndex
        }

type WorldProcessor<'appEvent, 'postLayoutEvent, 'userState> =
    abstract ProcessWorld :
        events : ReadOnlySpan<WorldStateChange<'appEvent>> * previousRenderState : IVdomContext * 'userState ->
            ProcessWorldResult<'userState>

    abstract ProcessPostLayoutEvents :
        events : ReadOnlySpan<'postLayoutEvent> * previousRenderState : IVdomContext * 'userState -> 'userState

/// Handle to a running application, providing tasks for lifecycle events.
type AppHandle =
    {
        /// Completes when the app has finished initial setup and rendered for the first time.
        Ready : Task
        /// Completes when the app has finished running (either normally or due to cancellation).
        /// This task will fault if the app throws an exception.
        Finished : Task
    }

[<RequireQualifiedAccess>]
module App =

    /// Maximum number of post-layout event stabilization iterations to prevent infinite loops.
    /// If a component's post-layout event handler keeps triggering renders that generate more
    /// post-layout events, we'll stop after this many iterations.
    [<Literal>]
    let private MAX_POST_LAYOUT_ITERATIONS = 100

    /// Process post-layout events until the queue is empty or max iterations reached.
    /// Returns the final state and whether max iterations was hit.
    let internal stabilizePostLayoutEvents<'state, 'appEvent, 'postLayoutEvent when 'state : equality>
        (state : 'state)
        (renderState : RenderState<'postLayoutEvent>)
        (processWorld : WorldProcessor<'appEvent, 'postLayoutEvent, 'state>)
        (vdom : IVdomContext<'postLayoutEvent> -> 'state -> Vdom<DesiredBounds>)
        : 'state * bool
        =
        let ctx = RenderState.vdomContext renderState
        // Caller must mark clean before calling; PostLayoutEvent can only set dirty during vdom construction,
        // and processWorld receives IVdomContext (not IVdomContext<'postLayoutEvent>) so cannot post events.
        assert (not (VdomContext.isDirty ctx))
        let mutable currentState = state
        let mutable iterations = 0
        let mutable continueLoop = true

        while continueLoop && iterations < MAX_POST_LAYOUT_ITERATIONS do
            let layoutEvents = VdomContext.drainPostLayoutEvents ctx

            if layoutEvents.Length = 0 then
                continueLoop <- false
            else
                let newState =
                    processWorld.ProcessPostLayoutEvents (
                        ReadOnlySpan layoutEvents,
                        VdomContext.asBase ctx,
                        currentState
                    )

                // If state changed, we need to render and potentially get more post-layout events
                if newState <> currentState then
                    currentState <- newState
                    Render.oneStepNoFlush renderState currentState (vdom (VdomContext.asTyped ctx))
                    VdomContext.markClean ctx
                    iterations <- iterations + 1
                else
                    // State didn't change, no need to render, we're done
                    continueLoop <- false

        currentState, iterations >= MAX_POST_LAYOUT_ITERATIONS

    let internal processNoChanges<'state, 'appEvent, 'postLayoutEvent when 'state : equality>
        (state : 'state)
        (renderState : RenderState<'postLayoutEvent>)
        (processWorld : WorldProcessor<'appEvent, 'postLayoutEvent, 'state>)
        (vdom : IVdomContext<'postLayoutEvent> -> 'state -> Vdom<DesiredBounds>)
        : 'state
        =
        let ctx = RenderState.vdomContext renderState

        if VdomContext.isDirty ctx then
            Render.oneStepNoFlush renderState state (vdom (VdomContext.asTyped ctx))
            VdomContext.markClean ctx

            let finalState, _hitLimit =
                stabilizePostLayoutEvents state renderState processWorld vdom

            Render.flush renderState
            finalState
        else
            state

    let internal processChanges<'state, 'appEvent, 'postLayoutEvent when 'state : equality>
        (changes : WorldStateChange<'appEvent>[])
        (state : 'state)
        (haveFrameworkHandleFocus : 'state -> bool)
        (renderState : RenderState<'postLayoutEvent>)
        (processWorld : WorldProcessor<'appEvent, 'postLayoutEvent, 'state>)
        (vdom : IVdomContext<'postLayoutEvent> -> 'state -> Vdom<DesiredBounds>)
        (resolveActivation : ActivationResolver<'appEvent, 'state>)
        (isCancelled : unit -> bool)
        : 'state
        =
        let ctx = RenderState.vdomContext renderState
        let mutable startState = state
        let mutable currentState = state
        let mutable startOfBatch = 0

        while startOfBatch < changes.Length && not (isCancelled ()) do
            let mutable forceRerender = false

            if haveFrameworkHandleFocus currentState then
                // The point is to pass as large a batch as possible to ProcessWorld.
                // Any character that isn't a tab will go into the batch.

                let mutable nextToProcess = startOfBatch

                let processBatch () =
                    let processResult =
                        processWorld.ProcessWorld (
                            changes.AsSpan().Slice (startOfBatch, nextToProcess - startOfBatch),
                            VdomContext.asBase ctx,
                            currentState
                        )

                    currentState <- processResult.NewState

                    match processResult.RequestRerender with
                    | RerenderRequest.Continue ->
                        // Successfully processed everything up but not including the tab.
                        // Just proceed.
                        startOfBatch <- nextToProcess
                        nextToProcess <- Int32.MaxValue
                    | RerenderRequest.Rerender lastProcessed ->
                        forceRerender <- true

                        let len = nextToProcess - startOfBatch

                        if lastProcessed >= len - 1 then
                            // Successfully processed everything up to but not including the tab.
                            // Just proceed.
                            startOfBatch <- nextToProcess
                            nextToProcess <- Int32.MaxValue
                        elif lastProcessed < 0 then
                            failwith "bad index from processing result: was negative"
                        else
                            startOfBatch <- startOfBatch + lastProcessed + 1
                            nextToProcess <- Int32.MaxValue

                while nextToProcess < changes.Length do
                    match Array.get changes nextToProcess with
                    | WorldStateChange.Keystroke t when
                        t.Key = ConsoleKey.Tab
                        && (t.Modifiers = ConsoleModifiers.None || t.Modifiers = ConsoleModifiers.Shift)
                        ->
                        if nextToProcess = startOfBatch then
                            // The tab is at the start of the batch, so we can process it on its own.

                            // Advance focus to the next/prev focusable element
                            if t.Modifiers = ConsoleModifiers.None then
                                RenderState.advanceFocus renderState
                            else
                                assert (t.Modifiers = ConsoleModifiers.Shift)
                                RenderState.retreatFocus renderState

                            // successfully consumed everything up to here; the tab is next to process
                            nextToProcess <- nextToProcess + 1
                            startOfBatch <- nextToProcess

                        else
                            assert (nextToProcess > startOfBatch)

                            // Split artificially at this boundary so that we have a completely fresh vdom just before
                            // the tab.
                            processBatch ()

                    | WorldStateChange.Keystroke k ->
                        // Check if the resolver handles this keystroke
                        match VdomContext.focusedKey ctx with
                        | Some focusedKey ->
                            match resolveActivation.Invoke (focusedKey, k, currentState) with
                            | Some appEvent ->
                                // Activation! Process batch up to here, then inject event
                                // Capture index before processBatch() sets nextToProcess to Int32.MaxValue
                                let idxBeforeProcessing = nextToProcess

                                if nextToProcess > startOfBatch then
                                    processBatch ()

                                // Check if processBatch() processed everything up to the activation.
                                // If not (partial consumption), don't handle activation yet - loop back
                                // to continue processing unprocessed events before the activation.
                                if startOfBatch < idxBeforeProcessing then
                                    // Still have unprocessed events before the activation
                                    // Continue from where processBatch() left off
                                    nextToProcess <- startOfBatch
                                else
                                    // Everything up to the activation has been processed
                                    // Now handle the activation

                                    // Record activation time for visual feedback
                                    VdomContext.recordActivation focusedKey ctx

                                    // Inject the resolved event
                                    let injectedEvent = [| WorldStateChange.ApplicationEvent appEvent |]

                                    let processResult =
                                        processWorld.ProcessWorld (
                                            ReadOnlySpan injectedEvent,
                                            VdomContext.asBase ctx,
                                            currentState
                                        )

                                    currentState <- processResult.NewState

                                    // Re-render for visual feedback
                                    Render.oneStepNoFlush renderState currentState (vdom (VdomContext.asTyped ctx))

                                    VdomContext.markClean ctx

                                    // Stabilize post-layout events before continuing
                                    let stabilizedState, _hitLimit =
                                        stabilizePostLayoutEvents currentState renderState processWorld vdom

                                    Render.flush renderState
                                    currentState <- stabilizedState
                                    startState <- currentState

                                    // Successfully consumed the keystroke; move forward
                                    // Use captured index to avoid arithmetic on sentinel value (Int32.MaxValue)
                                    startOfBatch <- idxBeforeProcessing + 1
                                    nextToProcess <- startOfBatch

                            | None ->
                                // Resolver didn't handle it, include in batch
                                nextToProcess <- nextToProcess + 1

                        | None ->
                            // Nothing focused, include in batch
                            nextToProcess <- nextToProcess + 1

                    | _ -> nextToProcess <- nextToProcess + 1

                // And finally, process any sequences which *don't* end in a tab.
                // We check nextToProcess <> Int32.MaxValue because processBatch() sets it to Int32.MaxValue
                // as a sentinel to indicate "we've already finished processing everything via early bailout".
                // Without this check, we'd try to slice with Int32.MaxValue and get an out-of-bounds error.
                if startOfBatch < changes.Length && nextToProcess <> Int32.MaxValue then
                    processBatch ()

            else
                // Framework is not handling focus; just pass all keystrokes through.
                let processResult =
                    processWorld.ProcessWorld (
                        changes.AsSpan().Slice startOfBatch,
                        VdomContext.asBase ctx,
                        currentState
                    )

                currentState <- processResult.NewState

                match processResult.RequestRerender with
                | RerenderRequest.Continue -> startOfBatch <- changes.Length
                | RerenderRequest.Rerender truncatedAt ->
                    forceRerender <- true

                    if truncatedAt < 0 then
                        failwith "bad index from processing result: was negative"
                    elif truncatedAt >= changes.Length - startOfBatch - 1 then
                        startOfBatch <- changes.Length
                    else
                        startOfBatch <- startOfBatch + truncatedAt + 1

            if forceRerender || VdomContext.isDirty ctx || currentState <> startState then
                Render.oneStepNoFlush renderState currentState (vdom (VdomContext.asTyped ctx))
                VdomContext.markClean ctx

                let stabilizedState, _hitLimit =
                    stabilizePostLayoutEvents currentState renderState processWorld vdom

                Render.flush renderState
                currentState <- stabilizedState
                startState <- currentState

        currentState

    let pumpOnce<'state, 'appEvent, 'postLayoutEvent when 'state : equality>
        (listener : WorldFreezer<'appEvent>)
        (state : 'state)
        (haveFrameworkHandleFocus : 'state -> bool)
        (renderState : RenderState<'postLayoutEvent>)
        (processWorld : WorldProcessor<'appEvent, 'postLayoutEvent, 'state>)
        (vdom : IVdomContext<'postLayoutEvent> -> 'state -> Vdom<DesiredBounds>)
        (resolveActivation : ActivationResolver<'appEvent, 'state>)
        (isCancelled : unit -> bool)
        : 'state
        =
        let ctx = RenderState.vdomContext renderState

        let go state =
            let resizeGeneration = listener.TerminalResizeGeneration
            RenderState.refreshTerminalSize renderState
            VdomContext.pruneExpiredActivations ctx

            listener.RefreshExternal ()

            let changes = listener.Changes ()

            let state =
                match changes with
                | ValueNone -> processNoChanges state renderState processWorld vdom
                | ValueSome changes ->
                    processChanges
                        changes
                        state
                        haveFrameworkHandleFocus
                        renderState
                        processWorld
                        vdom
                        resolveActivation
                        isCancelled

            if listener.TerminalResizeGeneration <> resizeGeneration then
                // Our knowledge of the current terminal's contents could be arbitrarily corrupted:
                // we were drawing to the screen when it had an arbitrary size. Need a *complete* refresh.
                RenderState.clearScreen renderState
                renderState.PreviousVdom <- None
                VdomContext.markDirty ctx
                true, state
            else
                false, state

        let mutable state = state

        while (let goAgain, state' = go state in
               state <- state'
               goAgain) do
            ()

        state

    /// We set up a ConsoleCancelEventHandler to suppress one Ctrl+C, and we also listen to stdin,
    /// for as long as this task is running.
    /// Cancel the CancellationToken to cause the render loop to quit and to unhook all these state listeners.
    ///
    /// Returns an AppHandle with:
    /// - Ready: completes when initial setup is done and first render is complete
    /// - Finished: completes when the app exits (faults if user logic raises an exception)
    let run'<'state, 'appEvent, 'postLayoutEvent when 'state : equality>
        (terminate : CancellationToken)
        (console : IConsole)
        (getUtcNow : unit -> DateTime)
        (ctrlC : CtrlCHandler)
        (worldFreezer : unit -> WorldFreezer<'appEvent>)
        (initialState : 'state)
        (haveFrameworkHandleFocus : 'state -> bool)
        (processWorld : IWorldBridge<'appEvent> -> WorldProcessor<'appEvent, 'postLayoutEvent, 'state>)
        (incrVdom : VdomContext<'postLayoutEvent> -> 'state Node -> Vdom<DesiredBounds> Node)
        (resolveActivation : ActivationResolver<'appEvent, 'state>)
        (debugWriter : StreamWriter option)
        : AppHandle
        =
        // RunContinuationsAsynchronously so that we don't force continuation on the UI thread.
        // I want to make sure the UI thread could in principle be torn down once execution of the UI has finished.
        // Synchronous continuations would run on that thread.
        let ready = TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously

        let complete =
            TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously

        let _thread =
            fun () ->
                try
                    // Get initial terminal bounds
                    let initialBounds =
                        {
                            TopLeftX = 0
                            TopLeftY = 0
                            Width = console.WindowWidth ()
                            Height = console.WindowHeight ()
                        }

                    // Set up incremental state for reactive updates
                    let incrState = IncrementalState.make initialState initialBounds None
                    let vdomContext = VdomContext.make incrState

                    // Create the incremental Vdom Node
                    let stateNode = IncrementalState.stateNode incrState
                    let vdomNode = incrVdom vdomContext stateNode

                    // Create an observer for the Vdom so we can read it after stabilization
                    let vdomObserver = incrState.Incr.Observe vdomNode

                    // Initial stabilization
                    let initialUtcNow = getUtcNow ()
                    VdomContext.setCurrentStabilizationTime initialUtcNow vdomContext
                    IncrementalState.advanceClockAndStabilize initialUtcNow incrState

                    // Create a wrapper that bridges the incremental system to the legacy API.
                    // When state changes, we update the state Var, stabilize, and observe.
                    let vdom (ctx : IVdomContext<'postLayoutEvent>) (state : 'state) : Vdom<DesiredBounds> =
                        // Update state Var if it changed
                        let currentState = incrState.Incr.Var.Value incrState.StateVar

                        if currentState <> state then
                            IncrementalState.setState state incrState

                        // Advance clock and stabilize
                        let utcNow = getUtcNow ()
                        VdomContext.setCurrentStabilizationTime utcNow vdomContext
                        IncrementalState.advanceClockAndStabilize utcNow incrState

                        // Observe and return the vdom - Observer module provides Value function
                        Observer.value vdomObserver

                    use renderState = RenderState.make console vdomContext debugWriter

                    RenderState.enterAlternateScreen renderState
                    RenderState.registerMouseMode renderState
                    RenderState.registerBracketedPaste renderState
                    RenderState.setCursorInvisible renderState

                    let mutable cancels = 0

                    let ctrlCHandler =
                        ConsoleCancelEventHandler (fun _ args ->
                            // Double-ctrlc to exit immediately
                            if Interlocked.Increment &cancels = 1 then
                                args.Cancel <- true
                        )

                    ctrlC.Register ctrlCHandler

                    let mutable listener = None

                    let exc =
                        try
                            let listener' = worldFreezer ()

                            use _ =
                                try
                                    PosixSignalRegistration.Create (
                                        PosixSignal.SIGWINCH,
                                        fun _ -> listener'.NotifyTerminalResize ()
                                    )
                                with :? PlatformNotSupportedException ->
                                    // SIGWINCH not supported on this platform (e.g., Windows).
                                    // Recall that the `use` syntax is special-cased to not throw on null!
                                    null

                            listener <- Some listener'
                            let processWorld = processWorld listener'

                            // Initial render - now with processWorld available for post-layout events
                            let mutable currentState =
                                processNoChanges initialState renderState processWorld vdom

                            // Track the previous vdom value to detect time-based changes
                            let mutable previousVdom = Observer.value vdomObserver

                            let isCancelled () =
                                cancels > 0 || terminate.IsCancellationRequested

                            // Signal that we're ready: initial setup complete, first render done
                            ready.SetResult ()

                            while not (isCancelled ()) do
                                // Advance clock and stabilize to propagate time-based changes.
                                // This must happen BEFORE checking isDirty in pumpOnce, so that
                                // time-dependent components (like spinners) can trigger re-renders.
                                let loopUtcNow = getUtcNow ()
                                VdomContext.setCurrentStabilizationTime loopUtcNow vdomContext
                                IncrementalState.advanceClockAndStabilize loopUtcNow incrState

                                // Check if vdom changed due to time advancement
                                let currentVdom = Observer.value vdomObserver

                                if not (Object.referenceEquals previousVdom currentVdom) then
                                    VdomContext.markDirty vdomContext

                                currentState <-
                                    pumpOnce
                                        listener'
                                        currentState
                                        haveFrameworkHandleFocus
                                        renderState
                                        processWorld
                                        vdom
                                        resolveActivation
                                        isCancelled

                                // Update previousVdom to track the most recently observed vdom.
                                // This ensures we don't mark dirty on the next iteration just
                                // because pumpOnce rendered due to state changes.
                                previousVdom <- Observer.value vdomObserver

                            None
                        with e ->
                            // If we fail before signaling ready, signal failure there too
                            ready.TrySetException e |> ignore
                            Some e

                    ctrlC.Unregister ctrlCHandler

                    match listener with
                    | None -> ()
                    | Some listener ->
                        // ANALYZER: synchronous blocking call allowed: we're on a dedicated thread, so can't deadlock.
                        (listener :> IAsyncDisposable).DisposeAsync().GetAwaiter().GetResult ()

                    // Ideally the terminal emulator has a completely self-contained state in the alternate buffer,
                    // which means our LIFO ordering here is confined to the alternate buffer, correctly leaving the
                    // main buffer in whatever state it was in before we started executing.
                    // According to the LLMs, some terminals *don't* confine state to the alternate buffer, but in that
                    // case this order is still correct: we'll leave the cursor visible when such a terminal leaks cursor
                    // visibility out into the main buffer.
                    RenderState.setCursorVisible renderState
                    RenderState.unregisterBracketedPaste renderState
                    RenderState.unregisterMouseMode renderState
                    RenderState.exitAlternateScreen renderState
                    // Flush any buffered output to ensure cleanup operations are written to the terminal
                    RenderState.flush renderState

                    match exc with
                    | None -> complete.SetResult ()
                    | Some exc ->
                        // report critical exceptions to the user *after* disabling the alternate buffer, so they can
                        // actually see them
                        complete.SetException exc
                with e ->
                    // Ensure lifecycle tasks complete even if setup or cleanup fails
                    ready.TrySetException e |> ignore
                    complete.TrySetException e |> ignore
            |> Thread
            |> _.Start()

        {
            Ready = ready.Task
            Finished = complete.Task
        }

    /// Lift a pure view function into an incremental one.
    /// The resulting view depends only on the state Node - any state change triggers full recomputation.
    /// For fine-grained incrementality, write an incremental view function directly.
    let pureView<'state, 'postLayoutEvent>
        (view : IVdomContext<'postLayoutEvent> -> 'state -> Vdom<DesiredBounds>)
        : VdomContext<'postLayoutEvent> -> 'state Node -> Vdom<DesiredBounds> Node
        =
        fun ctx stateNode ->
            let incr = VdomContext.incr ctx
            // We also depend on bounds and focus so the vdom updates when they change
            let boundsNode = VdomContext.boundsNode ctx
            let focusNode = VdomContext.focusedKeyNode ctx

            incr.Map
                (fun ((state, _bounds), _focus) -> view (VdomContext.asTyped ctx) state)
                (incr.Both (incr.Both stateNode boundsNode) focusNode)

    let run<'state, 'appEvent, 'postLayoutEvent when 'state : equality>
        (getEnv : string -> string option)
        (state : 'state)
        (haveFrameworkHandleFocus : 'state -> bool)
        (processWorld : IWorldBridge<'appEvent> -> WorldProcessor<'appEvent, 'postLayoutEvent, 'state>)
        (incrVdom : VdomContext<'postLayoutEvent> -> 'state Node -> Vdom<DesiredBounds> Node)
        (resolveActivation : ActivationResolver<'appEvent, 'state>)
        : AppHandle
        =
        // Check if debug logging is enabled
        let debugWriter =
            match getEnv "WOOFWARE_ZOOMIES_DEBUG_TO_FILE" with
            | Some value when
                value.Equals ("true", StringComparison.OrdinalIgnoreCase)
                || value.Equals ("1", StringComparison.OrdinalIgnoreCase)
                ->
                // Create temp file with unpredictable name to prevent symlink attacks
                let tempPath = Path.GetTempPath ()
                let fileName = $"zoomies-layout-%O{Guid.NewGuid ()}.txt"
                let fullPath = Path.Combine (tempPath, fileName)

                // Create the file exclusively (will fail if it somehow already exists)
                let stream =
                    new FileStream (fullPath, FileMode.CreateNew, FileAccess.Write, FileShare.Read)

                let writer = new StreamWriter (stream, AutoFlush = true)

                Console.Error.WriteLine $"WoofWare.Zoomies: Debug layout logging enabled. Writing to: %s{fullPath}"
                Some writer
            | _ -> None

        run'
            CancellationToken.None
            (IConsole.make getEnv)
            (fun () -> DateTime.UtcNow)
            (CtrlCHandler.make ())
            WorldFreezer.listen
            state
            haveFrameworkHandleFocus
            processWorld
            incrVdom
            resolveActivation
            debugWriter
