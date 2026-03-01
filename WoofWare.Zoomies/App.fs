namespace WoofWare.Zoomies

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Threading.Tasks
open WoofWare.Incremental

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

    /// Lift a pure view function into an incremental one.
    /// Depends on state, bounds, and focus; any change triggers full recomputation.
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

    /// Like pureView but the view function returns an incremental Vdom node.
    let pureViewIncr<'state, 'postLayoutEvent>
        (view : IVdomContext<'postLayoutEvent> -> 'state -> Vdom<DesiredBounds> Node)
        : VdomContext<'postLayoutEvent> -> 'state Node -> Vdom<DesiredBounds> Node
        =
        fun ctx stateNode ->
            let incr = VdomContext.incr ctx
            let boundsNode = VdomContext.boundsNode ctx
            let focusNode = VdomContext.focusedKeyNode ctx

            incr.Bind
                (fun ((state, _bounds), _focus) -> view (VdomContext.asTyped ctx) state)
                (incr.Both (incr.Both stateNode boundsNode) focusNode)

    /// Render, re-stabilizing if focus changed during render.
    let private renderWithFocusStabilization<'postLayoutEvent>
        (renderState : RenderState<'postLayoutEvent>)
        (vdomObserver : Vdom<DesiredBounds> Observer)
        (incrState : IncrementalState)
        : unit
        =
        let ctx = RenderState.vdomContext renderState
        let focusedBefore = VdomContext.focusedKey ctx

        Render.oneStepNoFlush renderState () (fun () -> Observer.value vdomObserver)

        let focusedAfter = VdomContext.focusedKey ctx

        if focusedBefore <> focusedAfter then
            incrState.Incr.Stabilize ()
            Render.oneStepNoFlush renderState () (fun () -> Observer.value vdomObserver)

    /// Process post-layout events. Returns true if the iteration limit was hit.
    let private stabilizePostLayoutEventsWithConfig<'state, 'appEvent, 'postLayoutEvent when 'state : equality>
        (stateMachine : StateMachine<'state, 'appEvent>)
        (renderState : RenderState<'postLayoutEvent>)
        (config : AppConfig<'state, 'appEvent, 'postLayoutEvent>)
        (vdomObserver : Vdom<DesiredBounds> Observer)
        (incrState : IncrementalState)
        : bool
        =
        let ctx = RenderState.vdomContext renderState
        let mutable iterations = 0
        let mutable continueLoop = true

        while continueLoop && iterations < MAX_POST_LAYOUT_ITERATIONS do
            let layoutEvents = VdomContext.drainPostLayoutEvents ctx

            if layoutEvents.Length = 0 then
                continueLoop <- false
            else
                let stateBeforeBatch = stateMachine.CurrentState ()

                for ev in layoutEvents do
                    let currentState = stateMachine.CurrentState ()
                    let newState = config.HandlePostLayout ev currentState

                    if currentState <> newState then
                        stateMachine.SetState newState

                incrState.Incr.Stabilize ()

                let stateAfterBatch = stateMachine.CurrentState ()

                if stateBeforeBatch <> stateAfterBatch then
                    renderWithFocusStabilization renderState vdomObserver incrState
                    iterations <- iterations + 1
                else
                    continueLoop <- false

        iterations >= MAX_POST_LAYOUT_ITERATIONS

    /// Process a batch of input changes.
    let private processChangesWithConfig<'state, 'appEvent, 'postLayoutEvent when 'state : equality>
        (now : DateTime)
        (changes : WorldStateChange<'appEvent>[])
        (stateMachine : StateMachine<'state, 'appEvent>)
        (renderState : RenderState<'postLayoutEvent>)
        (config : AppConfig<'state, 'appEvent, 'postLayoutEvent>)
        (vdomObserver : Vdom<DesiredBounds> Observer)
        (incrState : IncrementalState)
        (isCancelled : unit -> bool)
        : unit
        =
        let ctx = RenderState.vdomContext renderState

        let haveFrameworkHandleFocus =
            match config.FocusHandling with
            | FocusHandling.FrameworkManaged -> true
            | FocusHandling.UserManaged -> false

        let previousVdom = Observer.value vdomObserver

        // Fold events locally, then SetState once at the end (avoids per-event stabilization).
        let initialState = stateMachine.CurrentState ()
        let mutable localState = initialState

        // Defer Tab handling: positive = advance, negative = retreat.
        let mutable pendingFocusMovement = 0

        for change in changes do
            if isCancelled () then
                ()
            else
                match change with
                | WorldStateChange.Keystroke t when
                    haveFrameworkHandleFocus
                    && t.Key = ConsoleKey.Tab
                    && (t.Modifiers = ConsoleModifiers.None || t.Modifiers = ConsoleModifiers.Shift)
                    ->
                    if t.Modifiers = ConsoleModifiers.None then
                        pendingFocusMovement <- pendingFocusMovement + 1
                    else
                        pendingFocusMovement <- pendingFocusMovement - 1

                | WorldStateChange.Keystroke k ->
                    match VdomContext.focusedKey ctx with
                    | Some focusedKey ->
                        match config.ActivationResolver.Invoke (focusedKey, k, localState) with
                        | Some appEvent ->
                            VdomContext.recordActivation now focusedKey ctx
                            localState <- config.Transition localState appEvent
                        | None ->
                            match config.HandleInput change with
                            | Some appEvent -> localState <- config.Transition localState appEvent
                            | None -> ()
                    | None ->
                        match config.HandleInput change with
                        | Some appEvent -> localState <- config.Transition localState appEvent
                        | None -> ()

                | _ ->
                    match config.HandleInput change with
                    | Some appEvent -> localState <- config.Transition localState appEvent
                    | None -> ()

        if initialState <> localState then
            stateMachine.SetState localState

        incrState.Incr.Stabilize ()

        let currentVdom = Observer.value vdomObserver
        let ctx = RenderState.vdomContext renderState

        if not (Object.referenceEquals previousVdom currentVdom) || VdomContext.isDirty ctx then
            renderWithFocusStabilization renderState vdomObserver incrState

            let _hitLimit =
                stabilizePostLayoutEventsWithConfig stateMachine renderState config vdomObserver incrState

            // Reset dirty flag after post-layout processing
            VdomContext.markClean ctx
            Render.flush renderState

        if pendingFocusMovement <> 0 then
            if pendingFocusMovement > 0 then
                for _ in 1..pendingFocusMovement do
                    RenderState.advanceFocus renderState
            else
                for _ in 1 .. -pendingFocusMovement do
                    RenderState.retreatFocus renderState

            incrState.Incr.Stabilize ()
            renderWithFocusStabilization renderState vdomObserver incrState

            let _hitLimit =
                stabilizePostLayoutEventsWithConfig stateMachine renderState config vdomObserver incrState

            // Reset dirty flag after post-layout processing
            VdomContext.markClean ctx
            Render.flush renderState

    /// Process when no changes occurred: render if the vdom changed.
    let private processNoChangesWithConfig<'state, 'appEvent, 'postLayoutEvent when 'state : equality>
        (previousVdom : Vdom<DesiredBounds>)
        (stateMachine : StateMachine<'state, 'appEvent>)
        (renderState : RenderState<'postLayoutEvent>)
        (config : AppConfig<'state, 'appEvent, 'postLayoutEvent>)
        (vdomObserver : Vdom<DesiredBounds> Observer)
        (incrState : IncrementalState)
        : unit
        =
        let currentVdom = Observer.value vdomObserver
        let ctx = RenderState.vdomContext renderState

        if not (Object.referenceEquals previousVdom currentVdom) || VdomContext.isDirty ctx then
            renderWithFocusStabilization renderState vdomObserver incrState

            let _hitLimit =
                stabilizePostLayoutEventsWithConfig stateMachine renderState config vdomObserver incrState

            VdomContext.markClean ctx
            Render.flush renderState

    /// Run one iteration of the incremental event loop.
    let internal pumpOnce<'state, 'appEvent, 'postLayoutEvent when 'state : equality>
        (getUtcNow : unit -> DateTime)
        (listener : WorldFreezer<'appEvent>)
        (incrState : IncrementalState)
        (stateMachine : StateMachine<'state, 'appEvent>)
        (renderState : RenderState<'postLayoutEvent>)
        (vdomObserver : Vdom<DesiredBounds> Observer)
        (config : AppConfig<'state, 'appEvent, 'postLayoutEvent>)
        (previousVdom : Vdom<DesiredBounds> ref)
        (isCancelled : unit -> bool)
        : 'state
        =
        let vdomContext = RenderState.vdomContext renderState

        let loopUtcNow = getUtcNow ()
        IncrementalState.advanceClockAndStabilize loopUtcNow incrState

        let resizeGeneration = listener.TerminalResizeGeneration
        RenderState.refreshTerminalSize renderState
        VdomContext.pruneExpiredActivations loopUtcNow vdomContext

        listener.RefreshExternal ()

        match listener.Changes () with
        | ValueNone ->
            processNoChangesWithConfig previousVdom.Value stateMachine renderState config vdomObserver incrState
        | ValueSome changes ->
            processChangesWithConfig
                loopUtcNow
                changes
                stateMachine
                renderState
                config
                vdomObserver
                incrState
                isCancelled

        if listener.TerminalResizeGeneration <> resizeGeneration then
            // Our knowledge of the current terminal's contents could be arbitrarily corrupted:
            // we were drawing to the screen when it had an arbitrary size. Need a *complete* refresh.
            RenderState.clearScreen renderState
            renderState.PreviousVdom <- None
            VdomContext.markDirty vdomContext

        previousVdom.Value <- Observer.value vdomObserver

        stateMachine.CurrentState ()

    /// Run an application using AppConfig.
    let run<'state, 'appEvent, 'postLayoutEvent when 'state : equality>
        (terminate : CancellationToken)
        (console : IConsole)
        (getUtcNow : unit -> DateTime)
        (ctrlC : CtrlCHandler)
        (worldFreezer : unit -> WorldFreezer<'appEvent>)
        (config : AppConfig<'state, 'appEvent, 'postLayoutEvent>)
        (debugWriter : StreamWriter option)
        (frameDelayMs : int)
        : AppHandle
        =
        let ready = TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously

        let complete =
            TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously

        let _thread =
            fun () ->
                try
                    let initialBounds =
                        {
                            TopLeftX = 0
                            TopLeftY = 0
                            Width = console.WindowWidth ()
                            Height = console.WindowHeight ()
                        }

                    let incrState = IncrementalState.make initialBounds None
                    let vdomContext = VdomContext.make incrState

                    let stateMachine =
                        StateMachine.create incrState.Incr.State config.Initial config.Transition

                    let vdomNode = config.View vdomContext stateMachine.StateNode
                    let vdomObserver = incrState.Incr.Observe vdomNode

                    let initialUtcNow = getUtcNow ()
                    IncrementalState.advanceClockAndStabilize initialUtcNow incrState

                    use renderState = RenderState.make console vdomContext debugWriter

                    RenderState.enterAlternateScreen renderState
                    RenderState.registerMouseMode renderState
                    RenderState.registerBracketedPaste renderState
                    RenderState.setCursorInvisible renderState

                    let mutable cancels = 0

                    let ctrlCHandler =
                        ConsoleCancelEventHandler (fun _ args ->
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
                                    null

                            listener <- Some listener'

                            config.OnSetup listener'
                            renderWithFocusStabilization renderState vdomObserver incrState

                            let _hitLimit =
                                stabilizePostLayoutEventsWithConfig
                                    stateMachine
                                    renderState
                                    config
                                    vdomObserver
                                    incrState

                            Render.flush renderState

                            let previousVdom = ref (Observer.value vdomObserver)

                            let isCancelled () =
                                cancels > 0 || terminate.IsCancellationRequested

                            ready.SetResult ()

                            while not (isCancelled ()) do
                                pumpOnce
                                    getUtcNow
                                    listener'
                                    incrState
                                    stateMachine
                                    renderState
                                    vdomObserver
                                    config
                                    previousVdom
                                    isCancelled
                                |> ignore

                                if frameDelayMs > 0 then
                                    Thread.Sleep frameDelayMs

                            None
                        with e ->
                            ready.TrySetException e |> ignore
                            Some e

                    ctrlC.Unregister ctrlCHandler

                    match listener with
                    | None -> ()
                    | Some listener ->
                        // ANALYZER: synchronous blocking call allowed: we're on a dedicated thread, so can't deadlock.
                        (listener :> IAsyncDisposable).DisposeAsync().GetAwaiter().GetResult ()

                    RenderState.setCursorVisible renderState
                    RenderState.unregisterBracketedPaste renderState
                    RenderState.unregisterMouseMode renderState
                    RenderState.resetAttributes renderState
                    RenderState.exitAlternateScreen renderState
                    RenderState.flush renderState

                    match exc with
                    | None -> complete.SetResult ()
                    | Some exc -> complete.SetException exc
                with e ->
                    ready.TrySetException e |> ignore
                    complete.TrySetException e |> ignore
            |> Thread
            |> _.Start()

        {
            Ready = ready.Task
            Finished = complete.Task
        }
