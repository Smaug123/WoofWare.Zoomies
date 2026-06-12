namespace WoofWare.Zoomies

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Threading.Tasks
open WoofWare.Incremental
open WoofWare.TimingWheel

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
    ///
    /// Rendering can change focus (initial-focus assignment, clearing focus that points at a
    /// node which no longer exists), in which case we re-stabilize and render once more. The
    /// second render must not change focus again: it renders a vdom that already reflects the
    /// updated focus, and focus assignment is deterministic in the rendered tree. We enforce
    /// that invariant loudly rather than silently rendering a frame inconsistent with the
    /// graph: a violation is a framework bug.
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

            let focusedFinal = VdomContext.focusedKey ctx

            if focusedAfter <> focusedFinal then
                let show (key : NodeKey option) =
                    match key with
                    | None -> "None"
                    | Some key -> NodeKey.toHumanReadableString key

                failwith
                    $"Framework bug: rendering failed to reach a focus fixpoint. Focus was %s{show focusedBefore} before rendering, %s{show focusedAfter} after the first render, and %s{show focusedFinal} after the second."

    /// Process post-layout events. Returns true if the iteration limit was hit.
    let private stabilizePostLayoutEventsWithConfig<'state, 'appEvent, 'postLayoutEvent when 'state : equality>
        (stateVar : 'state Var)
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
                let stateBeforeBatch = incrState.Incr.Var.Value stateVar

                for ev in layoutEvents do
                    let currentState = incrState.Incr.Var.Value stateVar
                    let newState = config.HandlePostLayout ev currentState

                    if currentState <> newState then
                        incrState.Incr.Var.Set stateVar newState

                incrState.Incr.Stabilize ()

                let stateAfterBatch = incrState.Incr.Var.Value stateVar

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
        (stateVar : 'state Var)
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

        // Fold events locally, then set the var once at the end (avoids per-event stabilization).
        let initialState = incrState.Incr.Var.Value stateVar
        let mutable localState = initialState

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
                    // Apply focus movement immediately so subsequent keystrokes in
                    // the same batch see the updated focus.
                    if t.Modifiers = ConsoleModifiers.None then
                        RenderState.advanceFocus renderState
                    else
                        RenderState.retreatFocus renderState

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
            incrState.Incr.Var.Set stateVar localState

        incrState.Incr.Stabilize ()

        let currentVdom = Observer.value vdomObserver
        let ctx = RenderState.vdomContext renderState

        if
            not (Object.referenceEquals previousVdom currentVdom)
            || renderState.NeedsFullRedraw
            || VdomContext.hasPendingPostLayoutEvents ctx
        then
            renderWithFocusStabilization renderState vdomObserver incrState

            // If the iteration limit was hit, undrained events remain pending and the
            // render loop will keep rendering rather than sleep.
            stabilizePostLayoutEventsWithConfig stateVar renderState config vdomObserver incrState
            |> ignore<bool>

            renderState.NeedsFullRedraw <- false
            Render.flush renderState

    /// Process when no changes occurred: render if the vdom changed.
    let private processNoChangesWithConfig<'state, 'appEvent, 'postLayoutEvent when 'state : equality>
        (previousVdom : Vdom<DesiredBounds>)
        (stateVar : 'state Var)
        (renderState : RenderState<'postLayoutEvent>)
        (config : AppConfig<'state, 'appEvent, 'postLayoutEvent>)
        (vdomObserver : Vdom<DesiredBounds> Observer)
        (incrState : IncrementalState)
        : unit
        =
        let currentVdom = Observer.value vdomObserver
        let ctx = RenderState.vdomContext renderState

        if
            not (Object.referenceEquals previousVdom currentVdom)
            || renderState.NeedsFullRedraw
            || VdomContext.hasPendingPostLayoutEvents ctx
        then
            renderWithFocusStabilization renderState vdomObserver incrState

            stabilizePostLayoutEventsWithConfig stateVar renderState config vdomObserver incrState
            |> ignore<bool>

            renderState.NeedsFullRedraw <- false
            Render.flush renderState

    /// Run one iteration of the incremental event loop.
    let internal pumpOnce<'state, 'appEvent, 'postLayoutEvent when 'state : equality>
        (getUtcNow : unit -> DateTime)
        (listener : WorldFreezer<'appEvent>)
        (incrState : IncrementalState)
        (stateVar : 'state Var)
        (renderState : RenderState<'postLayoutEvent>)
        (vdomObserver : Vdom<DesiredBounds> Observer)
        (config : AppConfig<'state, 'appEvent, 'postLayoutEvent>)
        (previousVdom : Vdom<DesiredBounds> ref)
        (isCancelled : unit -> bool)
        : 'state
        =
        let vdomContext = RenderState.vdomContext renderState

        let loopUtcNow = getUtcNow ()

        // Refresh the bounds before stabilizing, so a resize propagates through the graph
        // in this very pump (rather than rendering a stale vdom now and the resized one on
        // the next pump).
        let resizeGeneration = listener.TerminalResizeGeneration
        RenderState.refreshTerminalSize renderState

        IncrementalState.advanceClockAndStabilize loopUtcNow incrState

        match listener.Changes () with
        | ValueNone -> processNoChangesWithConfig previousVdom.Value stateVar renderState config vdomObserver incrState
        | ValueSome changes ->
            processChangesWithConfig loopUtcNow changes stateVar renderState config vdomObserver incrState isCancelled

        if listener.TerminalResizeGeneration <> resizeGeneration then
            // Our knowledge of the current terminal's contents could be arbitrarily corrupted:
            // we were drawing to the screen when it had an arbitrary size. Need a *complete*
            // refresh. The resize notification that bumped the generation also completed the
            // armed wake signal, so the next loop iteration runs immediately and repaints.
            RenderState.clearScreen renderState
            renderState.PreviousVdom <- None
            renderState.NeedsFullRedraw <- true

        previousVdom.Value <- Observer.value vdomObserver

        incrState.Incr.Var.Value stateVar

    /// Run an application using AppConfig, with an injectable wait primitive (for tests).
    /// `waitForWork wake deadline ct` must block until `wake` completes, `deadline` (a
    /// duration from now) elapses, or `ct` fires.
    let internal runWith<'state, 'appEvent, 'postLayoutEvent when 'state : equality>
        (waitForWork : Task -> TimeSpan voption -> CancellationToken -> unit)
        (terminate : CancellationToken)
        (console : IConsole)
        (getUtcNow : unit -> DateTime)
        (ctrlC : CtrlCHandler)
        (worldFreezer : unit -> WorldFreezer<'appEvent>)
        (config : AppConfig<'state, 'appEvent, 'postLayoutEvent>)
        (debugWriter : StreamWriter option)
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

                    let stateVar = incrState.Incr.Var.Create config.Initial

                    let vdomNode = config.View vdomContext (incrState.Incr.Var.Watch stateVar)
                    let vdomObserver = incrState.Incr.Observe vdomNode

                    let initialUtcNow = getUtcNow ()
                    IncrementalState.advanceClockAndStabilize initialUtcNow incrState

                    use renderState = RenderState.make console vdomContext debugWriter

                    RenderState.enterAlternateScreen renderState
                    RenderState.registerMouseMode renderState
                    RenderState.registerBracketedPaste renderState
                    RenderState.setCursorInvisible renderState

                    let mutable cancels = 0

                    // The loop sleeps in waitForWork; Ctrl-C must wake it, so the handler
                    // cancels this token (linked to the caller's `terminate`).
                    use loopCts = CancellationTokenSource.CreateLinkedTokenSource terminate

                    let ctrlCHandler =
                        ConsoleCancelEventHandler (fun _ args ->
                            if Interlocked.Increment &cancels = 1 then
                                args.Cancel <- true

                            loopCts.Cancel ()
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

                            // If the iteration limit was hit, undrained events remain pending
                            // and the loop will keep rendering rather than sleep.
                            stabilizePostLayoutEventsWithConfig stateVar renderState config vdomObserver incrState
                            |> ignore<bool>

                            Render.flush renderState

                            let previousVdom = ref (Observer.value vdomObserver)

                            let isCancelled () =
                                cancels > 0 || terminate.IsCancellationRequested

                            // How long may the loop sleep? The earlier of the clock's next
                            // alarm (animations, activation expiry) and the freezer's next
                            // internal timeout (Esc disambiguation, paste timeout).
                            // Must be computed after the iteration's final stabilization,
                            // so that every alarm created during it is visible.
                            let nextDeadline () : TimeSpan voption =
                                let clockDelay =
                                    match incrState.Incr.Clock.NextAlarmFiresAt incrState.Clock with
                                    | ValueNone -> ValueNone
                                    | ValueSome alarmAt ->
                                        let nowNs = TimeConversion.dateTimeToNs (getUtcNow ())

                                        let deltaNs =
                                            TimeNs.toInt64NsSinceEpoch alarmAt - TimeNs.toInt64NsSinceEpoch nowNs

                                        // Round up to the next tick so a sub-tick-future alarm
                                        // cannot produce a zero-length (busy) wait.
                                        ValueSome (TimeSpan.FromTicks (max 0L ((deltaNs + 99L) / 100L)))

                                let inputDelay =
                                    match listener'.NextDeadline () with
                                    | ValueNone -> ValueNone
                                    | ValueSome deadlineTicks ->
                                        let sw = listener'.Stopwatch

                                        let deltaSeconds =
                                            float (deadlineTicks - sw.GetTimestamp ()) / float sw.Frequency

                                        ValueSome (TimeSpan.FromSeconds (max 0.0 deltaSeconds))

                                match clockDelay, inputDelay with
                                | ValueNone, d
                                | d, ValueNone -> d
                                | ValueSome clock, ValueSome input -> ValueSome (min clock input)

                            ready.SetResult ()

                            while not (isCancelled ()) do
                                // Arm the wake signal before pumping: anything that enters the
                                // world after this point either is seen by the pump's drain or
                                // completes the signal, so it cannot be slept through.
                                let wake = listener'.WaitForChange ()

                                pumpOnce
                                    getUtcNow
                                    listener'
                                    incrState
                                    stateVar
                                    renderState
                                    vdomObserver
                                    config
                                    previousVdom
                                    isCancelled
                                |> ignore

                                // Undrained post-layout events (the iteration limit tripped)
                                // mean there is definitely more work: don't sleep.
                                if not (isCancelled ()) && not (VdomContext.hasPendingPostLayoutEvents vdomContext) then
                                    waitForWork wake (nextDeadline ()) loopCts.Token

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

    /// Run an application using AppConfig. The render loop is event-driven: it sleeps until
    /// input arrives, an application event is posted, the terminal is resized, or the next
    /// scheduled clock alarm (animation frame, activation expiry) is due. An idle app does
    /// no work.
    let run<'state, 'appEvent, 'postLayoutEvent when 'state : equality>
        (terminate : CancellationToken)
        (console : IConsole)
        (getUtcNow : unit -> DateTime)
        (ctrlC : CtrlCHandler)
        (worldFreezer : unit -> WorldFreezer<'appEvent>)
        (config : AppConfig<'state, 'appEvent, 'postLayoutEvent>)
        (debugWriter : StreamWriter option)
        : AppHandle
        =
        runWith EventLoop.waitForWork terminate console getUtcNow ctrlC worldFreezer config debugWriter
