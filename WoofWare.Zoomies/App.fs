namespace WoofWare.Zoomies

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Threading.Tasks

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

type WorldProcessor<'appEvent, 'userState> =
    abstract ProcessWorld :
        events : ReadOnlySpan<WorldStateChange<'appEvent>> * previousRenderState : VdomContext * 'userState ->
            ProcessWorldResult<'userState>

[<RequireQualifiedAccess>]
module App =

    let internal processNoChanges<'state>
        (state : 'state)
        (renderState : RenderState)
        (vdom : VdomContext -> 'state -> Vdom<DesiredBounds, Unkeyed>)
        : 'state
        =
        if renderState.VdomContext.IsDirty then
            Render.oneStep renderState state (vdom renderState.VdomContext)
            VdomContext.markClean renderState.VdomContext

        state

    let internal processChanges<'state, 'appEvent when 'state : equality>
        (changes : WorldStateChange<'appEvent>[])
        (state : 'state)
        (haveFrameworkHandleFocus : 'state -> bool)
        (renderState : RenderState)
        (processWorld : WorldProcessor<'appEvent, 'state>)
        (vdom : VdomContext -> 'state -> Vdom<DesiredBounds, Unkeyed>)
        (resolveActivation : ActivationResolver<'appEvent, 'state>)
        (listener : WorldFreezer<'appEvent>)
        : 'state
        =
        let mutable startState = state
        let mutable currentState = state
        let mutable startOfBatch = 0

        while startOfBatch < changes.Length do
            let mutable forceRerender = false

            if haveFrameworkHandleFocus currentState then
                // The point is to pass as large a batch as possible to ProcessWorld.
                // Any character that isn't a tab will go into the batch.

                let mutable nextToProcess = startOfBatch

                let processBatch () =
                    let processResult =
                        processWorld.ProcessWorld (
                            changes.AsSpan().Slice (startOfBatch, nextToProcess - startOfBatch),
                            renderState.VdomContext,
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
                        match VdomContext.focusedKey renderState.VdomContext with
                        | Some focusedKey ->
                            match resolveActivation.Invoke (focusedKey, k, currentState) with
                            | Some appEvent ->
                                // Activation! Process batch up to here, then inject event
                                if nextToProcess > startOfBatch then
                                    processBatch ()

                                // Record activation time for visual feedback
                                VdomContext.recordActivation focusedKey renderState.VdomContext

                                // Inject the resolved event
                                let injectedEvent = [| WorldStateChange.ApplicationEvent appEvent |]

                                let processResult =
                                    processWorld.ProcessWorld (
                                        ReadOnlySpan injectedEvent,
                                        renderState.VdomContext,
                                        currentState
                                    )

                                currentState <- processResult.NewState

                                // Re-render for visual feedback
                                Render.oneStep renderState currentState (vdom renderState.VdomContext)
                                VdomContext.markClean renderState.VdomContext
                                startState <- currentState

                                // Successfully consumed the keystroke; move forward
                                nextToProcess <- nextToProcess + 1
                                startOfBatch <- nextToProcess

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
                        renderState.VdomContext,
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

            if forceRerender || renderState.VdomContext.IsDirty || currentState <> startState then
                Render.oneStep renderState currentState (vdom renderState.VdomContext)
                VdomContext.markClean renderState.VdomContext
                startState <- currentState

        currentState

    let pumpOnce<'state, 'appEvent when 'state : equality>
        (listener : WorldFreezer<'appEvent>)
        (state : 'state)
        (haveFrameworkHandleFocus : 'state -> bool)
        (renderState : RenderState)
        (processWorld : WorldProcessor<'appEvent, 'state>)
        (vdom : VdomContext -> 'state -> Vdom<DesiredBounds, Unkeyed>)
        (resolveActivation : ActivationResolver<'appEvent, 'state>)
        : 'state
        =
        let go state =
            let resizeGeneration = listener.TerminalResizeGeneration
            RenderState.refreshTerminalSize renderState
            VdomContext.pruneExpiredActivations renderState.VdomContext

            // Handle internal events first
            for internalEvt in listener.DrainInternalEvents () do
                match internalEvt with
                | FrameworkEvent.ActivationVisualTimeout key -> VdomContext.clearActivation key renderState.VdomContext

            listener.RefreshExternal ()

            let changes = listener.Changes ()

            let state =
                match changes with
                | ValueNone -> processNoChanges state renderState vdom
                | ValueSome changes ->
                    processChanges
                        changes
                        state
                        haveFrameworkHandleFocus
                        renderState
                        processWorld
                        vdom
                        resolveActivation
                        listener

            if listener.TerminalResizeGeneration <> resizeGeneration then
                // Our knowledge of the current terminal's contents could be arbitrarily corrupted:
                // we were drawing to the screen when it had an arbitrary size. Need a *complete* refresh.
                RenderState.clearScreen renderState
                renderState.PreviousVdom <- None
                VdomContext.markDirty renderState.VdomContext
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
    /// The resulting Task faults if any user logic raises an exception.
    let run'<'state, 'appEvent when 'state : equality>
        (terminate : CancellationToken)
        (console : IConsole)
        (getUtcNow : unit -> DateTime)
        (ctrlC : CtrlCHandler)
        (worldFreezer : unit -> WorldFreezer<'appEvent>)
        (initialState : 'state)
        (haveFrameworkHandleFocus : 'state -> bool)
        (processWorld : IWorldBridge<'appEvent> -> WorldProcessor<'appEvent, 'state>)
        (vdom : VdomContext -> 'state -> Vdom<DesiredBounds, Unkeyed>)
        (resolveActivation : ActivationResolver<'appEvent, 'state>)
        (debugWriter : StreamWriter option)
        : Task
        =
        // RunContinuationsAsynchronously so that we don't force continuation on the UI thread.
        // I want to make sure the UI thread could in principle be torn down once execution of the UI has finished.
        // Synchronous continuations would run on that thread.
        let complete =
            TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously

        let _thread =
            fun () ->
                // TODO: react to changes in dimension
                use renderState = RenderState.make console getUtcNow debugWriter

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
                        let mutable currentState = processNoChanges initialState renderState vdom

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

                        while cancels = 0 && not terminate.IsCancellationRequested do
                            currentState <-
                                pumpOnce
                                    listener'
                                    currentState
                                    haveFrameworkHandleFocus
                                    renderState
                                    processWorld
                                    vdom
                                    resolveActivation

                        None
                    with e ->
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

                match exc with
                | None -> complete.SetResult ()
                | Some exc ->
                    // report critical exceptions to the user *after* disabling the alternate buffer, so they can
                    // actually see them
                    complete.SetException exc
            |> Thread
            |> _.Start()

        complete.Task

    let run<'state, 'appEvent when 'state : equality>
        (getEnv : string -> string option)
        (state : 'state)
        (haveFrameworkHandleFocus : 'state -> bool)
        (processWorld : IWorldBridge<'appEvent> -> WorldProcessor<'appEvent, 'state>)
        (vdom : VdomContext -> 'state -> Vdom<DesiredBounds, Unkeyed>)
        (resolveActivation : ActivationResolver<'appEvent, 'state>)
        : Task
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
                let fileName = $"zoomies-layout-{Guid.NewGuid ()}.txt"
                let fullPath = Path.Combine (tempPath, fileName)

                // Create the file exclusively (will fail if it somehow already exists)
                let stream =
                    new FileStream (fullPath, FileMode.CreateNew, FileAccess.Write, FileShare.Read)

                let writer = new StreamWriter (stream, AutoFlush = true)

                Console.Error.WriteLine $"WoofWare.Zoomies: Debug layout logging enabled. Writing to: {fullPath}"
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
            vdom
            resolveActivation
            debugWriter
