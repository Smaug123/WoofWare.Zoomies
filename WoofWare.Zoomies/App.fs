namespace WoofWare.Zoomies

open System
open System.Threading
open System.Threading.Tasks

type RerenderRequest =
    /// Don't request a rerender; just let me continue processing the current batch of events.
    | Continue
    /// Request a rerender: effectively truncate the current batch of events and split the rest of the batch into a
    /// new iteration of the render loop.
    /// The index is the last element you processed.
    /// For example, to say you've processed only one element of the batch, you'd return 0 here.
    | NewBatch of truncationIndex : int
    /// Like NewBatch ("truncate the current batch of requests"), but additionally requests that your vdom be
    /// unconditionally reevaluated too. This may be useful to work around bugs in the cutoff logic, if you find any.
    /// The index is the last element you processed.
    /// For example, to say you've processed only one element of the batch, you'd return 0 here.
    | Rerender of truncationIndex : int

type ProcessWorldResult<'userState> =
    {
        NewState : 'userState
        /// Set this to `true` to request a rerender *now*, rather than continuing to process the rest of the batch
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

type WorldProcessor<'appEvent, 'userState> =
    abstract ProcessWorld :
        events : ReadOnlySpan<WorldStateChange<'appEvent>> * previousRenderState : VdomContext * 'userState ->
            ProcessWorldResult<'userState>

[<RequireQualifiedAccess>]
module App =

    let pumpOnce<'state, 'appEvent when 'state : equality>
        (listener : WorldFreezer<'appEvent>)
        (state : 'state)
        (haveFrameworkHandleFocus : 'state -> bool)
        (renderState : RenderState)
        (processWorld : WorldProcessor<'appEvent, 'state>)
        (vdom : VdomContext -> 'state -> Vdom<DesiredBounds, Unkeyed>)
        : 'state
        =
        listener.RefreshExternal ()

        let changes = listener.Changes ()

        match changes with
        | ValueNone ->
            if renderState.VdomContext.IsDirty then
                Render.oneStep renderState state (vdom renderState.VdomContext)
                VdomContext.markClean renderState.VdomContext

            state
        | ValueSome changes ->

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
                    | RerenderRequest.NewBatch lastProcessed ->
                        if lastProcessed >= nextToProcess - 1 then
                            // Successfully processed everything up to but not including the tab.
                            // Just proceed.
                            startOfBatch <- nextToProcess
                            nextToProcess <- Int32.MaxValue
                        elif lastProcessed < 0 then
                            failwith "bad index from processing result: was negative"
                        else
                            startOfBatch <- startOfBatch + lastProcessed + 1
                            nextToProcess <- Int32.MaxValue
                    | RerenderRequest.Rerender lastProcessed ->
                        forceRerender <- true

                        if lastProcessed >= nextToProcess - 1 then
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
                | RerenderRequest.NewBatch truncatedAt ->
                    if truncatedAt < 0 then
                        failwith "bad index from processing result: was negative"
                    elif truncatedAt >= changes.Length - startOfBatch - 1 then
                        startOfBatch <- changes.Length
                    else
                        startOfBatch <- startOfBatch + truncatedAt + 1
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

    /// We set up a ConsoleCancelEventHandler to suppress one Ctrl+C, and we also listen to stdin,
    /// for as long as this task is running.
    /// Cancel the CancellationToken to cause the render loop to quit and to unhook all these state listeners.
    ///
    /// The resulting Task faults if any user logic raises an exception.
    let run'<'state, 'appEvent when 'state : equality>
        (terminate : CancellationToken)
        (console : IConsole)
        (ctrlC : CtrlCHandler)
        (worldFreezer : unit -> WorldFreezer<'appEvent>)
        (initialState : 'state)
        (haveFrameworkHandleFocus : 'state -> bool)
        (processWorld : IWorldBridge<'appEvent> -> WorldProcessor<'appEvent, 'state>)
        (vdom : VdomContext -> 'state -> Vdom<DesiredBounds, Unkeyed>)
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
                let renderState = RenderState.make' console

                RenderState.enterAlternateScreen renderState
                RenderState.registerMouseMode renderState
                RenderState.registerBracketedPaste renderState

                let mutable cancels = 0

                let ctrlCHandler =
                    ConsoleCancelEventHandler (fun _ args ->
                        // Double-ctrlc to exit immediately
                        if Interlocked.Increment &cancels = 1 then
                            args.Cancel <- true
                    )

                ctrlC.Register ctrlCHandler

                let listener = worldFreezer ()
                let processWorld = processWorld listener

                let cleanUp () =
                    ctrlC.Unregister ctrlCHandler

                    // ANALYZER: synchronous blocking call allowed: we're on a dedicated thread, so can't deadlock.
                    (listener :> IAsyncDisposable).DisposeAsync().GetAwaiter().GetResult ()

                    RenderState.unregisterBracketedPaste renderState
                    RenderState.unregisterMouseMode renderState
                    RenderState.exitAlternateScreen renderState
                    RenderState.setCursorVisible renderState

                let exc =
                    try
                        RenderState.setCursorInvisible renderState

                        let mutable currentState = initialState

                        while cancels = 0 && not terminate.IsCancellationRequested do
                            currentState <-
                                pumpOnce listener currentState haveFrameworkHandleFocus renderState processWorld vdom

                        None
                    with e ->
                        Some e

                cleanUp ()

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
        (state : 'state)
        (haveFrameworkHandleFocus : 'state -> bool)
        (processWorld : IWorldBridge<'appEvent> -> WorldProcessor<'appEvent, 'state>)
        (vdom : VdomContext -> 'state -> Vdom<DesiredBounds, Unkeyed>)
        : Task
        =
        run'
            CancellationToken.None
            (IConsole.make ())
            (CtrlCHandler.make ())
            WorldFreezer.listen
            state
            haveFrameworkHandleFocus
            processWorld
            vdom
