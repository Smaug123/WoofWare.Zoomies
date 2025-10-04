namespace WoofWare.Zoomies

open System
open System.Threading
open System.Threading.Tasks

type WorldProcessor<'appEvent, 'userState> =
    abstract ProcessWorld :
        events : ReadOnlySpan<WorldStateChange<'appEvent>> * previousRenderState : VdomContext * 'userState ->
            'userState

[<RequireQualifiedAccess>]
module App =

    let pumpOnce
        (listener : WorldFreezer<'appEvent>)
        (state : 'state)
        (haveFrameworkHandleFocus : 'state -> bool)
        (renderState : RenderState)
        (processWorld : WorldProcessor<'appEvent, 'state>)
        (vdom : VdomContext -> 'state -> Vdom<DesiredBounds, Unkeyed>)
        : 'state
        =
        let startState = state

        listener.RefreshExternal ()

        let changes = listener.Changes ()

        let newState =
            match changes with
            | ValueNone -> state
            | ValueSome changes ->
                if haveFrameworkHandleFocus state then

                    let mutable currentState = state
                    let mutable i = 0
                    let mutable start = 0

                    while i < changes.Length do
                        match Array.get changes i with
                        | WorldStateChange.Keystroke t when t.Key = ConsoleKey.Tab && t.Modifiers = enum 0 ->
                            if i > 0 then
                                currentState <-
                                    processWorld.ProcessWorld (
                                        changes.AsSpan().Slice (start, i - 1 - start),
                                        renderState.VdomContext,
                                        currentState
                                    )

                            // Advance focus to the next focusable element
                            RenderState.advanceFocus renderState

                            start <- i + 1
                            // skip the tab input
                            i <- i + 1
                        | WorldStateChange.Keystroke t when
                            t.Key = ConsoleKey.Tab && t.Modifiers.HasFlag ConsoleModifiers.Shift
                            ->
                            if i > 0 then
                                currentState <-
                                    processWorld.ProcessWorld (
                                        changes.AsSpan().Slice (start, i - 1 - start),
                                        renderState.VdomContext,
                                        currentState
                                    )

                            // Retreat focus to the previous focusable element
                            RenderState.retreatFocus renderState

                            start <- i + 1
                            // skip the shift+tab input
                            i <- i + 1
                        | _ -> ()

                        i <- i + 1

                    if start < changes.Length then
                        processWorld.ProcessWorld (changes.AsSpan().Slice start, renderState.VdomContext, currentState)
                    else
                        currentState
                else
                    processWorld.ProcessWorld (changes.AsSpan (), renderState.VdomContext, state)

        if renderState.VdomContext.IsDirty || newState <> startState then
            Render.oneStep renderState newState (vdom renderState.VdomContext)
            VdomContext.markClean renderState.VdomContext

        newState


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
