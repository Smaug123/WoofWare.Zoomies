namespace WoofWare.Zoomies

open System
open System.Threading
open System.Threading.Tasks

type WorldProcessor<'appEvent, 'userState> =
    abstract ProcessWorld :
        events : ReadOnlySpan<WorldStateChange<'appEvent>> * prevVdom : Vdom<Rectangle> * 'userState -> unit

[<RequireQualifiedAccess>]
module App =

    let pumpOnce
        (listener : WorldFreezer<'appEvent>)
        (mutableState : 'state)
        (haveFrameworkHandleFocus : 'state -> bool)
        (renderState : RenderState)
        (processWorld : WorldProcessor<'appEvent, 'state>)
        (vdom : RenderState -> 'state -> Vdom<DesiredBounds>)
        : unit
        =
        listener.RefreshExternal ()

        let changes = listener.Changes ()

        match changes with
        | ValueNone -> ()
        | ValueSome changes ->

            match renderState.PreviousVdom with
            | None -> failwith "expected not to receive input before the first render"
            | Some node ->

            let prevVdom = node.Self

            if haveFrameworkHandleFocus mutableState then

                let mutable i = 0
                let mutable start = 0

                while i < changes.Length do
                    match changes.[i] with
                    | WorldStateChange.Keystroke t when t.Key = ConsoleKey.Tab && t.Modifiers = enum 0 ->
                        if i > 0 then
                            processWorld.ProcessWorld (
                                changes.AsSpan().Slice (start, i - 1 - start),
                                prevVdom,
                                mutableState
                            )

                        // Advance focus to the next focusable element
                        RenderState.advanceFocus renderState

                        start <- i + 1
                        // skip the tab input
                        i <- i + 1
                    // TODO: handle shift+tab too
                    | _ -> ()

                    i <- i + 1

                if start < changes.Length then
                    processWorld.ProcessWorld (changes.AsSpan().Slice start, prevVdom, mutableState)
            else
                processWorld.ProcessWorld (changes.AsSpan (), prevVdom, mutableState)

        Render.oneStep renderState mutableState (vdom renderState)


    /// We set up a ConsoleCancelEventHandler to suppress one Ctrl+C, and we also listen to stdin,
    /// for as long as this task is running.
    /// Cancel the CancellationToken to cause the render loop to quit and to unhook all these state listeners.
    ///
    /// The resulting Task faults if any user logic raises an exception.
    let run'<'state, 'appEvent>
        (terminate : CancellationToken)
        (console : IConsole)
        (ctrlC : CtrlCHandler)
        (worldFreezer : unit -> WorldFreezer<'appEvent>)
        (mutableState : 'state)
        (haveFrameworkHandleFocus : 'state -> bool)
        (processWorld : ((CancellationToken -> Task<'appEvent>) -> unit) -> WorldProcessor<'appEvent, 'state>)
        (vdom : RenderState -> 'state -> Vdom<DesiredBounds>)
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

                let cleanUp () =
                    ctrlC.Unregister ctrlCHandler

                    // We're on a dedicated thread, so this can't deadlock.
                    (listener :> IAsyncDisposable).DisposeAsync().GetAwaiter().GetResult ()

                    RenderState.unregisterBracketedPaste renderState
                    RenderState.unregisterMouseMode renderState
                    RenderState.exitAlternateScreen renderState
                    RenderState.setCursorVisible renderState

                let exc =
                    try
                        RenderState.setCursorInvisible renderState

                        while cancels = 0 && not terminate.IsCancellationRequested do
                            pumpOnce
                                listener
                                mutableState
                                haveFrameworkHandleFocus
                                renderState
                                (processWorld listener.PostAppEvent)
                                vdom

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

    let run<'state, 'appEvent>
        (state : 'state)
        (haveFrameworkHandleFocus : 'state -> bool)
        (processWorld : ((CancellationToken -> Task<'appEvent>) -> unit) -> WorldProcessor<'appEvent, 'state>)
        (vdom : RenderState -> 'state -> Vdom<DesiredBounds>)
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
