namespace WoofWare.Zoomies

open System
open System.Threading
open System.Threading.Tasks

[<RequireQualifiedAccess>]
module App =

    let pumpOnce
        (listener : WorldFreezer<'appEvent>)
        (mutableState : 'state)
        (haveFrameworkHandleFocus : 'state -> bool)
        (renderState : RenderState)
        (processWorld : WorldStateChange<'appEvent> seq -> 'state -> unit)
        (vdom : 'state -> Vdom)
        : unit
        =
        listener.RefreshExternal ()

        let changes = listener.Changes ()

        match changes with
        | ValueNone -> ()
        | ValueSome changes ->

            if haveFrameworkHandleFocus mutableState then

                let mutable i = 0
                let mutable start = 0

                while i < changes.Length do
                    match changes.[i] with
                    | WorldStateChange.Keystroke t when
                        t.Key = ConsoleKey.Tab && (t.Modifiers &&& ConsoleModifiers.Shift = enum 0)
                        ->
                        if i > 0 then
                            processWorld changes.[start .. i - 1] mutableState

                        match renderState.PreviousVdom with
                        | None -> failwith "expected not to receive input before the first render"
                        | Some (prevVdom, _) ->
                            // TODO: this is grossly inefficient!
                            let focusChange = Vdom.cata Vdom.advanceFocusCata prevVdom

                            match focusChange.FirstUnfocusedAfter with
                            | Some changeFocus -> changeFocus ()
                            | None ->
                                // Try wrapping round to the first focusable element
                                match focusChange.FirstUnfocusedAbsolute with
                                | Some changeFocus -> changeFocus ()
                                | None ->
                                    // couldn't find anything to change focus to
                                    ()

                        start <- i + 1
                        // skip the tab input
                        i <- i + 1
                    // TODO: handle shift+tab too
                    | _ -> ()

                    i <- i + 1

                if start < changes.Length then
                    processWorld changes.[start..] mutableState
            else
                processWorld changes mutableState

        Render.oneStep renderState mutableState vdom


    /// We set up a ConsoleCancelEventHandler to suppress one Ctrl+C, and we also listen to stdin,
    /// for as long as this task is running.
    /// Cancel the CancellationToken to cause the render loop to quit and to unhook all these state listeners.
    let run'<'state, 'appEvent>
        (terminate : CancellationToken)
        (console : IConsole)
        (ctrlC : CtrlCHandler)
        (worldFreezer : unit -> WorldFreezer<'appEvent>)
        (mutableState : 'state)
        (haveFrameworkHandleFocus : 'state -> bool)
        (processWorld :
            ((CancellationToken -> Task<'appEvent>) -> unit) -> WorldStateChange<'appEvent> seq -> 'state -> unit)
        (vdom : 'state -> Vdom)
        : Task
        =
        let complete = TaskCompletionSource ()

        let _thread =
            fun () ->
                // TODO: react to changes in dimension
                let renderState = RenderState.make' console

                RenderState.enterAlternateScreen renderState

                let mutable cancels = 0

                let ctrlCHandler =
                    ConsoleCancelEventHandler (fun _ args ->
                        // Double-ctrlc to exit immediately
                        if Interlocked.Increment &cancels = 1 then
                            args.Cancel <- true
                    )

                ctrlC.Register ctrlCHandler

                let listener = worldFreezer ()

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

                finally
                    ctrlC.Unregister ctrlCHandler

                    (listener :> IAsyncDisposable).DisposeAsync().GetAwaiter().GetResult ()

                    RenderState.exitAlternateScreen renderState
                    RenderState.setCursorVisible renderState

                    complete.SetResult ()
            |> Thread
            |> _.Start()

        complete.Task

    let run state haveFrameworkHandleFocus processWorld vdom =
        run'
            CancellationToken.None
            (IConsole.make ())
            (CtrlCHandler.make ())
            WorldFreezer.listen
            state
            haveFrameworkHandleFocus
            processWorld
            vdom
