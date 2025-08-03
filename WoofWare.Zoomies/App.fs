namespace WoofWare.Zoomies

open System
open System.Threading
open System.Threading.Tasks

[<RequireQualifiedAccess>]
module App =

    /// We set up a ConsoleCancelEventHandler to suppress one Ctrl+C, and we also listen to stdin,
    /// for as long as this task is running.
    /// Cancel the CancellationToken to cause the render loop to quit and to unhook all these state listeners.
    let run<'state>
        (terminate : CancellationToken)
        (mutableState : 'state)
        (processWorld : WorldStateChange seq -> 'state -> unit)
        (vdom : 'state -> Vdom)
        : Task
        =
        fun () ->
            // TODO: react to changes in dimension
            let renderState = RenderState.make ()

            RenderState.enterAlternateScreen renderState

            let mutable cancels = 0

            let ctrlCHandler =
                ConsoleCancelEventHandler (fun _ args ->
                    // Double-ctrlc to exit immediately
                    if Interlocked.Increment &cancels = 1 then
                        args.Cancel <- true
                )

            Console.CancelKeyPress.AddHandler ctrlCHandler

            try
                RenderState.setCursorInvisible renderState

                use listener = WorldFreezer.listen ()

                while cancels = 0 && not terminate.IsCancellationRequested do
                    listener.Refresh ()

                    processWorld (listener.Changes ()) mutableState

                    Render.oneStep renderState mutableState vdom

            finally
                Console.CancelKeyPress.RemoveHandler ctrlCHandler
                RenderState.exitAlternateScreen renderState
        |> fun f -> Task.Factory.StartNew (f, TaskCreationOptions.LongRunning)
