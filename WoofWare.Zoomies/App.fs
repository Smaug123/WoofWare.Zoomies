namespace WoofWare.Zoomies

[<RequireQualifiedAccess>]
module App =

    /// Cancel the cancellation token to dispose the
    let run<'state>
        (mutableState : 'state)
        (processWorld : WorldStateChange seq -> 'state -> unit)
        (vdom : 'state -> Vdom)
        =
        // TODO: react to changes in dimension
        let renderState = RenderState.make ()

        RenderState.enterAlternateScreen renderState

        try
            RenderState.setCursorInvisible renderState

            use listener = WorldFreezer.listen ()

            while true do
                listener.Refresh ()

                processWorld (listener.Changes ()) mutableState

                Render.oneStep renderState mutableState vdom

        finally
            RenderState.exitAlternateScreen renderState
