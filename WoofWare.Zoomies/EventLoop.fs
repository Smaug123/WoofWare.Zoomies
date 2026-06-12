namespace WoofWare.Zoomies

open System
open System.Threading
open System.Threading.Tasks

/// The blocking wait at the heart of the event-driven render loop.
[<RequireQualifiedAccess>]
module internal EventLoop =

    /// Block until the wake task completes, the deadline elapses, or the token is cancelled.
    /// `deadline` is a duration from now; ValueNone means wait indefinitely for the other two.
    ///
    /// Intended to run on the dedicated render-loop thread, so blocking is safe.
    let waitForWork (wake : Task) (deadline : TimeSpan voption) (ct : CancellationToken) : unit =
        if wake.IsCompleted || ct.IsCancellationRequested then
            ()
        else
            match deadline with
            | ValueSome d when d <= TimeSpan.Zero -> ()
            | _ ->

            // A linked source so the Task.Delay timer (or infinite-wait registration) is
            // released as soon as we wake for any reason, rather than accumulating one
            // registration per loop iteration.
            use delayCts = CancellationTokenSource.CreateLinkedTokenSource ct

            let delayTask =
                match deadline with
                | ValueNone -> Task.Delay (Timeout.InfiniteTimeSpan, delayCts.Token)
                | ValueSome d -> Task.Delay (d, delayCts.Token)

            // ANALYZER: synchronous blocking call allowed: we're on the dedicated render-loop
            // thread, which exists precisely to block here; nothing can deadlock against it.
            Task.WaitAny [| wake ; delayTask |] |> ignore<int>
            delayCts.Cancel ()
