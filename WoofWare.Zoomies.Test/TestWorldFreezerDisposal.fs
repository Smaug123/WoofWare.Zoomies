namespace WoofWare.Zoomies.Test

open System
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open WoofWare.Zoomies

[<TestFixture>]
module TestWorldFreezerDisposal =

    /// An event which counts how many handlers are currently attached.
    type private CountingEvent () =
        let mutable attached = 0
        member _.Attached = Volatile.Read &attached

        interface IEvent<int> with
            member _.AddHandler _ =
                Interlocked.Increment &attached |> ignore<int>

            member _.RemoveHandler _ =
                Interlocked.Decrement &attached |> ignore<int>

            member _.Subscribe _ = failwith "not used"

    [<Test>]
    let ``a subscription racing with disposal is never leaked`` () =
        task {
            // The race window is a few instructions wide, so we need many attempts to observe it.
            // Before the fix, this reproduced within a few hundred iterations.
            for i in 1..2_000 do
                let freezer =
                    WorldFreezer.listen'<unit> UnrecognisedEscapeCodeBehaviour.Throw StopwatchMock.Empty

                let evt = CountingEvent ()
                let go = ref 0

                let subscriber =
                    Task.Factory.StartNew (
                        (fun () ->
                            // Spin rather than block, so that we start as close as possible to the disposal.
                            while Volatile.Read &go.contents = 0 do
                                Thread.SpinWait 1

                            try
                                (freezer :> IWorldBridge<unit>).SubscribeEvent evt ignore |> ignore<IDisposable>
                            with :? ObjectDisposedException ->
                                ()
                        ),
                        TaskCreationOptions.LongRunning
                    )

                Volatile.Write (&go.contents, 1)
                do! (freezer :> IAsyncDisposable).DisposeAsync ()
                do! subscriber

                // Once disposal has completed and the subscriber has returned, nothing may remain attached.
                if evt.Attached <> 0 then
                    failwith $"Subscription leaked at iteration %d{i}"
        }
