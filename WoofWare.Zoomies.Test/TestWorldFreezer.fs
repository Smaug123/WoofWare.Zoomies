namespace WoofWare.Zoomies.Test

open System
open System.Threading
open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies

[<TestFixture>]
module TestWorldFreezer =

    [<Test>]
    let ``stops processing after cancellation`` () =
        task {
            let mutable keysRead = 0
            let keysAvailable () = true

            let readKey () =
                Interlocked.Increment &keysRead |> ignore<int>
                ConsoleKeyInfo ('x', ConsoleKey.X, false, false, false)

            let freezer = WorldFreezer.listen' keysAvailable readKey

            (freezer :> IDisposable).Dispose ()

            do! freezer.IsShutDown.WaitAsync (TimeSpan.FromSeconds 10.0)
            freezer.IsShutDown.IsCompletedSuccessfully |> shouldEqual true

            freezer.Refresh ()
            let _ = freezer.Changes () |> Seq.toList
            freezer.Refresh ()
            freezer.Changes () |> shouldBeEmpty
        }

    [<Test>]
    let ``clears previous changes on refresh`` () =
        let mutable callCount = 0

        let keys =
            [|
                ConsoleKeyInfo ('x', ConsoleKey.X, false, false, false)
                ConsoleKeyInfo ('y', ConsoleKey.Y, false, false, false)
            |]

        let keyAvailable () = callCount < keys.Length

        let readKey () =
            let key = keys.[callCount]
            Interlocked.Increment &callCount |> ignore<int>
            key

        use cts = new CancellationTokenSource ()

        use freezer = WorldFreezer.listen' keyAvailable readKey

        let seen = ResizeArray ()
        let mutable cont = true

        while cont do
            freezer.Refresh ()

            let result =
                freezer.Changes ()
                |> Seq.map (fun change ->
                    match change with
                    | WorldStateChange.Keystroke c -> c.KeyChar
                )
                |> Seq.toList

            seen.AddRange result

            match List.tryLast result with
            | Some 'y' -> cont <- false
            | _ -> ()

        seen |> Seq.toList |> shouldEqual [ 'x' ; 'y' ]

        freezer.Refresh ()
        freezer.Changes () |> shouldBeEmpty
