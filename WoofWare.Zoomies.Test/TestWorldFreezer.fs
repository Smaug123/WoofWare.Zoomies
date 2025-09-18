namespace WoofWare.Zoomies.Test

open System
open System.Threading
open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies

[<TestFixture>]
module TestWorldFreezer =

    [<Test>]
    let ``clears previous changes on change dump`` () =
        task {
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
                freezer.RefreshExternal ()

                let result =
                    freezer.Changes ()
                    |> ValueOption.defaultValue [||]
                    |> Array.map (fun change ->
                        match change with
                        | WorldStateChange.Keystroke c -> c.KeyChar
                        | ApplicationEvent () -> failwith "no app events"
                        | MouseEvent _ -> failwith "no mouse events"
                        | KeyboardEvent _ -> failwith "no keyboard events"
                        | ApplicationEventException _ -> failwith "no exceptions possible"
                    )

                seen.AddRange result

                match Array.tryLast result with
                | Some 'y' -> cont <- false
                | _ -> ()

            seen |> Seq.toList |> shouldEqual [ 'x' ; 'y' ]

            freezer.RefreshExternal ()
            freezer.Changes () |> shouldEqual ValueNone
        }
