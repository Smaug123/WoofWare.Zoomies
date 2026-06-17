namespace WoofWare.Zoomies.Demos

open System

module Program =

    let private getEnv (varName : string) : string option =
        match Environment.GetEnvironmentVariable varName with
        | null -> None
        | value -> Some value

    [<EntryPoint>]
    let main (args : string array) : int =
        match args with
        | [| "--list" |] ->
            printfn "Available demos:"

            for name in DemoRegistry.names do
                printfn "  %s" name

            0
        | [| demoName |] ->
            match Map.tryFind demoName DemoRegistry.demos with
            | Some run ->
                let handle = run getEnv
                handle.Finished.Wait ()
                0
            | None ->
                eprintfn "Unknown demo: %s" demoName
                eprintfn "Available demos: %s" (DemoRegistry.names |> String.concat ", ")
                1
        | _ ->
            eprintfn "Usage: WoofWare.Zoomies.Demos <demo-name>"
            eprintfn "       WoofWare.Zoomies.Demos --list"
            1
