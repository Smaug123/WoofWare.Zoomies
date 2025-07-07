namespace WoofWare.Zoomies.Test

open Expecto

module Program =
    [<EntryPoint>]
    let main argv =
        let res = runTestsInAssemblyWithCLIArgs [] argv
        res
