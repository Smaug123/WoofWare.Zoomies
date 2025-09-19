namespace WoofWare.Zoomies.Test

open FsCheck

[<AutoOpen>]
module FsCheck =
    let propConfig =
        Config.QuickThrowOnFailure.WithMaxTest(1000).WithQuietOnSuccess (true)
