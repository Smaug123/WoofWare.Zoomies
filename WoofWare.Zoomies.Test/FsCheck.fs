namespace WoofWare.Zoomies.Test

open FsCheck

[<AutoOpen>]
module FsCheck =
    let propConfig = Config.QuickThrowOnFailure.WithMaxTest(100).WithQuietOnSuccess true
