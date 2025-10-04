namespace WoofWare.Zoomies

type internal FakeUnit = private | FakeUnit

[<RequireQualifiedAccess>]
module internal FakeUnit =

    let fake () = FakeUnit
    let unfake (f : FakeUnit) = ()
