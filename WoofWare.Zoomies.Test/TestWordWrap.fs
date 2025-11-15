namespace WoofWare.Zoomies.Test

open NUnit.Framework
open FsUnitTyped
open FsCheck
open WoofWare.Zoomies

[<TestFixture>]
module TestWordWrap =

    [<Test>]
    let ``empty string is assigned no size`` () =
        let property (width : int) =
            let width = 1 + abs width
            Layout.wordWrapCount "" width |> shouldEqual 0

        Check.One (propConfig, property)
