namespace WoofWare.Zoomies.Test

open NUnit.Framework
open WoofWare.Zoomies
open FsUnitTyped

[<TestFixture>]
module TestTrampoline =

    [<Test>]
    let ``can run`` () =
        let t =
            trampoline {
                let x = 3
                let! y = Trampoline.lift 4
                return y - x
            }

        t |> Trampoline.run |> shouldEqual 1
