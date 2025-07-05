module WoofWare.Zoomies.Test.TestTrampoline

open WoofWare.Zoomies
open Expecto
open FsUnitTyped

[<Tests>]
let tests =
    testList
        "TestTrampoline"
        [ test "can run" {
              let t =
                  trampoline {
                      let x = 3
                      let! y = Trampoline.lift 4
                      return y - x
                  }

              t |> Trampoline.run |> shouldEqual 1
          } ]
