module WoofWare.PlayFetch.Test.TestList

open Expecto
open WoofWare.PlayFetch

let testListSorted (l1 : byte list) =
    List.isSortedBy id l1 |> (=) (List.sort l1 = l1)

[<Tests>]
let tests = testProperty "isSortedBy iff sorted" testListSorted
