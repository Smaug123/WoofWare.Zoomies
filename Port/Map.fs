module WoofWare.Zoomies.Port.Map

[<RequireQualifiedAccess>]
type DiffResult<'a> =
    | Left of 'a
    | Right of 'a
    | Unequal of 'a * 'a

let foldSymmetricDiff
    (oldMap : Map<'key, 'a>)
    (newMap : Map<'key, 'a>)
    (init : 'acc)
    (folder : 'acc -> 'key -> DiffResult<'a> -> 'acc)
    dataEqual
    : 'acc
    =
    let allKeys =
        Set.union (oldMap |> Map.keys |> Set.ofSeq) (newMap |> Map.keys |> Set.ofSeq)

    allKeys
    |> Set.fold
        (fun acc key ->
            match Map.tryFind key oldMap, Map.tryFind key newMap with
            | Some oldVal, None -> folder acc key (DiffResult.Left oldVal)
            | None, Some newVal -> folder acc key (DiffResult.Right newVal)
            | Some oldVal, Some newVal when not (dataEqual oldVal newVal) ->
                folder acc key (DiffResult.Unequal (oldVal, newVal))
            | _ -> acc
        )
        init
