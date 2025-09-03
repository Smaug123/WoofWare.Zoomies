// Human-reviewed
namespace WoofWare.Zoomies.Port

open WoofWare.Incremental

/// A universal map that can store values of different types, keyed by TypeId
type Environment =
    private
        {
            Values : Map<int, obj>
        }

[<RequireQualifiedAccess>]
module Environment =

    let empty : Environment =
        {
            Values = Map.empty
        }

    let addExn (env : Environment) (key : TypeId<'a>) (data : 'a Node) : Environment =
        if Map.containsKey key.Uid env.Values then
            failwithf "Key %s (uid %d) already exists in environment" key.Name key.Uid
        else
            {
                Values = Map.add key.Uid (box data) env.Values
            }

    let addOverwriting (env : Environment) (key : TypeId<'a>) (data : 'a Node) : Environment =
        {
            Values = Map.add key.Uid (box data) env.Values
        }

    let find (env : Environment) (key : TypeId<'a>) : 'a Node option =
        match Map.tryFind key.Uid env.Values with
        | Some obj ->
            match obj with
            | :? ('a Node) as node -> Some node
            | _ ->
                failwithf
                    "Type mismatch for key %s (uid %d): expected %s but found different type"
                    key.Name
                    key.Uid
                    (typeof<'a Node>.Name)
        | None -> None
