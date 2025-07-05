namespace WoofWare.Zoomies

type TypeIdMapper<'ret> =
    abstract f<'a> : 'a TypeId -> 'ret

type TypeIdFolder<'acc> =
    abstract f<'a> : 'acc -> 'a TypeId -> 'acc

type TypeIdSet = { Ids: Map<int, TypeIdCrate> }

[<RequireQualifiedAccess>]
module TypeIdSet =
    let empty = { Ids = Map.empty }

    let add' s (cr: TypeIdCrate) = { Ids = s.Ids |> Map.add cr.Uid cr }

    let add s (ty: TypeId<'a>) = add' s (TypeIdCrate.make ty)

    let singleton (typeId: TypeId<'a>) = add empty typeId

    let isEmpty s = s.Ids.IsEmpty

    let length s = s.Ids.Count

    let iter s f =
        s.Ids |> Map.iter (fun _ t -> t.Apply f)

    let remove<'a> s (ty: 'a TypeId) = { Ids = s.Ids |> Map.remove ty.Uid }

    let union (a: TypeIdSet) (b: TypeIdSet) =
        (a, b.Ids) ||> Map.fold (fun acc _ cr -> add' acc cr)

    let fold t (init: 'acc) (folder: 'acc TypeIdFolder) : 'acc =
        (init, t.Ids)
        ||> Map.fold (fun acc _ cr ->
            { new TypeIdEval<_> with
                member _.Eval x = folder.f acc x }
            |> cr.Apply)

    let mapToList (t: TypeIdSet) (f: TypeIdEval<'a>) : 'a list =
        ([], t.Ids) ||> Map.fold (fun acc _ cr -> cr.Apply f :: acc)
