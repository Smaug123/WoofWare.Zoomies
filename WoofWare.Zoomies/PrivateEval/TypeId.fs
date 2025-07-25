namespace WoofWare.Zoomies

type TypeId<'a> = private { Uid: int; Name: string }

/// <summary>
/// A function to apply to some TypeId.
/// </summary>
/// <remarks>
/// This was `type_id_set`'s `mapper` in the original OCaml.
/// </remarks>
type TypeIdEval<'ret> =
    /// A function to apply to some TypeId.
    abstract Eval<'a> : 'a TypeId -> 'ret

type TypeIdCrate =
    abstract Apply<'ret> : TypeIdEval<'ret> -> 'ret

    abstract Uid: int
    abstract Name: string

[<RequireQualifiedAccess>]
module TypeIdCrate =
    let make<'a> (t: 'a TypeId) : TypeIdCrate =
        { new TypeIdCrate with
            member _.Apply e = e.Eval t
            member _.Name = t.Name
            member _.Uid = t.Uid }
