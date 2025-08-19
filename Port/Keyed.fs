namespace WoofWare.Zoomies.Port

open TypeEquality

// Keyed values with existential types, using the crate pattern for type safety
module Keyed =

    type KeyedEval<'ret> =
        abstract Eval<'k> : key : 'k * id : TypeId<'k> * compare : ('k -> 'k -> int) -> 'ret

    type KeyedCrate =
        abstract Apply<'ret> : KeyedEval<'ret> -> 'ret

    let compareKeyedCrate (crateA : KeyedCrate) (crateB : KeyedCrate) =
        crateA.Apply
            { new KeyedEval<_> with
                member _.Eval (key1, id1, compare1) =
                    crateB.Apply
                        { new KeyedEval<_> with
                            member _.Eval (key2, id2, compare2) =
                                match TypeId.same_witness id1 id2 with
                                | Some teq ->
                                    // Use the type equality to safely compare keys of the same type
                                    // teq proves that 'k1 = 'k2, so we can use it to cast key2 to the same type as key1
                                    let key2Converted = Teq.cast (Teq.symmetry teq) key2
                                    compare1 key1 key2Converted
                                | None ->
                                    // Different types: compare using TypeId hash codes for stability
                                    // This will never return 0 since type IDs are different
                                    let hash1 = id1.GetHashCode ()
                                    let hash2 = id2.GetHashCode ()

                                    if hash1 < hash2 then -1
                                    elif hash1 > hash2 then 1
                                    else hash1.CompareTo (hash2) // fallback, should be rare
                        }
            }

    [<RequireQualifiedAccess>]
    [<CustomComparison>]
    [<CustomEquality>]
    type Keyed =
        | Keyed of KeyedCrate

        interface System.IComparable<Keyed> with
            member this.CompareTo (other) =
                (this :> System.IComparable).CompareTo (other)

        interface System.IComparable with
            member this.CompareTo (obj) =
                match obj with
                | :? Keyed as other ->
                    let Keyed this, Keyed other = this, other
                    compareKeyedCrate this other

                | _ -> -1

        override this.Equals (obj) =
            match obj with
            | :? Keyed as other -> compare this other = 0
            | _ -> false

        override this.GetHashCode () =
            // Simple hash implementation
            hash "Keyed"

    module KeyedCrate =
        let make<'k> (key : 'k) (id : TypeId<'k>) (compare : 'k -> 'k -> int) : KeyedCrate =
            { new KeyedCrate with
                member _.Apply e = e.Eval (key, id, compare)
            }

    let create (key : 'k) (id : TypeId<'k>) (compare : 'k -> 'k -> int) : Keyed =
        Keyed.Keyed (KeyedCrate.make key id compare)


    // Helper function to extract values from Keyed for debugging/serialization
    let toString (Keyed.Keyed crate) =
        crate.Apply
            { new KeyedEval<_> with
                member _.Eval (key, _id, _compare) = key.ToString ()
            }
