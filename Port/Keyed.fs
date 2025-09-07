namespace WoofWare.Zoomies.Port

open TypeEquality

// Keyed values with existential types, using the crate pattern for type safety
module Keyed =

    type KeyedEval<'ret> =
        abstract Eval<'k when 'k : comparison> : key : 'k -> 'ret

    type KeyedCrate =
        abstract Apply<'ret> : KeyedEval<'ret> -> 'ret

    let compareKeyedCrate (crateA : KeyedCrate) (crateB : KeyedCrate) =
        crateA.Apply
            { new KeyedEval<_> with
                member _.Eval (key1 : 'key1) =
                    crateB.Apply
                        { new KeyedEval<_> with
                            member _.Eval (key2 : 'key2) =
                                match Teq.tryRefl<'key1, 'key2> with
                                | Some teq ->
                                    // Use the type equality to safely compare keys of the same type
                                    // teq proves that 'k1 = 'k2, so we can use it to cast key2 to the same type as key1
                                    let key2Converted = Teq.cast (Teq.symmetry teq) key2
                                    compare key1 key2Converted
                                | None -> compare (hash typeof<'key1>) (hash typeof<'key2>)
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

                | _ -> invalidArg "obj" "type mismatch"

        override this.Equals (obj) =
            match obj with
            | :? Keyed as other -> compare this other = 0
            | _ -> false

        override this.GetHashCode () =
            // Simple hash implementation
            hash "Keyed"

    module KeyedCrate =
        let make<'k when 'k : comparison> (key : 'k) =
            { new KeyedCrate with
                member _.Apply e = e.Eval (key)
            }

    let create (key : 'k) : Keyed = Keyed.Keyed (KeyedCrate.make key)


    // Helper function to extract values from Keyed for debugging/serialization
    let toString (Keyed.Keyed crate) =
        crate.Apply
            { new KeyedEval<_> with
                member _.Eval key = key.ToString ()
            }
