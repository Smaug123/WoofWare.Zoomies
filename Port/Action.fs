namespace WoofWare.Zoomies.Port

#nowarn "3559"

open TypeEquality

type TypeId<'a> = private | TypeId


module TypeId =

    let same_witness (_ : TypeId<'a>) (id' : TypeId<'b>) : Teq<'a, 'b> option =
        match box id' with
        | :? TypeId<'a> -> Some (Teq.Cong.believeMe Teq.refl)
        | _ -> None

/// Private phantom types for GADTs
[<Struct>]
type Leaf<'a> = private | Leaf

[<Struct>]
type Sub<'from, 'into> = private | Sub

[<Struct>]
type ModelResetter<'a> = private | ModelResetter

[<Struct>]
type Wrap<'inner, 'outer> = private | Wrap

[<Struct>]
type WrapStatic<'inner> = private | WrapStatic

[<Struct>]
type WrapDynamic<'inner, 'outer> = private | WrapDynamic

[<Struct>]
type Switch = private | Switch

[<Struct>]
type Lazy' = private | Lazy'

[<Struct>]
type Assoc<'key, 'inner> = private | Assoc

[<Struct>]
type AssocOn<'ioKey, 'modelKey, 'inner> = private | AssocOn

module Teq =
    module Cong =
        let leaf (teq : Teq<'a, 'b>) : Teq<'a Leaf, 'b Leaf> = Teq.Cong.believeMe teq

        let sub
            (teqFrom : Teq<'from1, 'from2>)
            (teqInto : Teq<'into1, 'into2>)
            : Teq<Sub<'from1, 'into1>, Sub<'from2, 'into2>>
            =
            Teq.Cong.believeMe (Teq.refl)

        let wrap
            (teqInner : Teq<'inner1, 'inner2>)
            (teqOuter : Teq<'outer1, 'outer2>)
            : Teq<Wrap<'inner1, 'outer1>, Wrap<'inner2, 'outer2>>
            =
            Teq.Cong.believeMe (Teq.refl)

        let assoc
            (teqKey : Teq<'key1, 'key2>)
            (teqInner : Teq<'inner1, 'inner2>)
            : Teq<Assoc<'key1, 'inner1>, Assoc<'key2, 'inner2>>
            =
            Teq.Cong.believeMe (Teq.refl)

        let assocOn
            (teqIoKey : Teq<'ioKey1, 'ioKey2>)
            (teqModelKey : Teq<'modelKey1, 'modelKey2>)
            (teqInner : Teq<'inner1, 'inner2>)
            : Teq<AssocOn<'ioKey1, 'modelKey1, 'inner1>, AssocOn<'ioKey2, 'modelKey2, 'inner2>>
            =
            Teq.Cong.believeMe (Teq.refl)

        let modelResetter (teqInner : Teq<'inner1, 'inner2>) : Teq<ModelResetter<'inner1>, ModelResetter<'inner2>> =
            Teq.Cong.believeMe (Teq.refl)




/// GADT for action IDs using crate pattern
type ActionIdEval<'ret> =
    abstract Eval<'a> : ActionId<'a> -> 'ret

and ActionIdCrate =
    abstract Apply<'ret> : ActionIdEval<'ret> -> 'ret

and ActionId<'a> =
    | LeafId of LeafIdCrate<'a>
    | SubId of SubIdCrate<'a>
    | WrapId of WrapIdCrate<'a>
    | ModelResetId of ModelResetIdCrate<'a>
    | SwitchId of Teq<'a, Switch>
    | LazyId of Teq<'a, Lazy'>
    | AssocId of AssocIdCrate<'a>
    | AssocOnId of AssocOnIdCrate<'a>

and LeafIdEval<'a, 'ret> =
    abstract Eval<'action> : Teq<'a, Leaf<'action>> * TypeId<'action> -> 'ret

and LeafIdCrate<'a> =
    abstract Apply<'ret> : LeafIdEval<'a, 'ret> -> 'ret

and SubIdEval<'a, 'ret> =
    abstract Eval<'from, 'into> : Teq<'a, Sub<'from, 'into>> * ActionId<'from> * ActionId<'into> -> 'ret

and SubIdCrate<'a> =
    abstract Apply<'ret> : SubIdEval<'a, 'ret> -> 'ret

and WrapIdEval<'a, 'ret> =
    abstract Eval<'inner, 'outer> : Teq<'a, Wrap<'inner, 'outer>> * ActionId<'inner> * TypeId<'outer> -> 'ret

and WrapIdCrate<'a> =
    abstract Apply<'ret> : WrapIdEval<'a, 'ret> -> 'ret

and ModelResetIdEval<'a, 'ret> =
    abstract Eval<'inner> : Teq<'a, ModelResetter<'inner>> * ActionId<'inner> -> 'ret

and ModelResetIdCrate<'a> =
    abstract Apply<'ret> : ModelResetIdEval<'a, 'ret> -> 'ret

and AssocIdEval<'a, 'ret> =
    abstract Eval<'key, 'inner> : Teq<'a, Assoc<'key, 'inner>> * TypeId<'key> * ActionId<'inner> -> 'ret

and AssocIdCrate<'a> =
    abstract Apply<'ret> : AssocIdEval<'a, 'ret> -> 'ret

and AssocOnIdEval<'a, 'ret> =
    abstract Eval<'ioKey, 'modelKey, 'inner> :
        Teq<'a, AssocOn<'ioKey, 'modelKey, 'inner>> * TypeId<'ioKey> * TypeId<'modelKey> * ActionId<'inner> -> 'ret

and AssocOnIdCrate<'a> =
    abstract Apply<'ret> : AssocOnIdEval<'a, 'ret> -> 'ret

/// GADT for actions using crate pattern
and ActionEval<'ret> =
    abstract Eval<'a> : Action<'a> -> 'ret

and ActionCrate =
    abstract Apply<'ret> : ActionEval<'ret> -> 'ret

and Action<'a> =
    | LeafStatic of LeafStaticCrate<'a>
    | LeafDynamic of LeafDynamicCrate<'a>
    | SubFrom of SubFromCrate<'a>
    | SubInto of SubIntoCrate<'a>
    | WrapInner of WrapInnerCrate<'a>
    | WrapOuter of WrapOuterCrate<'a>
    | ModelResetInner of ModelResetInnerCrate<'a>
    | ModelResetOuter of ModelResetOuterCrate<'a>
    | Switch of SwitchCrate<'a>
    | Lazy of LazyCrate<'a>
    | Assoc of AssocCrate<'a>
    | AssocOn of AssocOnCrate<'a>

and LeafStaticEval<'a, 'ret> =
    abstract Eval<'static'> : Teq<'a, Leaf<'static'>> * 'static' -> 'ret

and LeafStaticCrate<'a> =
    abstract Apply<'ret> : LeafStaticEval<'a, 'ret> -> 'ret

and LeafDynamicEval<'a, 'ret> =
    abstract Eval<'dynamic> : Teq<'a, Leaf<'dynamic>> * 'dynamic -> 'ret

and LeafDynamicCrate<'a> =
    abstract Apply<'ret> : LeafDynamicEval<'a, 'ret> -> 'ret

and SubFromEval<'a, 'ret> =
    abstract Eval<'from, 'into> : Teq<'a, Sub<'from, 'into>> * Action<'from> -> 'ret

and SubFromCrate<'a> =
    abstract Apply<'ret> : SubFromEval<'a, 'ret> -> 'ret

and SubIntoEval<'a, 'ret> =
    abstract Eval<'from, 'into> : Teq<'a, Sub<'from, 'into>> * Action<'into> -> 'ret

and SubIntoCrate<'a> =
    abstract Apply<'ret> : SubIntoEval<'a, 'ret> -> 'ret

and WrapInnerEval<'a, 'ret> =
    abstract Eval<'inner, 'outer> : Teq<'a, Wrap<'inner, 'outer>> * Action<'inner> -> 'ret

and WrapInnerCrate<'a> =
    abstract Apply<'ret> : WrapInnerEval<'a, 'ret> -> 'ret

and WrapOuterEval<'a, 'ret> =
    abstract Eval<'inner, 'outer> : Teq<'a, Wrap<'inner, 'outer>> * Action<'outer> -> 'ret

and WrapOuterCrate<'a> =
    abstract Apply<'ret> : WrapOuterEval<'a, 'ret> -> 'ret

and ModelResetInnerEval<'a, 'ret> =
    abstract Eval<'inner> : Teq<'a, ModelResetter<'inner>> * Action<'inner> -> 'ret

and ModelResetInnerCrate<'a> =
    abstract Apply<'ret> : ModelResetInnerEval<'a, 'ret> -> 'ret

and ModelResetOuterEval<'a, 'ret> =
    abstract Eval<'inner> : Teq<'a, ModelResetter<'inner>> -> 'ret

and ModelResetOuterCrate<'a> =
    abstract Apply<'ret> : ModelResetOuterEval<'a, 'ret> -> 'ret

and SwitchEval<'a, 'ret> =
    abstract Eval<'inner> : Teq<'a, Switch> * int * Action<'inner> * ActionId<'inner> -> 'ret

and SwitchCrate<'a> =
    abstract Apply<'ret> : SwitchEval<'a, 'ret> -> 'ret

and LazyEval<'a, 'ret> =
    abstract Eval<'inner> : Teq<'a, Lazy'> * Action<'inner> * ActionId<'inner> -> 'ret

and LazyCrate<'a> =
    abstract Apply<'ret> : LazyEval<'a, 'ret> -> 'ret

and AssocEval<'a, 'ret> =
    abstract Eval<'key, 'inner> :
        Teq<'a, Assoc<'key, 'inner>> * 'key * Action<'inner> * TypeId<'key> * ('key -> 'key -> int) -> 'ret

and AssocCrate<'a> =
    abstract Apply<'ret> : AssocEval<'a, 'ret> -> 'ret

and AssocOnEval<'a, 'ret> =
    abstract Eval<'ioKey, 'modelKey, 'inner> :
        Teq<'a, AssocOn<'ioKey, 'modelKey, 'inner>> *
        'ioKey *
        'modelKey *
        Action<'inner> *
        TypeId<'ioKey> *
        ('ioKey -> 'ioKey -> int) ->
            'ret

and AssocOnCrate<'a> =
    abstract Apply<'ret> : AssocOnEval<'a, 'ret> -> 'ret

module ActionId =

    /// It's unclear that recursing over the data structure is necessary when reflection exists, but:
    /// - It proves we've got the types correct, as this function is implementable.
    /// - This is a useful demonstration of recursing over the ActionId type.
    let rec sameWitness<'a, 'b> (idA : ActionId<'a>) (idB : ActionId<'b>) : Teq<'a, 'b> option =
        match idA, idB with
        | LeafId crateA, LeafId crateB ->
            crateA.Apply
                { new LeafIdEval<_, _> with
                    member _.Eval (teqA, typeIdA) =
                        crateB.Apply
                            { new LeafIdEval<_, _> with
                                member _.Eval (teqB, typeIdB) =
                                    match TypeId.same_witness typeIdA typeIdB with
                                    | None -> None
                                    | Some teqAB ->
                                        let teqLeafAB = Teq.Cong.leaf teqAB
                                        let step1 = Teq.transitivity teqA teqLeafAB
                                        let step2 = Teq.symmetry teqB
                                        Some (Teq.transitivity step1 step2)
                            }
                }
        | SubId crateA, SubId crateB ->
            crateA.Apply
                { new SubIdEval<_, _> with
                    member _.Eval
                        (teqA : Teq<'a, Sub<'from1, 'into1>>, fromIdA : ActionId<'from1>, intoIdA : ActionId<'into1>)
                        =
                        crateB.Apply
                            { new SubIdEval<_, _> with
                                member _.Eval
                                    (
                                        teqB : Teq<'b, Sub<'from2, 'into2>>,
                                        fromIdB : ActionId<'from2>,
                                        intoIdB : ActionId<'into2>
                                    )
                                    =
                                    match sameWitness fromIdA fromIdB, sameWitness intoIdA intoIdB with
                                    | Some teqFrom, Some teqInto ->
                                        let teqSubAB = Teq.Cong.sub teqFrom teqInto
                                        let step1 = Teq.transitivity teqA teqSubAB
                                        let step2 = Teq.symmetry teqB
                                        Some (Teq.transitivity step1 step2)
                                    | _ -> None
                            }
                }
        | WrapId crateA, WrapId crateB ->
            crateA.Apply
                { new WrapIdEval<_, _> with
                    member _.Eval
                        (
                            teqA : Teq<'a, Wrap<'inner1, 'outer1>>,
                            innerIdA : ActionId<'inner1>,
                            outerIdA : TypeId<'outer1>
                        )
                        =
                        crateB.Apply
                            { new WrapIdEval<_, _> with
                                member _.Eval
                                    (
                                        teqB : Teq<'b, Wrap<'inner2, 'outer2>>,
                                        innerIdB : ActionId<'inner2>,
                                        outerIdB : TypeId<'outer2>
                                    )
                                    =
                                    match sameWitness innerIdA innerIdB, TypeId.same_witness outerIdA outerIdB with
                                    | Some teqInner, Some teqOuter ->
                                        let teqWrapAB = Teq.Cong.wrap teqInner teqOuter
                                        let step1 = Teq.transitivity teqA teqWrapAB
                                        let step2 = Teq.symmetry teqB
                                        Some (Teq.transitivity step1 step2)
                                    | _ -> None
                            }
                }
        | ModelResetId crateA, ModelResetId crateB ->
            crateA.Apply
                { new ModelResetIdEval<_, _> with
                    member _.Eval<'inner1> (teqA : Teq<'a, ModelResetter<'inner1>>, innerIdA : ActionId<'inner1>) =
                        crateB.Apply
                            { new ModelResetIdEval<_, _> with
                                member _.Eval<'inner2>
                                    (teqB : Teq<'b, ModelResetter<'inner2>>, innerIdB : ActionId<'inner2>)
                                    =
                                    match sameWitness innerIdA innerIdB with
                                    | Some teqInner ->
                                        let teqModelResetAB = Teq.Cong.modelResetter teqInner
                                        let step1 = Teq.transitivity teqA teqModelResetAB
                                        let step2 = Teq.symmetry teqB
                                        Some (Teq.transitivity step1 step2)
                                    | None -> None
                            }
                }
        | SwitchId teqA, SwitchId teqB ->
            let step1 = teqA
            let step2 = Teq.symmetry teqB
            Some (Teq.transitivity step1 step2)
        | LazyId teqA, LazyId teqB ->
            let step1 = teqA
            let step2 = Teq.symmetry teqB
            Some (Teq.transitivity step1 step2)
        | AssocId crateA, AssocId crateB ->
            crateA.Apply
                { new AssocIdEval<_, _> with
                    member _.Eval<'key1, 'inner1>
                        (teqA : Teq<'a, Assoc<'key1, 'inner1>>, keyIdA, innerIdA : ActionId<'inner1>)
                        =
                        crateB.Apply
                            { new AssocIdEval<_, _> with
                                member _.Eval<'key2, 'inner2>
                                    (teqB : Teq<'b, Assoc<'key2, 'inner2>>, keyIdB, innerIdB : ActionId<'inner2>)
                                    =
                                    match sameWitness innerIdA innerIdB, TypeId.same_witness keyIdA keyIdB with
                                    | Some teqInner, Some teqKey ->
                                        let teqAssocAB = Teq.Cong.assoc teqKey teqInner
                                        let step1 = Teq.transitivity teqA teqAssocAB
                                        let step2 = Teq.symmetry teqB
                                        Some (Teq.transitivity step1 step2)
                                    | _ -> None
                            }
                }
        | AssocOnId crateA, AssocOnId crateB ->
            crateA.Apply
                { new AssocOnIdEval<_, _> with
                    member _.Eval<'ioKey1, 'modelKey1, 'inner1>
                        (
                            teqA : Teq<'a, AssocOn<'ioKey1, 'modelKey1, 'inner1>>,
                            ioKeyIdA : TypeId<'ioKey1>,
                            modelKeyIdA : TypeId<'modelKey1>,
                            innerIdA : ActionId<'inner1>
                        )
                        =
                        crateB.Apply
                            { new AssocOnIdEval<_, _> with
                                member _.Eval<'ioKey2, 'modelKey2, 'inner2>
                                    (
                                        teqB : Teq<'b, AssocOn<'ioKey2, 'modelKey2, 'inner2>>,
                                        ioKeyIdB : TypeId<'ioKey2>,
                                        modelKeyIdB : TypeId<'modelKey2>,
                                        innerIdB : ActionId<'inner2>
                                    )
                                    =
                                    match
                                        sameWitness innerIdA innerIdB,
                                        TypeId.same_witness ioKeyIdA ioKeyIdB,
                                        TypeId.same_witness modelKeyIdA modelKeyIdB
                                    with
                                    | Some teqInner, Some teqIoKey, Some teqModelKey ->
                                        let teqAssocOnAB = Teq.Cong.assocOn teqIoKey teqModelKey teqInner
                                        let step1 = Teq.transitivity teqA teqAssocOnAB
                                        let step2 = Teq.symmetry teqB
                                        Some (Teq.transitivity step1 step2)
                                    | _ -> None
                            }
                }
        | _ ->
            // Different constructor types cannot be equal
            None

    /// Create a LeafId ActionId for a specific action type
    let leafId<'action> (typeId : TypeId<'action>) : ActionId<Leaf<'action>> =
        let teq = Teq.refl<Leaf<'action>>

        LeafId
            { new LeafIdCrate<_> with
                member _.Apply e = e.Eval (teq, typeId)
            }


    /// Create a SubId ActionId for from/into types
    let subId<'from, 'into> (fromId : ActionId<'from>) (intoId : ActionId<'into>) : ActionId<Sub<'from, 'into>> =
        let teq = Teq.refl<Sub<'from, 'into>>

        SubId
            { new SubIdCrate<Sub<'from, 'into>> with
                member _.Apply e = e.Eval (teq, fromId, intoId)
            }


    /// Create a WrapId ActionId for inner/outer types
    let wrapId<'inner, 'outer> (inner : ActionId<'inner>) (outer : TypeId<'outer>) : ActionId<Wrap<'inner, 'outer>> =
        let teq = Teq.refl<Wrap<'inner, 'outer>>

        WrapId
            { new WrapIdCrate<Wrap<'inner, 'outer>> with
                member _.Apply e = e.Eval (teq, inner, outer)
            }


    /// Create a ModelResetId ActionId
    let modelResetId<'inner> (inner : ActionId<'inner>) : ActionId<ModelResetter<'inner>> =
        let teq = Teq.refl<ModelResetter<'inner>>

        ModelResetId
            { new ModelResetIdCrate<ModelResetter<'inner>> with
                member _.Apply e = e.Eval (teq, inner)
            }


    /// Create a SwitchId ActionId
    let switchId () : ActionId<Switch> =
        let teq = Teq.refl<Switch>
        SwitchId teq

    /// Create a LazyId ActionId
    let lazyId () : ActionId<Lazy'> =
        let teq = Teq.refl<Lazy'>
        LazyId teq

    /// Create an AssocId ActionId
    let assocId<'key, 'inner> key (action : ActionId<'inner>) : ActionId<Assoc<'key, 'inner>> =
        let teq = Teq.refl<Assoc<'key, 'inner>>

        AssocId
            { new AssocIdCrate<Assoc<'key, 'inner>> with
                member _.Apply e = e.Eval (teq, key, action)
            }


    /// Create an AssocOnId ActionId
    let assocOnId<'ioKey, 'modelKey, 'inner>
        ioKey
        modelKey
        (action : ActionId<'inner>)
        : ActionId<AssocOn<'ioKey, 'modelKey, 'inner>>
        =
        let teq = Teq.refl<AssocOn<'ioKey, 'modelKey, 'inner>>

        AssocOnId
            { new AssocOnIdCrate<AssocOn<'ioKey, 'modelKey, 'inner>> with
                member _.Apply e = e.Eval (teq, ioKey, modelKey, action)
            }


module Action =
    /// Create a LeafStatic Action
    let leafStatic<'static'> (value : 'static') : Action<Leaf<'static'>> =
        let teq = Teq.refl<Leaf<'static'>>

        LeafStatic
            { new LeafStaticCrate<Leaf<'static'>> with
                member _.Apply e = e.Eval (teq, value)
            }


    /// Create a LeafDynamic Action
    let leafDynamic<'dynamic> (value : 'dynamic) : Action<Leaf<'dynamic>> =
        let teq = Teq.refl<Leaf<'dynamic>>

        LeafDynamic
            { new LeafDynamicCrate<Leaf<'dynamic>> with
                member _.Apply e = e.Eval (teq, value)
            }


    /// Create a SubFrom Action
    let subFrom<'from, 'into> (action : Action<'from>) : Action<Sub<'from, 'into>> =
        let teq = Teq.refl<Sub<'from, 'into>>

        SubFrom
            { new SubFromCrate<Sub<'from, 'into>> with
                member _.Apply e = e.Eval (teq, action)
            }


    /// Create a SubInto Action
    let subInto<'from, 'into> (action : Action<'into>) : Action<Sub<'from, 'into>> =
        let teq = Teq.refl<Sub<'from, 'into>>

        SubInto
            { new SubIntoCrate<Sub<'from, 'into>> with
                member _.Apply e = e.Eval (teq, action)
            }


    /// Create a WrapInner Action
    let wrapInner<'inner, 'outer> (action : Action<'inner>) : Action<Wrap<'inner, 'outer>> =
        let teq = Teq.refl<Wrap<'inner, 'outer>>

        WrapInner
            { new WrapInnerCrate<Wrap<'inner, 'outer>> with
                member _.Apply e = e.Eval (teq, action)
            }


    /// Create a WrapOuter Action
    let wrapOuter<'inner, 'outer> (action : Action<'outer>) : Action<Wrap<'inner, 'outer>> =
        let teq = Teq.refl<Wrap<'inner, 'outer>>

        WrapOuter
            { new WrapOuterCrate<Wrap<'inner, 'outer>> with
                member _.Apply e = e.Eval (teq, action)
            }


    /// Create a ModelResetInner Action
    let modelResetInner<'inner> (action : Action<'inner>) : Action<ModelResetter<'inner>> =
        let teq = Teq.refl<ModelResetter<'inner>>

        ModelResetInner
            { new ModelResetInnerCrate<ModelResetter<'inner>> with
                member _.Apply e = e.Eval (teq, action)
            }


    /// Create a ModelResetOuter Action
    let modelResetOuter<'inner> () : Action<ModelResetter<'inner>> =
        let teq = Teq.refl<ModelResetter<'inner>>

        ModelResetOuter
            { new ModelResetOuterCrate<ModelResetter<'inner>> with
                member _.Apply e = e.Eval (teq)
            }


    /// Create a Switch Action
    let switch<'inner> (branch : int) (action : Action<'inner>) (typeId : ActionId<'inner>) : Action<Switch> =
        let teq = Teq.refl<Switch>

        Switch
            { new SwitchCrate<Switch> with
                member _.Apply e = e.Eval (teq, branch, action, typeId)
            }


    /// Create a Lazy Action
    let lazy_<'inner> (action : Action<'inner>) (typeId : ActionId<'inner>) : Action<Lazy'> =
        let teq = Teq.refl<Lazy'>

        Lazy
            { new LazyCrate<Lazy'> with
                member _.Apply e = e.Eval (teq, action, typeId)
            }


    /// Create an Assoc Action
    let assoc<'key, 'inner>
        (key : 'key)
        (action : Action<'inner>)
        (typeId : TypeId<'key>)
        (compare : 'key -> 'key -> int)
        : Action<Assoc<'key, 'inner>>
        =
        let teq = Teq.refl<Assoc<'key, 'inner>>

        Assoc
            { new AssocCrate<Assoc<'key, 'inner>> with
                member _.Apply e =
                    e.Eval (teq, key, action, typeId, compare)
            }


    /// Create an AssocOn Action
    let assocOn<'ioKey, 'modelKey, 'inner>
        (ioKey : 'ioKey)
        (modelKey : 'modelKey)
        (action : Action<'inner>)
        (ioTypeId : TypeId<'ioKey>)
        (ioCompare : 'ioKey -> 'ioKey -> int)
        : Action<AssocOn<'ioKey, 'modelKey, 'inner>>
        =
        let teq = Teq.refl<AssocOn<'ioKey, 'modelKey, 'inner>>

        AssocOn
            { new AssocOnCrate<AssocOn<'ioKey, 'modelKey, 'inner>> with
                member _.Apply e =
                    e.Eval (teq, ioKey, modelKey, action, ioTypeId, ioCompare)
            }
