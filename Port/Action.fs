namespace WoofWare.Zoomies.Port

#nowarn "3559"

open TypeEquality
open WoofWare.Zoomies.Port

type TypeId<'a> = private | TypeId

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
    abstract Eval<'inner, 'outer> : Teq<'a, Wrap<'inner, 'outer>> * ActionIdCrate -> 'ret

and WrapIdCrate<'a> =
    abstract Apply<'ret> : WrapIdEval<'a, 'ret> -> 'ret

and ModelResetIdEval<'a, 'ret> =
    abstract Eval<'inner> : Teq<'a, ModelResetter<'inner>> * ActionIdCrate -> 'ret

and ModelResetIdCrate<'a> =
    abstract Apply<'ret> : ModelResetIdEval<'a, 'ret> -> 'ret

and AssocIdEval<'a, 'ret> =
    abstract Eval<'key, 'inner> : Teq<'a, Assoc<'key, 'inner>> * ActionId<'inner> -> 'ret

and AssocIdCrate<'a> =
    abstract Apply<'ret> : AssocIdEval<'a, 'ret> -> 'ret

and AssocOnIdEval<'a, 'ret> =
    abstract Eval<'ioKey, 'modelKey, 'inner> : Teq<'a, AssocOn<'ioKey, 'modelKey, 'inner>> * ActionIdCrate -> 'ret

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

        SubId (
            { new SubIdCrate<Sub<'from, 'into>> with
                member _.Apply e = e.Eval (teq, fromId, intoId)
            }
        )

    /// Create a WrapId ActionId for inner/outer types
    let wrapId<'inner, 'outer> (inner : ActionIdCrate) : ActionId<Wrap<'inner, 'outer>> =
        let teq = Teq.refl<Wrap<'inner, 'outer>>

        WrapId (
            { new WrapIdCrate<Wrap<'inner, 'outer>> with
                member _.Apply e = e.Eval (teq, inner)
            }
        )

    /// Create a ModelResetId ActionId
    let modelResetId<'inner> (inner : ActionIdCrate) : ActionId<ModelResetter<'inner>> =
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
    let assocId<'key, 'inner> (action : ActionId<'inner>) : ActionId<Assoc<'key, 'inner>> =
        let teq = Teq.refl<Assoc<'key, 'inner>>

        AssocId
            { new AssocIdCrate<Assoc<'key, 'inner>> with
                member _.Apply e = e.Eval (teq, action)
            }


    /// Create an AssocOnId ActionId
    let assocOnId<'ioKey, 'modelKey, 'inner> (action : ActionIdCrate) : ActionId<AssocOn<'ioKey, 'modelKey, 'inner>> =
        let teq = Teq.refl<AssocOn<'ioKey, 'modelKey, 'inner>>

        AssocOnId
            { new AssocOnIdCrate<AssocOn<'ioKey, 'modelKey, 'inner>> with
                member _.Apply e = e.Eval (teq, action)
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
