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

and LeafIdEval<'action, 'ret> =
    abstract Eval : Teq<'a, Leaf<'action>> * TypeId<'action> -> 'ret

and LeafIdCrate<'a> =
    abstract Apply<'action, 'ret> : LeafIdEval<'action, 'ret> -> 'ret

and SubIdEval<'a, 'ret> =
    abstract Eval<'from, 'into> : Teq<'a, Sub<'from, 'into>> * ActionIdCrate * ActionIdCrate -> 'ret

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

module Action =

    /// Module for creating ActionId crates
    module ActionIdCrate =
        let make<'a> (value : ActionId<'a>) : ActionIdCrate =
            { new ActionIdCrate with
                member _.Apply e = e.Eval value
            }

    /// Module for creating Action crates
    module ActionCrate =
        let make<'a> (value : Action<'a>) : ActionCrate =
            { new ActionCrate with
                member _.Apply e = e.Eval value
            }

    /// Module for LeafIdCrate construction
    module LeafIdCrate =
        let make (action : Teq<'action, 'action>) : LeafIdCrate<'a> =
            { new LeafIdCrate<'a> with
                member _.Apply e = e.Eval action
            }

    /// Module for SubIdCrate construction
    module SubIdCrate =
        let make (fromId : ActionIdCrate) (intoId : ActionIdCrate) : SubIdCrate<'a> =
            { new SubIdCrate<'a> with
                member _.Apply e = e.Eval (fromId, intoId)
            }

    /// Module for WrapIdCrate construction
    module WrapIdCrate =
        let make (inner : ActionIdCrate) (outer : Teq<'outer, 'outer>) : WrapIdCrate<'a> =
            { new WrapIdCrate<'a> with
                member _.Apply e = e.Eval (inner, outer)
            }

    /// Module for ModelResetIdCrate construction
    module ModelResetIdCrate =
        let make (inner : ActionIdCrate) : ModelResetIdCrate<'a> =
            { new ModelResetIdCrate<'a> with
                member _.Apply e = e.Eval inner
            }

    /// Module for SwitchIdCrate construction
    module SwitchIdCrate =
        let make () : SwitchIdCrate<'a> =
            { new SwitchIdCrate<'a> with
                member _.Apply e = e.Eval ()
            }

    /// Module for LazyIdCrate construction
    module LazyIdCrate =
        let make () : LazyIdCrate<'a> =
            { new LazyIdCrate<'a> with
                member _.Apply e = e.Eval ()
            }

    /// Module for AssocIdCrate construction
    module AssocIdCrate =
        let make (key : Teq<'key, 'key>) (action : ActionIdCrate) : AssocIdCrate<'a> =
            { new AssocIdCrate<'a> with
                member _.Apply e = e.Eval (key, action)
            }

    /// Module for AssocOnIdCrate construction
    module AssocOnIdCrate =
        let make
            (ioKey : Teq<'ioKey, 'ioKey>)
            (modelKey : Teq<'modelKey, 'modelKey>)
            (action : ActionIdCrate)
            : AssocOnIdCrate<'a>
            =
            { new AssocOnIdCrate<'a> with
                member _.Apply e = e.Eval (ioKey, modelKey, action)
            }

    /// Module for LeafStaticCrate construction
    module LeafStaticCrate =
        let make (value : 'static') : LeafStaticCrate<'a> =
            { new LeafStaticCrate<'a> with
                member _.Apply e = e.Eval (box value)
            }

    /// Module for LeafDynamicCrate construction
    module LeafDynamicCrate =
        let make (value : 'dynamic) : LeafDynamicCrate<'a> =
            { new LeafDynamicCrate<'a> with
                member _.Apply e = e.Eval (box value)
            }

    /// Module for SubFromCrate construction
    module SubFromCrate =
        let make (action : ActionCrate) : SubFromCrate<'a> =
            { new SubFromCrate<'a> with
                member _.Apply e = e.Eval action
            }

    /// Module for SubIntoCrate construction
    module SubIntoCrate =
        let make (action : ActionCrate) : SubIntoCrate<'a> =
            { new SubIntoCrate<'a> with
                member _.Apply e = e.Eval action
            }

    /// Module for WrapInnerCrate construction
    module WrapInnerCrate =
        let make (action : ActionCrate) : WrapInnerCrate<'a> =
            { new WrapInnerCrate<'a> with
                member _.Apply e = e.Eval action
            }

    /// Module for WrapOuterCrate construction
    module WrapOuterCrate =
        let make (value : 'outer) : WrapOuterCrate<'a> =
            { new WrapOuterCrate<'a> with
                member _.Apply e = e.Eval (box value)
            }

    /// Module for ModelResetInnerCrate construction
    module ModelResetInnerCrate =
        let make (action : ActionCrate) : ModelResetInnerCrate<'a> =
            { new ModelResetInnerCrate<'a> with
                member _.Apply e = e.Eval action
            }

    /// Module for ModelResetOuterCrate construction
    module ModelResetOuterCrate =
        let make () : ModelResetOuterCrate<'a> =
            { new ModelResetOuterCrate<'a> with
                member _.Apply e = e.Eval ()
            }

    /// Module for SwitchCrate construction
    module SwitchCrate =
        let make (branch : int) (action : ActionCrate) (typeId : ActionIdCrate) : SwitchCrate<'a> =
            { new SwitchCrate<'a> with
                member _.Apply e = e.Eval (branch, action, typeId)
            }

    /// Module for LazyCrate construction
    module LazyCrate =
        let make (action : ActionCrate) (typeId : ActionIdCrate) : LazyCrate<'a> =
            { new LazyCrate<'a> with
                member _.Apply e = e.Eval (action, typeId)
            }

    /// Module for AssocCrate construction
    module AssocCrate =
        let make
            (key : 'key)
            (action : ActionCrate)
            (id : Teq<'key, 'key>)
            (compare : 'key -> 'key -> int)
            : AssocCrate<'a>
            =
            { new AssocCrate<'a> with
                member _.Apply e =
                    e.Eval (box key, action, External.unsafeCoerce id, fun x y -> compare (unbox x) (unbox y))
            }

    /// Module for AssocOnCrate construction
    module AssocOnCrate =
        let make
            (ioKey : 'ioKey)
            (modelKey : 'modelKey)
            (action : ActionCrate)
            (ioId : Teq<'ioKey, 'ioKey>)
            (ioCompare : 'ioKey -> 'ioKey -> int)
            : AssocOnCrate<'a>
            =
            { new AssocOnCrate<'a> with
                member _.Apply e =
                    e.Eval (
                        box ioKey,
                        box modelKey,
                        action,
                        External.unsafeCoerce ioId,
                        fun x y -> ioCompare (unbox x) (unbox y)
                    )
            }

    /// Type ID construction functions
    module TypeId =
        let nothingTypeId : Teq<obj, obj> = Teq.refl

        let nothing : ActionIdCrate =
            ActionIdCrate.make (LeafId (LeafIdCrate.make nothingTypeId))

        let leaf (typeId : Teq<'action, 'action>) : ActionIdCrate =
            ActionIdCrate.make (LeafId (LeafIdCrate.make (External.unsafeCoerce typeId)))

        let sub (from : ActionIdCrate) (into : ActionIdCrate) : ActionIdCrate =
            ActionIdCrate.make (SubId (SubIdCrate.make from into))

        let wrap (inner : ActionIdCrate) (outer : Teq<'outer, 'outer>) : ActionIdCrate =
            ActionIdCrate.make (WrapId (WrapIdCrate.make inner (External.unsafeCoerce outer)))

        let modelReset (inner : ActionIdCrate) : ActionIdCrate =
            ActionIdCrate.make (ModelResetId (ModelResetIdCrate.make inner))

        let lazy_ : ActionIdCrate = ActionIdCrate.make (LazyId (LazyIdCrate.make ()))
        let switch : ActionIdCrate = ActionIdCrate.make (SwitchId (SwitchIdCrate.make ()))

        let assoc (key : Teq<'key, 'key>) (action : ActionIdCrate) : ActionIdCrate =
            ActionIdCrate.make (AssocId (AssocIdCrate.make (External.unsafeCoerce key) action))

        let assocOn
            (ioKey : Teq<'ioKey, 'ioKey>)
            (modelKey : Teq<'modelKey, 'modelKey>)
            (action : ActionIdCrate)
            : ActionIdCrate
            =
            ActionIdCrate.make (
                AssocOnId (AssocOnIdCrate.make (External.unsafeCoerce ioKey) (External.unsafeCoerce modelKey) action)
            )

    /// Action construction functions
    let staticLeaf (action : 'static') : ActionCrate =
        ActionCrate.make (LeafStatic (LeafStaticCrate.make action))

    let dynamicLeaf (action : 'dynamic) : ActionCrate =
        ActionCrate.make (LeafDynamic (LeafDynamicCrate.make action))

    let subFrom (action : ActionCrate) : ActionCrate =
        ActionCrate.make (SubFrom (SubFromCrate.make action))

    let subInto (action : ActionCrate) : ActionCrate =
        ActionCrate.make (SubInto (SubIntoCrate.make action))

    let wrapInner (action : ActionCrate) : ActionCrate =
        ActionCrate.make (WrapInner (WrapInnerCrate.make action))

    let wrapOuter (action : 'outer) : ActionCrate =
        ActionCrate.make (WrapOuter (WrapOuterCrate.make action))

    let modelResetInner (action : ActionCrate) : ActionCrate =
        ActionCrate.make (ModelResetInner (ModelResetInnerCrate.make action))

    let modelResetOuter : ActionCrate =
        ActionCrate.make (ModelResetOuter (ModelResetOuterCrate.make ()))

    let switch (branch : int) (typeId : ActionIdCrate) (action : ActionCrate) : ActionCrate =
        ActionCrate.make (Switch (SwitchCrate.make branch action typeId))

    let lazy_ (typeId : ActionIdCrate) (action : ActionCrate) : ActionCrate =
        ActionCrate.make (Lazy (LazyCrate.make action typeId))

    let assoc (key : 'key) (id : Teq<'key, 'key>) (compare : 'key -> 'key -> int) (action : ActionCrate) : ActionCrate =
        ActionCrate.make (Assoc (AssocCrate.make key action id compare))

    let assocOn
        (ioKey : 'ioKey)
        (ioId : Teq<'ioKey, 'ioKey>)
        (ioCompare : 'ioKey -> 'ioKey -> int)
        (modelKey : 'modelKey)
        (action : ActionCrate)
        : ActionCrate
        =
        ActionCrate.make (AssocOn (AssocOnCrate.make ioKey modelKey action ioId ioCompare))
