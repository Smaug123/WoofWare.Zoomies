namespace WoofWare.Zoomies.Port

#nowarn "3559"

open TypeEquality
open WoofWare.Zoomies.Port

/// Private phantom types for GADTs
[<Struct>]
type Leaf = private | Leaf
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
    | LeafId of LeafIdCrate
    | SubId of SubIdCrate
    | WrapId of WrapIdCrate
    | ModelResetId of ModelResetIdCrate
    | SwitchId of SwitchIdCrate
    | LazyId of LazyIdCrate
    | AssocId of AssocIdCrate
    | AssocOnId of AssocOnIdCrate

and LeafIdEval<'ret> =
    abstract Eval : Teq<'action, 'action> -> 'ret

and LeafIdCrate =
    abstract Apply<'ret> : LeafIdEval<'ret> -> 'ret

and SubIdEval<'ret> =
    abstract Eval : ActionIdCrate * ActionIdCrate -> 'ret

and SubIdCrate =
    abstract Apply<'ret> : SubIdEval<'ret> -> 'ret

and WrapIdEval<'ret> =
    abstract Eval : ActionIdCrate * Teq<'outer, 'outer> -> 'ret

and WrapIdCrate =
    abstract Apply<'ret> : WrapIdEval<'ret> -> 'ret

and ModelResetIdEval<'ret> =
    abstract Eval : ActionIdCrate -> 'ret

and ModelResetIdCrate =
    abstract Apply<'ret> : ModelResetIdEval<'ret> -> 'ret

and SwitchIdEval<'ret> =
    abstract Eval : unit -> 'ret

and SwitchIdCrate =
    abstract Apply<'ret> : SwitchIdEval<'ret> -> 'ret

and LazyIdEval<'ret> =
    abstract Eval : unit -> 'ret

and LazyIdCrate =
    abstract Apply<'ret> : LazyIdEval<'ret> -> 'ret

and AssocIdEval<'ret> =
    abstract Eval : Teq<'key, 'key> * ActionIdCrate -> 'ret

and AssocIdCrate =
    abstract Apply<'ret> : AssocIdEval<'ret> -> 'ret

and AssocOnIdEval<'ret> =
    abstract Eval : Teq<'ioKey, 'ioKey> * Teq<'modelKey, 'modelKey> * ActionIdCrate -> 'ret

and AssocOnIdCrate =
    abstract Apply<'ret> : AssocOnIdEval<'ret> -> 'ret

/// GADT for actions using crate pattern
and ActionEval<'ret> =
    abstract Eval<'a> : Action<'a> -> 'ret

and ActionCrate =
    abstract Apply<'ret> : ActionEval<'ret> -> 'ret

and Action<'a> =
    | LeafStatic of LeafStaticCrate
    | LeafDynamic of LeafDynamicCrate
    | SubFrom of SubFromCrate
    | SubInto of SubIntoCrate
    | WrapInner of WrapInnerCrate
    | WrapOuter of WrapOuterCrate
    | ModelResetInner of ModelResetInnerCrate
    | ModelResetOuter of ModelResetOuterCrate
    | Switch of SwitchCrate
    | Lazy of LazyCrate
    | Assoc of AssocCrate
    | AssocOn of AssocOnCrate

and LeafStaticEval<'ret> =
    abstract Eval : obj -> 'ret

and LeafStaticCrate =
    abstract Apply<'ret> : LeafStaticEval<'ret> -> 'ret

and LeafDynamicEval<'ret> =
    abstract Eval : obj -> 'ret

and LeafDynamicCrate =
    abstract Apply<'ret> : LeafDynamicEval<'ret> -> 'ret

and SubFromEval<'ret> =
    abstract Eval : ActionCrate -> 'ret

and SubFromCrate =
    abstract Apply<'ret> : SubFromEval<'ret> -> 'ret

and SubIntoEval<'ret> =
    abstract Eval : ActionCrate -> 'ret

and SubIntoCrate =
    abstract Apply<'ret> : SubIntoEval<'ret> -> 'ret

and WrapInnerEval<'ret> =
    abstract Eval : ActionCrate -> 'ret

and WrapInnerCrate =
    abstract Apply<'ret> : WrapInnerEval<'ret> -> 'ret

and WrapOuterEval<'ret> =
    abstract Eval : obj -> 'ret

and WrapOuterCrate =
    abstract Apply<'ret> : WrapOuterEval<'ret> -> 'ret

and ModelResetInnerEval<'ret> =
    abstract Eval : ActionCrate -> 'ret

and ModelResetInnerCrate =
    abstract Apply<'ret> : ModelResetInnerEval<'ret> -> 'ret

and ModelResetOuterEval<'ret> =
    abstract Eval : unit -> 'ret

and ModelResetOuterCrate =
    abstract Apply<'ret> : ModelResetOuterEval<'ret> -> 'ret

and SwitchEval<'ret> =
    abstract Eval : int * ActionCrate * ActionIdCrate -> 'ret

and SwitchCrate =
    abstract Apply<'ret> : SwitchEval<'ret> -> 'ret

and LazyEval<'ret> =
    abstract Eval : ActionCrate * ActionIdCrate -> 'ret

and LazyCrate =
    abstract Apply<'ret> : LazyEval<'ret> -> 'ret

and AssocEval<'ret> =
    abstract Eval : obj * ActionCrate * Teq<obj, obj> * (obj -> obj -> int) -> 'ret

and AssocCrate =
    abstract Apply<'ret> : AssocEval<'ret> -> 'ret

and AssocOnEval<'ret> =
    abstract Eval : obj * obj * ActionCrate * Teq<obj, obj> * (obj -> obj -> int) -> 'ret

and AssocOnCrate =
    abstract Apply<'ret> : AssocOnEval<'ret> -> 'ret

module Action =

    /// Module for creating ActionId crates
    module ActionIdCrate =
        let make<'a> (value : ActionId<'a>) : ActionIdCrate =
            { new ActionIdCrate with
                member _.Apply e = e.Eval value }

    /// Module for creating Action crates  
    module ActionCrate =
        let make<'a> (value : Action<'a>) : ActionCrate =
            { new ActionCrate with
                member _.Apply e = e.Eval value }

    /// Module for LeafIdCrate construction
    module LeafIdCrate =
        let make (action : Teq<'action, 'action>) : LeafIdCrate =
            { new LeafIdCrate with
                member _.Apply e = e.Eval action }

    /// Module for SubIdCrate construction
    module SubIdCrate =
        let make (fromId : ActionIdCrate) (intoId : ActionIdCrate) : SubIdCrate =
            { new SubIdCrate with
                member _.Apply e = e.Eval (fromId, intoId) }

    /// Module for WrapIdCrate construction
    module WrapIdCrate =
        let make (inner : ActionIdCrate) (outer : Teq<'outer, 'outer>) : WrapIdCrate =
            { new WrapIdCrate with
                member _.Apply e = e.Eval (inner, outer) }

    /// Module for ModelResetIdCrate construction
    module ModelResetIdCrate =
        let make (inner : ActionIdCrate) : ModelResetIdCrate =
            { new ModelResetIdCrate with
                member _.Apply e = e.Eval inner }

    /// Module for SwitchIdCrate construction
    module SwitchIdCrate =
        let make () : SwitchIdCrate =
            { new SwitchIdCrate with
                member _.Apply e = e.Eval () }

    /// Module for LazyIdCrate construction
    module LazyIdCrate =
        let make () : LazyIdCrate =
            { new LazyIdCrate with
                member _.Apply e = e.Eval () }

    /// Module for AssocIdCrate construction
    module AssocIdCrate =
        let make (key : Teq<'key, 'key>) (action : ActionIdCrate) : AssocIdCrate =
            { new AssocIdCrate with
                member _.Apply e = e.Eval (key, action) }

    /// Module for AssocOnIdCrate construction
    module AssocOnIdCrate =
        let make (ioKey : Teq<'ioKey, 'ioKey>) (modelKey : Teq<'modelKey, 'modelKey>) (action : ActionIdCrate) : AssocOnIdCrate =
            { new AssocOnIdCrate with
                member _.Apply e = e.Eval (ioKey, modelKey, action) }

    /// Module for LeafStaticCrate construction
    module LeafStaticCrate =
        let make (value : 'static') : LeafStaticCrate =
            { new LeafStaticCrate with
                member _.Apply e = e.Eval (box value) }

    /// Module for LeafDynamicCrate construction
    module LeafDynamicCrate =
        let make (value : 'dynamic) : LeafDynamicCrate =
            { new LeafDynamicCrate with
                member _.Apply e = e.Eval (box value) }

    /// Module for SubFromCrate construction
    module SubFromCrate =
        let make (action : ActionCrate) : SubFromCrate =
            { new SubFromCrate with
                member _.Apply e = e.Eval action }

    /// Module for SubIntoCrate construction
    module SubIntoCrate =
        let make (action : ActionCrate) : SubIntoCrate =
            { new SubIntoCrate with
                member _.Apply e = e.Eval action }

    /// Module for WrapInnerCrate construction
    module WrapInnerCrate =
        let make (action : ActionCrate) : WrapInnerCrate =
            { new WrapInnerCrate with
                member _.Apply e = e.Eval action }

    /// Module for WrapOuterCrate construction
    module WrapOuterCrate =
        let make (value : 'outer) : WrapOuterCrate =
            { new WrapOuterCrate with
                member _.Apply e = e.Eval (box value) }

    /// Module for ModelResetInnerCrate construction
    module ModelResetInnerCrate =
        let make (action : ActionCrate) : ModelResetInnerCrate =
            { new ModelResetInnerCrate with
                member _.Apply e = e.Eval action }

    /// Module for ModelResetOuterCrate construction
    module ModelResetOuterCrate =
        let make () : ModelResetOuterCrate =
            { new ModelResetOuterCrate with
                member _.Apply e = e.Eval () }

    /// Module for SwitchCrate construction
    module SwitchCrate =
        let make (branch : int) (action : ActionCrate) (typeId : ActionIdCrate) : SwitchCrate =
            { new SwitchCrate with
                member _.Apply e = e.Eval (branch, action, typeId) }

    /// Module for LazyCrate construction
    module LazyCrate =
        let make (action : ActionCrate) (typeId : ActionIdCrate) : LazyCrate =
            { new LazyCrate with
                member _.Apply e = e.Eval (action, typeId) }

    /// Module for AssocCrate construction
    module AssocCrate =
        let make (key : 'key) (action : ActionCrate) (id : Teq<'key, 'key>) (compare : 'key -> 'key -> int) : AssocCrate =
            { new AssocCrate with
                member _.Apply e = e.Eval (box key, action, External.unsafeCoerce id, fun x y -> compare (unbox x) (unbox y)) }

    /// Module for AssocOnCrate construction
    module AssocOnCrate =
        let make (ioKey : 'ioKey) (modelKey : 'modelKey) (action : ActionCrate) (ioId : Teq<'ioKey, 'ioKey>) (ioCompare : 'ioKey -> 'ioKey -> int) : AssocOnCrate =
            { new AssocOnCrate with
                member _.Apply e = e.Eval (box ioKey, box modelKey, action, External.unsafeCoerce ioId, fun x y -> ioCompare (unbox x) (unbox y)) }

    /// Type ID construction functions
    module TypeId =
        let nothingTypeId : Teq<obj, obj> = Teq.refl
        let nothing : ActionIdCrate = ActionIdCrate.make (LeafId (LeafIdCrate.make nothingTypeId))
        let leaf (typeId : Teq<'action, 'action>) : ActionIdCrate = ActionIdCrate.make (LeafId (LeafIdCrate.make (External.unsafeCoerce typeId)))
        let sub (from : ActionIdCrate) (into : ActionIdCrate) : ActionIdCrate = ActionIdCrate.make (SubId (SubIdCrate.make from into))
        let wrap (inner : ActionIdCrate) (outer : Teq<'outer, 'outer>) : ActionIdCrate = ActionIdCrate.make (WrapId (WrapIdCrate.make inner (External.unsafeCoerce outer)))
        let modelReset (inner : ActionIdCrate) : ActionIdCrate = ActionIdCrate.make (ModelResetId (ModelResetIdCrate.make inner))
        let lazy_ : ActionIdCrate = ActionIdCrate.make (LazyId (LazyIdCrate.make ()))
        let switch : ActionIdCrate = ActionIdCrate.make (SwitchId (SwitchIdCrate.make ()))
        let assoc (key : Teq<'key, 'key>) (action : ActionIdCrate) : ActionIdCrate = ActionIdCrate.make (AssocId (AssocIdCrate.make (External.unsafeCoerce key) action))
        let assocOn (ioKey : Teq<'ioKey, 'ioKey>) (modelKey : Teq<'modelKey, 'modelKey>) (action : ActionIdCrate) : ActionIdCrate = ActionIdCrate.make (AssocOnId (AssocOnIdCrate.make (External.unsafeCoerce ioKey) (External.unsafeCoerce modelKey) action))

    /// Action construction functions  
    let staticLeaf (action : 'static') : ActionCrate = ActionCrate.make (LeafStatic (LeafStaticCrate.make action))
    let dynamicLeaf (action : 'dynamic) : ActionCrate = ActionCrate.make (LeafDynamic (LeafDynamicCrate.make action))
    let subFrom (action : ActionCrate) : ActionCrate = ActionCrate.make (SubFrom (SubFromCrate.make action))
    let subInto (action : ActionCrate) : ActionCrate = ActionCrate.make (SubInto (SubIntoCrate.make action))
    let wrapInner (action : ActionCrate) : ActionCrate = ActionCrate.make (WrapInner (WrapInnerCrate.make action))
    let wrapOuter (action : 'outer) : ActionCrate = ActionCrate.make (WrapOuter (WrapOuterCrate.make action))
    let modelResetInner (action : ActionCrate) : ActionCrate = ActionCrate.make (ModelResetInner (ModelResetInnerCrate.make action))
    let modelResetOuter : ActionCrate = ActionCrate.make (ModelResetOuter (ModelResetOuterCrate.make ()))
    let switch (branch : int) (typeId : ActionIdCrate) (action : ActionCrate) : ActionCrate = ActionCrate.make (Switch (SwitchCrate.make branch action typeId))
    let lazy_ (typeId : ActionIdCrate) (action : ActionCrate) : ActionCrate = ActionCrate.make (Lazy (LazyCrate.make action typeId))
    let assoc (key : 'key) (id : Teq<'key, 'key>) (compare : 'key -> 'key -> int) (action : ActionCrate) : ActionCrate = ActionCrate.make (Assoc (AssocCrate.make key action id compare))
    let assocOn (ioKey : 'ioKey) (ioId : Teq<'ioKey, 'ioKey>) (ioCompare : 'ioKey -> 'ioKey -> int) (modelKey : 'modelKey) (action : ActionCrate) : ActionCrate = ActionCrate.make (AssocOn (AssocOnCrate.make ioKey modelKey action ioId ioCompare))