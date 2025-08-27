namespace WoofWare.Zoomies.Port

open WoofWare.Incremental
open TypeEquality

/// Crate Pattern implementations for Computation GADT

// Sub crate (sub-computation)
type SubEval<'result, 'ret> =
    abstract Eval<'via> : SubData<'via, 'result> -> 'ret
and SubCrate<'result> =
    abstract Apply<'ret> : SubEval<'result, 'ret> -> 'ret
and SubData<'via, 'result> = {
    From : Computation<'via>
    Via : 'via TypeId
    Into : Computation<'result>
    Here : string option
}

// Store crate (store value in dynamic scope)
and StoreEval<'result, 'ret> =
    abstract Eval<'x> : StoreData<'x, 'result> -> 'ret
and StoreCrate<'result> =
    abstract Apply<'ret> : StoreEval<'result, 'ret> -> 'ret
and StoreData<'x, 'result> = {
    Id : 'x TypeId
    Value : Value<'x>
    Inner : Computation<'result>
}

// Fetch crate (fetch value from dynamic scope)
and FetchEval<'result, 'ret> =
    abstract Eval<'a> : FetchData<'a, 'result> -> 'ret
and FetchCrate<'result> =
    abstract Apply<'ret> : FetchEval<'result, 'ret> -> 'ret
and FetchData<'a, 'result> = {
    Id : 'a TypeId
    Default : 'result
    ForSome : 'a -> 'result
}

// ComputationSwitch crate (pattern matching on integers)
and ComputationSwitchEval<'result, 'ret> =
    abstract Eval : ComputationSwitchData<'result> -> 'ret
and ComputationSwitchCrate<'result> =
    abstract Apply<'ret> : ComputationSwitchEval<'result, 'ret> -> 'ret
and ComputationSwitchData<'result> = {
    Match : Value<int>
    Arms : Map<int, Computation<'result>>
    Here : string
}

/// Computation type representing Bonsai computations using Crate Pattern
and Computation<'result> =
    | Return of Value<'result>
    | Sub of SubCrate<'result>
    | Store of StoreCrate<'result>
    | Fetch of FetchCrate<'result>
    | Switch of ComputationSwitchCrate<'result>
    | Lazy' of Lazy<Computation<'result>>

[<RequireQualifiedAccess>]
module Computation =
    
    /// Create a computation that returns a constant value
    let return' (value : Value<'result>) : Computation<'result> =
        Return value
    
    /// Create a lazy computation
    let lazy' (computation : Lazy<Computation<'result>>) : Computation<'result> =
        Lazy' computation
    
    /// Create a sub-computation
    let sub (from : Computation<'via>) (via : 'via TypeId) (into : Computation<'result>) (here : string option) : Computation<'result> =
        Sub { new SubCrate<'result> with
            member _.Apply eval = eval.Eval<'via> { From = from; Via = via; Into = into; Here = here } }
    
    /// Store a value in dynamic scope
    let store (id : 'x TypeId) (value : Value<'x>) (inner : Computation<'result>) : Computation<'result> =
        Store { new StoreCrate<'result> with
            member _.Apply eval = eval.Eval<'x> { Id = id; Value = value; Inner = inner } }
    
    /// Fetch a value from dynamic scope
    let fetch (id : 'a TypeId) (defaultValue : 'result) (forSome : 'a -> 'result) : Computation<'result> =
        Fetch { new FetchCrate<'result> with
            member _.Apply eval = eval.Eval<'a> { Id = id; Default = defaultValue; ForSome = forSome } }
    
    /// Create a switch computation
    let switch (matchValue : Value<int>) (arms : Map<int, Computation<'result>>) (here : string) : Computation<'result> =
        Switch { new ComputationSwitchCrate<'result> with
            member _.Apply eval = eval.Eval { Match = matchValue; Arms = arms; Here = here } }