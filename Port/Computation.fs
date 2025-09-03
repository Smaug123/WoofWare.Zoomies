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

// Leaf1 crate (state machine with single input) - specialized to avoid casting
and Leaf1Eval<'ret> =
    abstract Eval<'model, 'dynamic_action, 'input> : Leaf1Data<'model, 'dynamic_action, 'input> -> 'ret
and Leaf1Crate =
    abstract Apply<'ret> : Leaf1Eval<'ret> -> 'ret
and Leaf1Data<'model, 'dynamic_action, 'input> = {
    Model : Model.Model<'model>
    InputId : MetaInput.Input<'input>
    DynamicAction : 'dynamic_action TypeId
    ApplyAction : ('dynamic_action -> Effect.Effect<unit>) -> (Effect.Effect<unit> -> unit) -> 'input option -> 'model -> 'dynamic_action -> 'model
    Reset : ('dynamic_action -> Effect.Effect<unit>) -> (Effect.Effect<unit> -> unit) -> 'model -> 'model
    Input : Value<'input>
}

// Leaf0 crate (state machine with no input) - specialized to avoid casting
and Leaf0Eval<'ret> =
    abstract Eval<'model, 'static_action> : Leaf0Data<'model, 'static_action> -> 'ret
and Leaf0Crate =
    abstract Apply<'ret> : Leaf0Eval<'ret> -> 'ret
and Leaf0Data<'model, 'static_action> = {
    Model : Model.Model<'model>
    StaticAction : 'static_action TypeId
    ApplyAction : ('static_action -> Effect.Effect<unit>) -> (Effect.Effect<unit> -> unit) -> 'model -> 'static_action -> 'model
    Reset : ('static_action -> Effect.Effect<unit>) -> (Effect.Effect<unit> -> unit) -> 'model -> 'model
}

// Leaf_incr crate (incremental computation)
and LeafIncrEval<'result, 'ret> =
    abstract Eval<'input> : LeafIncrData<'input, 'result> -> 'ret
and LeafIncrCrate<'result> =
    abstract Apply<'ret> : LeafIncrEval<'result, 'ret> -> 'ret
and LeafIncrData<'input, 'result> = {
    Input : Value<'input>
    Compute : WoofWare.Incremental.Incremental -> WoofWare.Incremental.Node<'input> -> WoofWare.Incremental.Node<'result>
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
    | Leaf1 of Leaf1Crate  // Specialized crate handles existential types internally
    | Leaf0 of Leaf0Crate  // Specialized crate handles existential types internally  
    | LeafIncr of LeafIncrCrate<'result>
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
    
    /// Create a Leaf1 state machine (single input)
    let leaf1<'model, 'dynamic_action, 'input> 
        (model : Model.Model<'model>) 
        (inputId : MetaInput.Input<'input>)
        (dynamicAction : 'dynamic_action TypeId)
        (applyAction : ('dynamic_action -> Effect.Effect<unit>) -> (Effect.Effect<unit> -> unit) -> 'input option -> 'model -> 'dynamic_action -> 'model)
        (reset : ('dynamic_action -> Effect.Effect<unit>) -> (Effect.Effect<unit> -> unit) -> 'model -> 'model)
        (input : Value<'input>) : Computation<'model * ('dynamic_action -> Effect.Effect<unit>)> =
        Leaf1 { new Leaf1Crate with
            member _.Apply eval = 
                eval.Eval<'model, 'dynamic_action, 'input> { 
                    Model = model
                    InputId = inputId
                    DynamicAction = dynamicAction
                    ApplyAction = applyAction
                    Reset = reset
                    Input = input
                } }
    
    /// Create a Leaf0 state machine (no input)
    let leaf0<'model, 'static_action> 
        (model : Model.Model<'model>) 
        (staticAction : 'static_action TypeId)
        (applyAction : ('static_action -> Effect.Effect<unit>) -> (Effect.Effect<unit> -> unit) -> 'model -> 'static_action -> 'model)
        (reset : ('static_action -> Effect.Effect<unit>) -> (Effect.Effect<unit> -> unit) -> 'model -> 'model) : Computation<'model * ('static_action -> Effect.Effect<unit>)> =
        Leaf0 { new Leaf0Crate with
            member _.Apply eval = 
                eval.Eval<'model, 'static_action> { 
                    Model = model
                    StaticAction = staticAction
                    ApplyAction = applyAction
                    Reset = reset
                } }
    
    /// Create a LeafIncr computation (incremental)
    let leafIncr<'input, 'result> 
        (input : Value<'input>) 
        (compute : WoofWare.Incremental.Incremental -> WoofWare.Incremental.Node<'input> -> WoofWare.Incremental.Node<'result>) : Computation<'result> =
        LeafIncr { new LeafIncrCrate<'result> with
            member _.Apply eval = eval.Eval<'input> { Input = input; Compute = compute } }