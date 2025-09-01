namespace WoofWare.Zoomies.Port

open TypeEquality

/// Minimal Proc interface - core computation construction functions
[<RequireQualifiedAccess>]
module ProcMin =
    
    /// Read a value into a computation
    let read (value : Value<'a>) : Computation<'a> =
        Computation.return' value
    
    /// Create a sub-computation
    let sub (from : Computation<'via>) (f : Value<'via> -> Computation<'result>) : Computation<'result> =
        match from with
        | Computation.Return value -> f value
        | _ -> 
            let via : 'via TypeId = TypeId.create "sub"
            let namedValue = Value.named (NameSource.Sub None)
            let into = f namedValue
            Computation.sub from via into None
    
    /// Dynamic scope operations
    module DynamicScope =
        
        /// Fetch a value from dynamic scope
        let fetch (id : 'a TypeId) (defaultValue : 'result) (forSome : 'a -> 'result) : Computation<'result> =
            Computation.fetch id defaultValue forSome
        
        /// Store a value in dynamic scope
        let store (id : 'a TypeId) (value : Value<'a>) (inner : Computation<'result>) : Computation<'result> =
            Computation.store id value inner
    
    /// Edge operations for lifecycle management
    module Edge =
        
        /// Add lifecycle to a computation
        let lifecycle (computation : Computation<'a>) : Computation<'a> =
            // For now, lifecycle is handled at the evaluation level
            // The computation itself doesn't change, but the runtime will handle lifecycle events
            computation
    
    /// Switch between computations based on integer matching
    let switch (matchValue : Value<int>) (arms : Map<int, Computation<'result>>) (here : string option) : Computation<'result> =
        Computation.switch matchValue arms (here |> Option.defaultValue "switch")
    
    /// State machine with one input
    let stateMachine1 (defaultModel : 'model) (applyAction : ApplyActionContext.ApplyActionContext<'input option> -> 'input option -> 'model -> 'action -> 'model) (input : Value<'input>) : Computation<'model * ('action -> unit Effect.Effect)> =
        // For now, return a simplified version - this needs proper state machine implementation
        let modelValue = Value.return' (defaultModel, fun _ -> Effect.ignore ())
        Computation.return' modelValue
    
    /// State machine with no input
    let stateMachine0 (defaultModel : 'model) (applyAction : ApplyActionContext.ApplyActionContext<unit> -> unit -> 'model -> 'action -> 'model) : Computation<'model * ('action -> unit Effect.Effect)> =
        // For now, return a simplified version - this needs proper state machine implementation
        let modelValue = Value.return' (defaultModel, fun _ -> Effect.ignore ())
        Computation.return' modelValue
    
    /// Associate over a map structure
    let assoc (map : Value<Map<'key, 'data>>) (f : Value<'key> -> Value<'data> -> Computation<'result>) : Computation<Map<'key, 'result>> =
        // Simplified implementation - needs proper association logic
        let emptyResult = Value.return' Map.empty
        Computation.return' emptyResult
    
    /// Lazy computation wrapper
    let lazy_ (computation : Lazy<Computation<'a>>) : Computation<'a> =
        Computation.lazy' computation
    
    /// Model resetter functionality
    let withModelResetter (f : Value<unit -> unit Effect.Effect> -> Computation<'a>) : Computation<'a> =
        let resetValue = Value.return' (fun () -> Effect.ignore ())
        f resetValue