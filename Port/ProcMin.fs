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
    let stateMachine1<'model, 'input, 'action when 'model : equality> (defaultModel : 'model) (applyAction : ApplyActionContext.ApplyActionContext<'action> -> 'input option -> 'model -> 'action -> 'model) (input : Value<'input>) : Computation<'model * ('action -> Effect.Effect<unit>)> =
        let name = "state_machine1"
        let modelInfo = Model.ofModule string (Some (=)) defaultModel name
        let inputId = MetaInput.create<'input> ()
        let dynamicActionId = TypeId.create<'action> (sprintf "%s-action" name)
        
        let applyActionImpl (inject : 'action -> Effect.Effect<unit>) (scheduleEvent : Effect.Effect<unit> -> unit) (inputOpt : 'input option) (model : 'model) (action : 'action) : 'model =
            // Convert Effect.Effect<unit> to ApplyActionContext.Effect<unit> and vice versa
            let applyActionInject (action : 'action) : ApplyActionContext.Effect<unit> = 
                fun () -> 
                    let effect = inject action
                    scheduleEvent effect
            let applyActionSchedule (effect : ApplyActionContext.Effect<unit>) : unit = 
                let unitEffect = Effect.ofThunk effect
                scheduleEvent unitEffect
            let context = ApplyActionContext.create applyActionInject applyActionSchedule
            applyAction context inputOpt model action
        
        let resetImpl (inject : 'action -> Effect.Effect<unit>) (scheduleEvent : Effect.Effect<unit> -> unit) (model : 'model) : 'model =
            defaultModel
        
        Computation.leaf1 modelInfo inputId dynamicActionId applyActionImpl resetImpl input
    
    /// State machine with no input
    let stateMachine0<'model, 'action when 'model : equality> (defaultModel : 'model) (applyAction : ApplyActionContext.ApplyActionContext<'action> -> unit -> 'model -> 'action -> 'model) : Computation<'model * ('action -> Effect.Effect<unit>)> =
        let name = "state_machine0"
        let modelInfo = Model.ofModule string (Some (=)) defaultModel name
        let staticActionId = TypeId.create<'action> (sprintf "%s-action" name)
        
        let applyActionImpl (inject : 'action -> Effect.Effect<unit>) (scheduleEvent : Effect.Effect<unit> -> unit) (model : 'model) (action : 'action) : 'model =
            // Convert Effect.Effect<unit> to ApplyActionContext.Effect<unit> and vice versa
            let applyActionInject (action : 'action) : ApplyActionContext.Effect<unit> = 
                fun () -> 
                    let effect = inject action
                    scheduleEvent effect
            let applyActionSchedule (effect : ApplyActionContext.Effect<unit>) : unit = 
                let unitEffect = Effect.ofThunk effect
                scheduleEvent unitEffect
            let context = ApplyActionContext.create applyActionInject applyActionSchedule
            applyAction context () model action
        
        let resetImpl (inject : 'action -> Effect.Effect<unit>) (scheduleEvent : Effect.Effect<unit> -> unit) (model : 'model) : 'model =
            defaultModel
        
        Computation.leaf0 modelInfo staticActionId applyActionImpl resetImpl
    
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