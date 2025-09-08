#nowarn "3559"
namespace WoofWare.Zoomies.Port

open System.Collections.Generic

/// Driver for running Bonsai computations
[<RequireQualifiedAccess>]
module Driver =

    /// Internal state of a driver instance
    type private DriverState =
        {
            ModelVar : obj
            DefaultModel : obj
            Queue : obj
            mutable Result : obj option
            ApplyAction : obj -> obj -> obj
            GetResult : obj -> obj
        }

    /// Opaque driver type
    type T<'result> = private DriverT of obj

    /// Create a driver from a Cont-based computation
    let create (computation : Cont.Graph -> Value<'result>) : T<'result> =
        // Execute the computation to get the initial result
        let graph = { Cont.Graph.Transform = id }
        let resultValue = computation graph
        
        // Extract the result if it's a constant, otherwise use default
        let initialResult = 
            match resultValue.WithoutPosition with
            | ValueWithoutPosition.Constant value -> value
            | _ -> Unchecked.defaultof<'result>
        
        let state = 
            {
                ModelVar = box (Var.create (box ()))
                DefaultModel = box ()
                Queue = box (Queue<obj>())
                Result = Some (box initialResult)
                ApplyAction = fun model action -> model
                GetResult = fun model -> box initialResult
            }
        DriverT state

    /// Create a driver directly from a computation
    let createDirect (computation : Computation<'result>) : T<'result> =
        create (fun graph -> 
            Cont.Primitives.perform graph computation)

    /// Flush pending actions and update state
    let flush (DriverT state) : unit =
        let state = unbox<DriverState> state
        
        // Process all queued actions if we have a proper queue
        match state.Queue with
        | :? Queue<obj> as queue when queue.Count > 0 ->
            // For now, just clear the queue - in a real implementation
            // would apply actions to update state
            queue.Clear()
        | _ -> 
            // No actions to process
            ()

    /// Get the current result value
    let result (DriverT state) : 'result =
        let state = unbox<DriverState> state
        match state.Result with
        | Some result -> unbox result
        | None -> 
            // In real implementation, would compute from current model
            let currentModel = state.DefaultModel
            state.GetResult currentModel |> unbox

    /// Check if there are after-display events pending
    let hasAfterDisplayEvents (driver : T<'result>) : bool =
        // Simplified: no after-display events for now
        false

    /// Trigger lifecycle events
    let triggerLifecycles (driver : T<'result>) : unit =
        // Simplified: no lifecycle events for now  
        ()

    /// Expert-level operations
    module Expert =
        
        /// Reset model to default value
        let resetModelToDefault (DriverT state) : unit =
            let state = unbox<DriverState> state
            // In a real implementation, would reset the model variable
            // For now, just update the cached result
            state.Result <- None

        /// Enable action printing for debugging
        let printActions (driver : T<'result>) : unit =
            // Simplified: would enable action logging
            ()

        /// Get the incremental result value (simplified)
        let resultIncr (DriverT state) : obj =
            let state = unbox<DriverState> state
            // Simplified: return default result
            state.GetResult state.DefaultModel

    /// Testing utilities  
    module ForTesting =
        
        /// Create a simple driver for testing
        let createSimple (initialModel : 'model) (applyAction : 'model -> 'action -> 'model) (getResult : 'model -> 'result) : T<'result> =
            let state =
                {
                    ModelVar = box (Var.create (box initialModel))
                    DefaultModel = box initialModel
                    Queue = box (Queue<obj>())  // Use Queue<obj> for consistent type handling
                    Result = Some (box (getResult initialModel))
                    ApplyAction = fun model action -> box (applyAction (unbox model) (unbox action))
                    GetResult = fun model -> box (getResult (unbox model))
                }
            DriverT state
            
        /// Inject an action directly for testing
        let injectAction (DriverT state) (action : 'action) : unit =
            let state = unbox<DriverState> state
            match state.Queue with
            | :? Queue<obj> as queue ->
                queue.Enqueue(box action)
            | _ -> 
                // Can't enqueue to this type of queue
                ()