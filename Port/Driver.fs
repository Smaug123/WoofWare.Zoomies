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
        // Simple implementation: create with unit model and dummy result
        let dummyState = 
            {
                ModelVar = box (Var.create ())
                DefaultModel = box ()
                Queue = box (Queue<unit>())
                Result = None
                ApplyAction = fun model action -> model
                GetResult = fun model -> box (Unchecked.defaultof<'result>)
            }
        DriverT dummyState

    /// Create a driver directly from a computation
    let createDirect (computation : Computation<'result>) : T<'result> =
        create (fun graph -> 
            Cont.Primitives.perform graph computation)

    /// Flush pending actions and update state
    let flush (DriverT state) : unit =
        let state = unbox<DriverState> state
        // Process all queued actions - simplified for testing
        // In real implementation, would properly handle queue
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
            // Simplified: in real implementation would reset model var
            ()

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
                    ModelVar = box (Var.create initialModel)
                    DefaultModel = box initialModel
                    Queue = box (Queue<'action>())
                    Result = None
                    ApplyAction = fun model action -> box (applyAction (unbox model) (unbox action))
                    GetResult = fun model -> box (getResult (unbox model))
                }
            DriverT state
            
        /// Inject an action directly for testing
        let injectAction (DriverT state) (action : 'action) : unit =
            // Simplified: in real implementation would enqueue action
            ()