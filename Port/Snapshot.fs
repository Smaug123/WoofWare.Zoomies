namespace WoofWare.Zoomies.Port

open WoofWare.Incremental

/// Snapshot represents the state of a computation at a particular point in time
type Snapshot<'model, 'input, 'result> = {
    Input : Input<'input>
    Lifecycle : Lifecycle.Collection.t Node option
    Result : 'result Node
}

[<RequireQualifiedAccess>]
module Snapshot =
    
    // Use shared incremental computation instance
    let private I : Incremental = SharedIncremental.Instance
    
    /// Create a new snapshot with the given components
    let create (input : Input<'input>) (lifecycle : Lifecycle.Collection.t Node option) (result : 'result Node) : Snapshot<'model, 'input, 'result> =
        // TODO: Add input iteration and annotation when available
        // Input.iterIncremental input (fun x -> annotate_packed Input x)
        
        // TODO: Add lifecycle annotation when available  
        // Option.iter lifecycle (annotate Lifecycle)
        
        // TODO: Add result annotation when available
        // annotate Result result
        
        { 
            Input = input
            Lifecycle = lifecycle
            Result = result
        }
    
    /// Get the lifecycle collection, or empty if none
    let lifecycleOrEmpty (snapshot : Snapshot<'model, 'input, 'result>) : Lifecycle.Collection.t Node =
        match snapshot.Lifecycle with
        | None ->
            let emptyLifecycle = I.Return Lifecycle.Collection.empty
            // TODO: Add annotation when available
            // annotate Empty_lifecycle emptyLifecycle
            emptyLifecycle
        | Some lifecycle -> lifecycle
    
    /// Attribute positions for debugging (placeholder implementation)
    let attributePositions (here : string) (snapshot : Snapshot<'model, 'input, 'result>) : unit =
        // TODO: Implement position attribution when debugging infrastructure is available
        // Input.iterIncremental snapshot.Input (fun x -> attribute_packed here x)
        // Option.iter snapshot.Lifecycle (attribute here) 
        // attribute here snapshot.Result
        ()
        
    /// Get the input from the snapshot
    let input (snapshot : Snapshot<'model, 'input, 'result>) : Input<'input> =
        snapshot.Input
        
    /// Get the lifecycle from the snapshot  
    let lifecycle (snapshot : Snapshot<'model, 'input, 'result>) : Lifecycle.Collection.t Node option =
        snapshot.Lifecycle
        
    /// Get the result from the snapshot
    let result (snapshot : Snapshot<'model, 'input, 'result>) : 'result Node =
        snapshot.Result