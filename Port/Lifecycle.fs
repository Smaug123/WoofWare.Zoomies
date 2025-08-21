namespace WoofWare.Zoomies.Port

/// Lifecycle management for component activation, deactivation, and after-display events
type Lifecycle = {
    OnActivate : ApplyActionContext.Effect<unit> option
    OnDeactivate : ApplyActionContext.Effect<unit> option  
    AfterDisplay : ApplyActionContext.Effect<unit> option
}

[<RequireQualifiedAccess>]
module Lifecycle =
    
    /// Collection of lifecycle events keyed by Path
    module Collection =
        type t = Path.Map.t<Lifecycle>
        
        let empty : t = Path.Map.empty
        
        let hasAfterDisplay (lifecycles : t) : bool =
            Path.Map.exists lifecycles (fun lifecycle -> Option.isSome lifecycle.AfterDisplay)
        
        /// Helper to add optional effect to accumulator list
        let private maybeCons (effect : ApplyActionContext.Effect<unit> option) (acc : ApplyActionContext.Effect<unit> list) =
            match effect with
            | Some eff -> eff :: acc
            | None -> acc
        
        /// Compute the diff between old and new lifecycle collections
        let diff (old : t) (new_ : t) : ApplyActionContext.Effect<unit> =
            // Collect after_display effects from new collection
            let afterDisplays =
                Path.Map.fold new_ [] (fun acc _ lifecycle ->
                    maybeCons lifecycle.AfterDisplay acc)
            
            // Collect activations and deactivations
            let activations, deactivations =
                let collectDiff acc (path, diffResult) =
                    let (activations, deactivations) = acc
                    match diffResult with
                    | Path.Map.DiffResult.Left lifecycle ->
                        // Component removed - add deactivation
                        activations, maybeCons lifecycle.OnDeactivate deactivations
                    | Path.Map.DiffResult.Right lifecycle ->
                        // Component added - add activation  
                        maybeCons lifecycle.OnActivate activations, deactivations
                    | Path.Map.DiffResult.Unequal _ ->
                        // Component changed but not handled for now
                        activations, deactivations
                        
                let dataEqual (a : Lifecycle) (b : Lifecycle) = System.Object.ReferenceEquals(a, b)
                Path.Map.foldSymmetricDiff old new_ dataEqual ([], []) collectDiff
            
            // Create a combined effect that runs all three phases in order:
            // 1. Deactivations (in reverse order)
            // 2. Activations (in reverse order) 
            // 3. After displays (in reverse order)
            let allEffects = 
                (List.rev deactivations) @ 
                (List.rev activations) @ 
                (List.rev afterDisplays)
            
            // Return a single effect that runs all collected effects
            fun () ->
                for effect in allEffects do
                    effect ()
        
        /// Merge two disjoint lifecycle collections
        let merge (a : t) (b : t) : t =
            // Simple merge - assumes disjoint keys
            Map.fold (fun acc key value -> Map.add key value acc) a b