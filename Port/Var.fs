namespace WoofWare.Zoomies.Port

open WoofWare.Incremental

/// Variable type - a mutable incremental variable from WoofWare.Incremental
type Var<'a> = WoofWare.Incremental.Var<'a>

[<RequireQualifiedAccess>]
module Var =
    
    // Create incremental computation instance
    module private VarIncrInstance =
        let I : Incremental = Incremental.make ()
    
    /// Create a new variable with the given initial value
    let create (initial : 'a) : Var<'a> =
        VarIncrInstance.I.Var.Create initial
    
    /// Set the value of the variable
    let set (var : Var<'a>) (value : 'a) : unit =
        // Mark incremental as dirty to trigger stabilization
        StabilizationTracker.markIncrementalDirty ()
        
        // Check if we're currently stabilizing - this would be an error
        if VarIncrInstance.I.AmStabilizing then
            failwith "Bonsai.Var mutated during the computation of a Bonsai value"
        
        VarIncrInstance.I.Var.Set var value
    
    /// Update the variable using a function
    let update (var : Var<'a>) (f : 'a -> 'a) : unit =
        let oldValue = VarIncrInstance.I.Var.Value var
        set var (f oldValue)
    
    /// Get the current value of the variable
    let get (var : Var<'a>) : 'a =
        VarIncrInstance.I.Var.Value var
    
    /// Get a Value that tracks this variable
    let value (var : Var<'a>) : Value<'a> =
        let watchNode = VarIncrInstance.I.Var.Watch var
        Value.fromIncr watchNode
    
    /// Get the underlying incremental variable (identity function)
    let incrVar (var : Var<'a>) : Var<'a> = var