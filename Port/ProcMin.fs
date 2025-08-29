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