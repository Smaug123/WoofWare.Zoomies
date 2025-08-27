namespace WoofWare.Zoomies.Port

open WoofWare.Incremental
open TypeEquality

/// Simplified Computation type representing Bonsai computations
type Computation<'result> =
    | Return of Value<'result>
    | Lazy' of Lazy<Computation<'result>>
    // TODO: Add more complex computation types as dependencies become available

[<RequireQualifiedAccess>]
module Computation =
    
    /// Create a computation that returns a constant value
    let return' (value : Value<'result>) : Computation<'result> =
        Return value
    
    /// Create a lazy computation
    let lazy' (computation : Lazy<Computation<'result>>) : Computation<'result> =
        Lazy' computation