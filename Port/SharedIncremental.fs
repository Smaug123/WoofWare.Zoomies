namespace WoofWare.Zoomies.Port

open WoofWare.Incremental

/// Shared incremental computation instance for the entire Bonsai system
/// This ensures all reactive computations participate in the same incremental graph
[<RequireQualifiedAccess>]
module SharedIncremental =
    
    /// The single shared incremental instance used throughout Bonsai
    /// All reactive computations should use this instance to ensure consistency
    let Instance : Incremental = Incremental.make ()