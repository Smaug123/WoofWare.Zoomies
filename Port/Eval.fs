namespace WoofWare.Zoomies.Port

/// Core evaluation module for Bonsai computations - simplified placeholder
[<RequireQualifiedAccess>]
module Eval =
    
    let (>>>) f inject b = inject (f b)
    
    /// Basic gather function for evaluating computations - placeholder
    let gather (computation : 'result Computation) : 'result =
        // TODO: Implement proper evaluation logic
        failwith "Eval.gather not yet implemented"