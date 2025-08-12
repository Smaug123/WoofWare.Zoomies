namespace WoofWare.Zoomies.Port

// This module provides types for static analysis to track whether a computation
// may contain certain features like lifecycle hooks or path dependencies
module MayContain =

    // Base type for tracking whether something may or may not contain a feature
    [<RequireQualifiedAccess>]
    type MayContain =
        | YesOrMaybe
        | No

    module MayContain =
        /// Merge two MayContain values - if either is YesOrMaybe, the result is YesOrMaybe
        let merge (a: MayContain) (b: MayContain) =
            match a, b with
            | MayContain.YesOrMaybe, _ 
            | _, MayContain.YesOrMaybe -> MayContain.YesOrMaybe
            | MayContain.No, MayContain.No -> MayContain.No

    // Specific instances for lifecycle and path tracking
    module Lifecycle =
        type T = MayContain

        let merge = MayContain.merge

    module Path =  
        type T = MayContain

        let merge = MayContain.merge