namespace WoofWare.Zoomies.Port

#nowarn "3559" // Type implicitly inferred as 'obj'

/// FixTransform module for fixed-point computation graph transformations
/// This handles recursive transformation with accumulator threading and up/down value passing
[<RequireQualifiedAccess>]
module FixTransform =
    
    /// Up values are passed from child to parent during transformation
    type Up<'t> = {
        Combine : 't -> 't -> 't
        Empty : 't
        EmptyForLazy : 't
    }
    
    /// Transformation behavior choice
    type TransformBehavior = DirectlyOn | SkippingOver
    
    /// User transformation interface (using obj for generic existentials)
    type Transform<'down, 'acc, 'up> = {
        TransformC : 'down -> 'acc -> obj -> ('acc * 'up * obj) Trampoline
        TransformV : 'down -> 'acc -> obj -> 'acc * 'up * obj
    }
    
    /// Unit implementation of Up for simple cases
    module Unit =
        let up : Up<unit> = {
            Combine = fun _ _ -> ()
            Empty = ()
            EmptyForLazy = ()
        }
    
    /// Create a simple pass-through transformation
    let createPassThrough<'down, 'acc, 'up> (up : Up<'up>) : Transform<'down, 'acc, 'up> = {
        TransformC = fun down acc computation ->
            trampoline {
                // Simple pass-through transformation for now
                return (acc, up.Empty, computation)
            }
        TransformV = fun down acc value ->
            // Simple pass-through transformation for now  
            (acc, up.Empty, value)
    }
    
    /// Apply a transformation to a computation 
    let transformComputation<'down, 'acc, 'up> 
        (transformer : Transform<'down, 'acc, 'up>)
        (down : 'down) 
        (acc : 'acc) 
        (computation : obj) : ('acc * 'up * obj) Trampoline =
        transformer.TransformC down acc computation
    
    /// Apply a transformation to a value
    let transformValue<'down, 'acc, 'up>
        (transformer : Transform<'down, 'acc, 'up>)
        (down : 'down)
        (acc : 'acc) 
        (value : obj) : 'acc * 'up * obj =
        transformer.TransformV down acc value
    
    /// Identity transformation for unit types
    let identityTransform () : Transform<unit, unit, unit> =
        createPassThrough Unit.up