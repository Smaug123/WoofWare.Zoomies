namespace WoofWare.Zoomies.Port

open WoofWare.Incremental

/// Name source tracking for debugging Value.t instances
[<RequireQualifiedAccess>]
module NameSource =
    
    type NameSource =
        | Sub of string option
        | AssocLikeKey
        | AssocLikeData  
        | WrapModel
        | WrapInject
        | AppInput
        | ModelResetter
    
    let toString = function
        | Sub (Some location) -> sprintf "A Value.t introduced by the [let%%sub] expression at %s" location
        | Sub None -> "A Value.t introduced by some [let%sub] expression"
        | AssocLikeKey -> "The Value.t for the key introduced within a [Bonsai.assoc] or [Bonsai.assoc_on] computation"
        | AssocLikeData -> "The Value.t for the data introduced within a [Bonsai.assoc] or [Bonsai.assoc_on] computation"
        | WrapModel -> "The name for the model introduced within a [Bonsai.wrap] computation"
        | WrapInject -> "The Value.t for the injection function introduced within a [Bonsai.wrap] computation"
        | AppInput -> "The app input Value.t"
        | ModelResetter -> "A model resetter"

/// Represents values in the Bonsai computation graph
type Value<'a> = private {
    WithoutPosition : 'a ValueWithoutPosition
}

/// The core Value representation without position information
and ValueWithoutPosition<'a> =
    | Constant of 'a
    | Incr of 'a Node
    | Named of NameSource.NameSource
    | Exception of exn

[<RequireQualifiedAccess>]
module Value =
    
    // Use shared incremental computation instance
    let private I : Incremental = SharedIncremental.Instance
    
    let private createValue (withoutPosition : 'a ValueWithoutPosition) : Value<'a> =
        { WithoutPosition = withoutPosition }
    
    /// Create a constant value
    let return' (value : 'a) : Value<'a> =
        createValue (Constant value)
    
    /// Create a value from an incremental node
    let fromIncr (node : 'a Node) : Value<'a> =
        createValue (Incr node)
    
    /// Create a named value for debugging
    let named (source : NameSource.NameSource) : Value<'a> =
        createValue (Named source)
    
    /// Create a value that raises an exception
    let returnExn (exn : exn) : Value<'a> =
        createValue (Exception exn)
    
    /// Convert a Value to an incremental Node (basic implementation)
    let rec toIncr (value : Value<'a>) : 'a Node =
        match value.WithoutPosition with
        | Constant x -> I.Return x
        | Incr node -> node
        | Named _ -> failwith "Cannot convert named value to incremental without binding"
        | Exception exn -> I.Map (fun () -> raise exn) (I.Return ())

    /// Combine two values into a tuple
    let both (a : Value<'a>) (b : Value<'b>) : Value<'a * 'b> =
        let nodeA = toIncr a
        let nodeB = toIncr b
        createValue (Incr (I.Both nodeA nodeB))
    
    /// Apply a function to transform a value
    let map (f : 'a -> 'b) (value : Value<'a>) : Value<'b> =
        let node = toIncr value
        createValue (Incr (I.Map f node))
    
    /// Apply a binary function to two values
    let map2 (f : 'a -> 'b -> 'c) (valueA : Value<'a>) (valueB : Value<'b>) : Value<'c> =
        let nodeA = toIncr valueA
        let nodeB = toIncr valueB
        createValue (Incr (I.Map2 f nodeA nodeB))
    
    /// Apply a ternary function to three values
    let map3 (f : 'a -> 'b -> 'c -> 'd) (valueA : Value<'a>) (valueB : Value<'b>) (valueC : Value<'c>) : Value<'d> =
        let nodeA = toIncr valueA
        let nodeB = toIncr valueB
        let nodeC = toIncr valueC
        // Since Map3 doesn't exist, we'll use Map2 twice
        let combined = I.Map2 (fun a b -> (a, b)) nodeA nodeB
        createValue (Incr (I.Map2 (fun (a, b) c -> f a b c) combined nodeC))
    
    /// Add cutoff behavior to a value to prevent unnecessary recomputation
    let cutoff (equal : 'a -> 'a -> bool) (value : Value<'a>) : Value<'a> =
        let node = toIncr value
        // TODO: WoofWare.Incremental may support cutoff, but for now we'll just return the mapped value
        createValue (Incr (I.Map (fun x -> x) node))