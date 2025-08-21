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
    
    // Create incremental computation instance
    module private ValueIncrInstance =
        let I : Incremental = Incremental.make ()
    
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
    let toIncr (value : Value<'a>) : 'a Node =
        let rec loop (value : Value<'a>) : 'a Node =
            match value.WithoutPosition with
            | Constant x -> ValueIncrInstance.I.Return x
            | Incr node -> node
            | Named _ -> failwith "Cannot convert named value to incremental without binding"
            | Exception exn -> ValueIncrInstance.I.Map (fun () -> raise exn) (ValueIncrInstance.I.Return ())
        loop value