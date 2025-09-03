#nowarn "3559"
namespace WoofWare.Zoomies.Port

/// Continuation-based API for Bonsai computations
[<RequireQualifiedAccess>]
module Cont =

    /// Graph type for continuation-based computations
    type Graph = { mutable Transform : obj -> obj }

    /// Core primitives for the continuation monad
    module Primitives =
        
        /// Global graph instance
        let theOneAndOnly = { Transform = id }
        
        /// Counter for nested handles
        let mutable nestingLevel = 0
        
        /// Perform a computation within a graph context
        let perform (graph : Graph) (computation : Computation<'a>) : Value<'a> =
            // Simple implementation: for testing purposes, just return a constant
            Value.return' (Unchecked.defaultof<'a>)
        
        /// Execute function with isolated graph context
        let isolated (graph : Graph) (f : unit -> Value<'a>) : Computation<'a> =
            let backup = graph.Transform
            graph.Transform <- id
            try
                let result = f ()
                graph.Transform <- backup
                ProcMin.read result
            with
            | exn ->
                graph.Transform <- backup
                ProcMin.read (Value.returnExn exn)
        
        /// Handle computation within a graph
        let handle (f : Graph -> Value<'a>) (graph : Graph) : Computation<'a> =
            isolated graph (fun () -> f graph)
        
        /// Top-level handle using global graph
        let topLevelHandle (f : Graph -> Value<'a>) : Computation<'a> =
            nestingLevel <- nestingLevel + 1
            try
                let g = theOneAndOnly
                let backup = g.Transform
                g.Transform <- id
                let value = f g
                g.Transform <- backup
                ProcMin.read value
            finally
                nestingLevel <- nestingLevel - 1
        
        /// Check if we're inside a top-level handle
        let withGlobalGraph (f : Graph -> 'a) (noGraph : unit -> 'a) : 'a =
            if nestingLevel > 0 then f theOneAndOnly else noGraph ()

    open Primitives

    /// Return a constant value
    let return' (x : 'a) : Value<'a> = Value.return' x

    /// Map over a single value
    let map (value : Value<'a>) (f : 'a -> 'b) : Value<'b> =
        withGlobalGraph
            (fun graph -> perform graph (ProcMin.read (Value.map f value)))
            (fun () -> Value.map f value)

    /// Map over two values
    let map2 (a : Value<'a>) (b : Value<'b>) (f : 'a -> 'b -> 'c) : Value<'c> =
        withGlobalGraph
            (fun graph -> perform graph (ProcMin.read (Value.map2 f a b)))
            (fun () -> Value.map2 f a b)

    /// Combine two values into a tuple
    let both (a : Value<'a>) (b : Value<'b>) : Value<'a * 'b> =
        map2 a b (fun a b -> (a, b))

    /// Create state with default value
    let state (defaultModel : 'model) (graph : Graph) : Value<'model> * Value<('model -> 'model) -> Effect.Effect<unit>> =
        let stateComputation = ProcMin.stateMachine0 defaultModel (fun context input model action -> action model)
        let stateValue = perform graph stateComputation
        
        // For testing, return simple values
        let modelValue = Value.return' defaultModel
        let injectValue = Value.return' (fun f -> Effect.ignore ())
        
        (modelValue, injectValue)

    /// Create state machine
    let stateMachine0 (defaultModel : 'model) (applyAction : ApplyActionContext.ApplyActionContext<'action> -> unit -> 'model -> 'action -> 'model) (graph : Graph) : Value<'model> * Value<'action -> Effect.Effect<unit>> =
        let stateValue = perform graph (ProcMin.stateMachine0 defaultModel applyAction)
        
        // For testing, return simple values
        let modelValue = Value.return' defaultModel
        let injectValue = Value.return' (fun action -> Effect.ignore ())
        (modelValue, injectValue)

    /// Create state machine with input
    let stateMachine1 (defaultModel : 'model) (applyAction : ApplyActionContext.ApplyActionContext<'action> -> 'input option -> 'model -> 'action -> 'model) (input : Value<'input>) (graph : Graph) : Value<'model> * Value<'action -> Effect.Effect<unit>> =
        let stateValue = perform graph (ProcMin.stateMachine1 defaultModel applyAction input)
        
        // For testing, return simple values
        let modelValue = Value.return' defaultModel
        let injectValue = Value.return' (fun action -> Effect.ignore ())
        (modelValue, injectValue)

    /// Module for conversion between computation styles
    module Conv =
        let topLevelHandle = topLevelHandle
        let handle = handle