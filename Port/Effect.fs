namespace WoofWare.Zoomies.Port

/// Effect module - Side-effectful operations in Bonsai
/// Simplified port of Ui_effect for F# compatibility
[<RequireQualifiedAccess>]
module Effect =
    
    /// An effect that can be scheduled and executed
    type Effect<'a> =
        | Ignore
        | Many of Effect<unit> list
        | Return of 'a
        | Never
        | OfThunk of (unit -> 'a)
        | PrintS of string

    /// Type alias for convenience
    type t<'a> = Effect<'a>
    
    /// Ignore effect - does nothing
    let ignore () : Effect<unit> = Ignore
    
    /// Never-completing effect
    let never<'a> : Effect<'a> = Never
    
    /// Return a value as an effect  
    let return' (value : 'a) : Effect<'a> = Return value
    
    /// Map over an effect
    let map (effect : Effect<'a>) (f : 'a -> 'b) : Effect<'b> =
        match effect with
        | Return a -> Return (f a)
        | OfThunk thunk -> OfThunk (fun () -> f (thunk ()))
        | _ -> OfThunk (fun () -> Unchecked.defaultof<'b>)
    
    /// Bind for effects (monadic composition)
    let bind (effect : Effect<'a>) (f : 'a -> Effect<'b>) : Effect<'b> =
        match effect with
        | Return a -> f a
        | OfThunk thunk -> f (thunk ())
        | _ -> Never
    
    /// Combine multiple unit effects
    let many (effects : Effect<unit> list) : Effect<unit> = 
        Many effects
    
    /// Create an effect from a thunk
    let ofThunk (thunk : unit -> 'a) : Effect<'a> =
        OfThunk thunk
    
    /// Print a message as an effect
    let printS (message : string) : Effect<unit> =
        PrintS message
    
    /// From sync function
    let ofSyncFun (f : 'query -> 'result) (query : 'query) : Effect<'result> =
        OfThunk (fun () -> f query)
    
    /// Lazy effect construction
    let lazy_ (effect : Lazy<Effect<'a>>) : Effect<'a> =
        OfThunk (fun () -> 
            match effect.Value with
            | Return a -> a
            | OfThunk thunk -> thunk ()
            | _ -> Unchecked.defaultof<'a>)
    
    /// Monad-like operators
    module Operators =
        let (>>=) effect f = bind effect f
        let (>>|) effect f = map effect f

    /// Expert module for advanced effect operations
    module Expert =
        /// Evaluate an effect with a callback
        let rec eval (effect : Effect<'a>) (f : 'a -> unit) : unit =
            match effect with
            | Return value -> f value
            | Ignore -> f (Unchecked.defaultof<'a>)
            | OfThunk thunk -> f (thunk ())
            | PrintS msg -> 
                System.Console.WriteLine(msg)
                f (Unchecked.defaultof<'a>)
            | Never -> () // Never calls the callback
            | Many effects ->
                // Simplified handling of multiple effects
                f (Unchecked.defaultof<'a>)
        
        /// Handle a unit effect
        let handle (effect : Effect<unit>) : unit =
            eval effect (fun _ -> ())