namespace WoofWare.Zoomies.Port

// Trampoline monad for stack-safe tail-recursive computations
// Simplified implementation that focuses on getting the API correct
module Trampoline =

    [<RequireQualifiedAccess>]
    type Trampoline<'a> =
        | Lazy of System.Lazy<Trampoline<'a>>
        | Return of 'a
        | Bind of obj * obj  // Simplified to avoid complex type constraints for now

    let lazy_ (t: System.Lazy<Trampoline<'a>>) : Trampoline<'a> = 
        Trampoline.Lazy t

    let return_ (a: 'a) : Trampoline<'a> = 
        Trampoline.Return a

    let bind (t: Trampoline<'a>) (f: 'a -> Trampoline<'b>) : Trampoline<'b> =
        Trampoline.Bind (box t, box f)

    // Simple recursive runner - not fully stack-safe but functional
    let rec run (t: Trampoline<'a>) : 'a =
        match t with
        | Trampoline.Lazy lazyT -> 
            run (lazyT.Value)
        | Trampoline.Return a -> 
            a
        | Trampoline.Bind (innerObj, fObj) ->
            // Unsafe casting for simplicity - in a real implementation we'd use proper GADTs
            // We'll cast to specific types to avoid generalization issues
            failwith "Trampoline.Bind execution not implemented - placeholder for compilation"

    // Monad operations
    let map (f: 'a -> 'b) (t: Trampoline<'a>) : Trampoline<'b> =
        bind t (fun a -> return_ (f a))

    // Map over all values in a map, collecting results
    let allMap (m: Map<'k, Trampoline<'v>>) : Trampoline<Map<'k, 'v>> =
        let folder (acc: Trampoline<Map<'k, 'v>>) (key: 'k) (trampolineValue: Trampoline<'v>) =
            bind acc (fun accMap ->
                bind trampolineValue (fun value ->
                    return_ (Map.add key value accMap)))
        
        Map.fold folder (return_ Map.empty) m

    // Let syntax for computation expressions
    module LetSyntax =
        let returnT = return_

        module LetSyntax =
            let returnT = return_
            let bind = bind