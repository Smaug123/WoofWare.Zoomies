namespace WoofWare.Zoomies.Port

open WoofWare.Incremental
open WoofWare.Zoomies.Port.External

/// A crate for the Join case that hides the intermediate types
type InputJoinEval<'input, 'ret> =
    abstract Eval<'a> : 'a Node * ('a -> 'input) -> 'ret

type InputJoinCrate<'input> =
    abstract Apply<'ret> : InputJoinEval<'input, 'ret> -> 'ret

[<RequireQualifiedAccess>]
module InputJoinCrate =
    let make<'a, 'input> (node : 'a Node) (f : 'a -> 'input) : InputJoinCrate<'input> =
        { new InputJoinCrate<'input> with
            member _.Apply eval = eval.Eval (node, f) }

/// This type represents a combination of static and dynamic data. When only one
/// is present, the representation is straightforward, but when both are
/// present, we use the Join constructor. Join contains the dynamic portion
/// as a single incremental node (which probably contains a tuple-tree built up
/// with several both nodes), and the static portion is contained implicitly
/// within the function, call it f. f rearranges the 'a by inserting all
/// the pieces of static data into their appropriates places in the tuple tree.
type Input<'input> =
    | Dynamic of 'input Node
    | Static of 'input
    | Join of InputJoinCrate<'input>

[<RequireQualifiedAccess>]
module Input =
    
    // Use shared incremental computation instance for Input module
    let private I : Incremental = SharedIncremental.Instance
    
    let dynamic (input : 'input Node) : Input<'input> = Dynamic input
    
    let staticValue : Input<unit> = Static ()
    
    let map (t : Input<'a>) (f : 'a -> 'b) : Input<'b> =
        match t with
        | Dynamic input -> Dynamic (I.Map f input)
        | Static input -> Static (f input)
        | Join joinCrate ->
            joinCrate.Apply { new InputJoinEval<'a, Input<'b>> with
                member _.Eval (input, g) = Join (InputJoinCrate.make input (fun x -> f (g x))) }
    
    let iterIncremental (t : Input<'a>) (f : NodeCrate -> unit) : unit =
        match t with
        | Dynamic incr -> f (NodeCrate.make incr)
        | Static _ -> ()
        | Join joinCrate ->
            joinCrate.Apply { new InputJoinEval<'a, FakeUnit> with
                member _.Eval (incr, _) = f (NodeCrate.make incr); FakeUnit }
            |> ignore
    
    let toIncremental (input : Input<'a>) : 'a Node =
        match input with
        | Dynamic input -> input
        | Static input -> I.Return input
        | Join joinCrate ->
            joinCrate.Apply { new InputJoinEval<'a, 'a Node> with
                member _.Eval (incr, f) = I.Map f incr }
    
    let merge (a : Input<'a>) (b : Input<'b>) : Input<'a * 'b> =
        match a, b with
        | Dynamic a, Dynamic b -> Dynamic (I.Both a b)
        | Dynamic a, Static b -> Join (InputJoinCrate.make a (fun a -> a, b))
        | Static a, Dynamic b -> Join (InputJoinCrate.make b (fun b -> a, b))
        | Static a, Static b -> Static (a, b)
        | Dynamic a, Join joinCrateB ->
            joinCrateB.Apply { new InputJoinEval<'b, Input<'a * 'b>> with
                member _.Eval (b, f) = Join (InputJoinCrate.make (I.Both a b) (fun (a, b) -> a, f b)) }
        | Static a, Join joinCrateB ->
            joinCrateB.Apply { new InputJoinEval<'b, Input<'a * 'b>> with
                member _.Eval (b, f) = Join (InputJoinCrate.make b (fun b -> a, f b)) }
        | Join joinCrateA, Static b ->
            joinCrateA.Apply { new InputJoinEval<'a, Input<'a * 'b>> with
                member _.Eval (a, f) = Join (InputJoinCrate.make a (fun a -> f a, b)) }
        | Join joinCrateA, Dynamic b ->
            joinCrateA.Apply { new InputJoinEval<'a, Input<'a * 'b>> with
                member _.Eval (a, f) = Join (InputJoinCrate.make (I.Both a b) (fun (a, b) -> f a, b)) }
        | Join joinCrateA, Join joinCrateB ->
            joinCrateA.Apply { new InputJoinEval<'a, Input<'a * 'b>> with
                member _.Eval (a, f) = 
                    joinCrateB.Apply { new InputJoinEval<'b, Input<'a * 'b>> with
                        member _.Eval (b, g) = Join (InputJoinCrate.make (I.Both a b) (fun (a, b) -> f a, g b)) } }