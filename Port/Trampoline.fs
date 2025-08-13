namespace WoofWare.Zoomies.Port

open TypeEquality

type TrampolineBindEval<'b, 'ret> =
    abstract Eval<'a> : 'a Trampoline -> ('a -> 'b Trampoline) -> 'ret

and TrampolineBindCrate<'b> =
    abstract Apply<'ret> : TrampolineBindEval<'b, 'ret> -> 'ret

and Trampoline<'a> =
    | Lazy of Lazy<Trampoline<'a>>
    | Return of 'a
    | Bind of TrampolineBindCrate<'a>

[<RequireQualifiedAccess>]
module Trampoline =
    let ofLazy t = Trampoline.Lazy t
    let lift a = Trampoline.Return a

    let bind (f : 'a -> 'b Trampoline) (t : Trampoline<'a>) : 'b Trampoline =
        { new TrampolineBindCrate<_> with
            member _.Apply e = e.Eval<'a> t f
        }
        |> Trampoline.Bind

    type StackConsEval<'a, 'b, 'ret> =
        abstract Eval<'c> : ('a -> 'c Trampoline) * Stack<'c, 'b> -> 'ret

    and StackConsCrate<'a, 'b> =
        abstract Apply<'ret> : StackConsEval<'a, 'b, 'ret> -> 'ret

    and Stack<'a, 'b> =
        | Empty of Teq<'a, 'b>
        | Cons of StackConsCrate<'a, 'b>

    let stackCons k stack =
        { new StackConsCrate<_, _> with
            member _.Apply e = e.Eval (k, stack)
        }
        |> Stack.Cons

    let rec runAux<'a, 'b> (t : Trampoline<'a>) (stack : Stack<'a, 'b>) : 'b =
        match t with
        | Trampoline.Lazy t -> runAux (t.Force ()) stack
        | Trampoline.Bind cr ->
            { new TrampolineBindEval<_, _> with
                member _.Eval t k = runAux t (stackCons k stack)
            }
            |> cr.Apply
        | Trampoline.Return a ->
            match stack with
            | Stack.Empty t -> Teq.cast t a
            | Stack.Cons cr ->
                { new StackConsEval<_, _, _> with
                    member _.Eval (k, stack) = runAux (k a) stack
                }
                |> cr.Apply

    let run t = runAux t (Stack.Empty Teq.refl)

    let map (f : 'a -> 'b) (t : Trampoline<'a>) : Trampoline<'b> = bind (fun x -> f x |> lift) t

    let all (ts : 'a Trampoline seq) : 'a list Trampoline =
        (lift [], ts)
        ||> Seq.fold (fun acc t -> bind (fun xs -> map (fun x -> x :: xs) t) acc)
        |> map List.rev

    let allMap m =
        m
        |> Map.toSeq
        |> Seq.map (fun (k, v) -> map (fun v -> k, v) v)
        |> all
        |> bind (fun alist -> lift (Map.ofList alist))

    let both a b =
        a |> bind (fun a -> b |> bind (fun b -> lift (a, b)))

[<Sealed>]
type TrampolineBuilder () =
    member _.Return (x) = Trampoline.lift x
    member _.ReturnFrom (t : Trampoline<'a>) = t
    member _.Bind (t : Trampoline<'a>, f : 'a -> Trampoline<'b>) = Trampoline.bind f t
    member _.Zero () = Trampoline.lift ()

    // For 'map' functionality - this enables let! with a non-monadic continuation
    member _.BindReturn (t : Trampoline<'a>, f : 'a -> 'b) = Trampoline.map f t

    // For 'both' functionality - this enables and! syntax for parallel composition
    member _.MergeSources (ta : Trampoline<'a>, tb : Trampoline<'b>) = Trampoline.both ta tb

    // Additional useful operations
    member _.Delay (f : unit -> Trampoline<'a>) = Trampoline.ofLazy (lazy f ())
    member _.Combine (t1 : Trampoline<unit>, t2 : Trampoline<'a>) = Trampoline.bind (fun () -> t2) t1

    // For if-then-else
    member _.Source (t : Trampoline<'a>) = t

    member _.Using (disposable : System.IDisposable, body : _ -> Trampoline<'a>) =
        try
            body disposable
        finally
            disposable.Dispose ()

[<AutoOpen>]
module TrampolineBuilders =
    let trampoline = TrampolineBuilder ()
