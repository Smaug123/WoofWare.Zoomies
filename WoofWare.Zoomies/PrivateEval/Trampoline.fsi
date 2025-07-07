namespace WoofWare.Zoomies

open System

type Trampoline<'a>

[<RequireQualifiedAccess>]
module Trampoline =
    /// <remarks>
    /// This was called <c>lazy</c> in the original OCaml.
    /// </remarks>
    val ofLazy : Lazy<Trampoline<'a>> -> Trampoline<'a>

    val run : Trampoline<'a> -> 'a

    /// <remarks>
    /// This was called <c>return</c> in the original OCaml.
    /// </remarks>
    val lift : 'a -> 'a Trampoline

    val allMap<'k, 'v when 'k : comparison> : Map<'k, 'v Trampoline> -> Trampoline<Map<'k, 'v>>

    /// <remarks>
    /// This eagerly consumes the seq.
    /// </remarks>
    val all : 'a Trampoline seq -> 'a list Trampoline

    val map : ('a -> 'b) -> 'a Trampoline -> 'b Trampoline

[<Sealed>]
type TrampolineBuilder =
    member Return : 'b -> Trampoline<'b>
    member ReturnFrom : Trampoline<'a> -> Trampoline<'a>
    member Bind : Trampoline<'a> * ('a -> Trampoline<'b>) -> Trampoline<'b>
    member Zero : unit -> Trampoline<unit>

    member BindReturn : Trampoline<'a> * ('a -> 'b) -> Trampoline<'b>

    member MergeSources : Trampoline<'a> * Trampoline<'b> -> Trampoline<'a * 'b>

    member Delay : (unit -> Trampoline<'a>) -> Trampoline<'a>
    member Combine : Trampoline<unit> * Trampoline<'a> -> Trampoline<'a>

    member Source : Trampoline<'a> -> Trampoline<'a>
    member Using : IDisposable * (IDisposable -> Trampoline<'a>) -> Trampoline<'a>

[<AutoOpen>]
module TrampolineBuilders =
    val trampoline : TrampolineBuilder
