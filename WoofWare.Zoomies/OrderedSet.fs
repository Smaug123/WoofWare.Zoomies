namespace WoofWare.Zoomies

type internal OrderedSet<'T when 'T : equality> (capacity : int) =
    let items = ResizeArray<'T> (capacity)
    let membership = System.Collections.Generic.HashSet<'T> (capacity)

    // default ResizeArray capacity
    new () = OrderedSet (4)

    /// Returns `true` if addition was successful, or `false` if we didn't add because the element was already present.
    member _.Add (item : 'T) : bool =
        if membership.Add (item) then
            items.Add (item)
            true
        else
            false

    member _.Contains (item : 'T) : bool = membership.Contains (item)

    member _.Count : int = items.Count

    member _.Item
        with get (index : int) : 'T = items.[index]

    member _.ToSeq () : seq<'T> = items :> seq<'T>

    member _.Clear () : unit =
        items.Clear ()
        membership.Clear ()

    interface System.Collections.Generic.IEnumerable<'T> with
        member this.GetEnumerator () = this.ToSeq().GetEnumerator ()

    interface System.Collections.IEnumerable with
        member this.GetEnumerator () =
            this.ToSeq().GetEnumerator () :> System.Collections.IEnumerator
