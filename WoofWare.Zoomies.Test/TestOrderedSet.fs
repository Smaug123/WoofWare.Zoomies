namespace WoofWare.Zoomies.Test

open NUnit.Framework
open FsCheck
open WoofWare.Zoomies

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestOrderedSet =
    [<Test>]
    let ``adding duplicate items does not increase count`` () =
        let property (items : int list) =
            let set = OrderedSet<int> ()
            items |> List.iter (fun x -> set.Add (x) |> ignore<bool>)
            let distinctItems = items |> List.distinct
            set.Count = distinctItems.Length

        Check.One (propConfig, property)

    [<Test>]
    let ``items appear in insertion order`` () =
        let property (items : int list) =
            let set = OrderedSet<int> ()
            items |> List.iter (fun x -> set.Add (x) |> ignore<bool>)

            let expected = items |> List.distinct
            let actual = set.ToSeq () |> Seq.toList

            expected = actual

        Check.One (propConfig, property)

    [<Test>]
    let ``contains returns true for added items`` () =
        let property (items : int list) =
            let set = OrderedSet<int> ()
            items |> List.iter (fun x -> set.Add (x) |> ignore<bool>)
            items |> List.forall (fun x -> set.Contains (x))

        Check.One (propConfig, property)

    [<Test>]
    let ``contains returns false for items not added`` () =
        let property (added : int list) (notAdded : int list) =
            let set = OrderedSet<int> ()
            added |> List.iter (fun x -> set.Add (x) |> ignore<bool>)

            let notAddedDistinct =
                notAdded |> List.filter (fun x -> not (List.contains x added))

            notAddedDistinct |> List.forall (fun x -> not (set.Contains (x)))

        Check.One (propConfig, property)

    [<Test>]
    let ``add returns true for new items and false for duplicates`` () =
        let property (items : int list) =
            let set = OrderedSet<int> ()
            let mutable seen = Set.empty

            items
            |> List.forall (fun x ->
                let wasAdded = set.Add (x)
                let shouldAdd = not (Set.contains x seen)
                seen <- Set.add x seen
                wasAdded = shouldAdd
            )

        Check.One (propConfig, property)

    [<Test>]
    let ``indexed access matches insertion order`` () =
        let property (item1 : int) (items : int list) =
            let items = item1 :: items

            let set = OrderedSet<int> ()
            items |> List.iter (fun x -> set.Add (x) |> ignore<bool>)
            let expected = items |> List.distinct
            [ 0 .. expected.Length - 1 ] |> List.forall (fun i -> set.[i] = expected.[i])

        Check.One (propConfig, property)

    [<Test>]
    let ``clear removes all items`` () =
        let property (items : int list) =
            let set = OrderedSet<int> ()
            items |> List.iter (fun x -> set.Add (x) |> ignore<bool>)
            set.Clear ()
            set.Count = 0 && (items |> List.forall (fun x -> not (set.Contains (x))))

        Check.One (propConfig, property)

    [<Test>]
    let ``ToSeq is empty after Clear`` () =
        let property (items : int list) =
            let set = OrderedSet<int> ()
            items |> List.iter (fun x -> set.Add (x) |> ignore<bool>)
            set.Clear ()
            set.ToSeq () |> Seq.isEmpty

        Check.One (propConfig, property)

    type OrderedSetOp = | Add of int

    let private applyOps (ops : OrderedSetOp list) (set : OrderedSet<int>) =
        for op in ops do
            match op with
            | Add x -> set.Add (x) |> ignore<bool>

    [<Test>]
    let ``operations after Clear are equivalent to operations on fresh set`` () =
        let property (opsBefore : OrderedSetOp list) (opsAfter : OrderedSetOp list) =
            let setClearThenOps = OrderedSet<int> ()
            applyOps opsBefore setClearThenOps
            setClearThenOps.Clear ()
            applyOps opsAfter setClearThenOps

            let setFresh = OrderedSet<int> ()
            applyOps opsAfter setFresh

            let seqAfterClear = setClearThenOps.ToSeq () |> Seq.toList
            let seqFresh = setFresh.ToSeq () |> Seq.toList
            seqAfterClear = seqFresh && setClearThenOps.Count = setFresh.Count

        Check.One (propConfig, property)
