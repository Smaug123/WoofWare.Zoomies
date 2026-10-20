namespace WoofWare.Zoomies.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSelectionIdType =

    let private listKey = NodeKey.make "list"

    let private render (view : IVdomContext<SelectionListViewportInfo> -> Vdom<DesiredBounds>) : string =
        let console, terminal = ConsoleHarness.make' (fun () -> 20) (fun () -> 6)

        use renderState =
            RenderState.make<SelectionListViewportInfo> console MockTime.getStaticUtcNow None

        Render.oneStep renderState () (fun () -> view (VdomContext.asTyped (RenderState.vdomContext renderState)))
        ConsoleHarness.toString terminal

    let private itemsGen =
        gen {
            let! labels = Gen.listOf (Gen.elements [ "apple" ; "banana" ; "cherry" ])
            let! selected = Gen.listOfLength labels.Length (ArbMap.defaults |> ArbMap.generate<bool>)
            let! ids = Gen.listOfLength labels.Length (ArbMap.defaults |> ArbMap.generate<int>)
            let! cursor = Gen.choose (0, max 0 (labels.Length - 1))
            return List.zip3 labels selected ids, cursor
        }

    [<Test>]
    let ``MultiSelection renders the same whatever the item ids are`` () =
        let prop (items : (string * bool * int) list, cursor : int) =
            let state = SelectionListState.AtIndex cursor

            let withIntIds =
                items
                |> List.map (fun (label, isSelected, id) ->
                    {
                        Id = id
                        Label = label
                        IsSelected = isSelected
                    }
                )
                |> Array.ofList

            let withKeyIds =
                withIntIds
                |> Array.map (fun item ->
                    {
                        Id = NodeKey.make $"item-%d{item.Id}"
                        Label = item.Label
                        IsSelected = item.IsSelected
                    }
                )

            render (fun ctx -> (MultiSelection.make (ctx, listKey, withIntIds, state, id)).Vdom)
            |> shouldEqual (render (fun ctx -> (MultiSelection.make (ctx, listKey, withKeyIds, state, id)).Vdom))

        Check.One (propConfig.WithMaxTest 200, Prop.forAll (Arb.fromGen itemsGen) prop)

    [<Test>]
    let ``SingleSelection renders the same whatever the item ids are`` () =
        let prop (items : (string * bool * int) list, cursor : int) =
            let state = SelectionListState.AtIndex cursor

            let selectedIndex =
                items |> List.tryFindIndex (fun (_, isSelected, _) -> isSelected)

            let withIntIds =
                items
                |> List.map (fun (label, _, id) ->
                    {
                        Id = id
                        Label = label
                    }
                )
                |> Array.ofList

            let withKeyIds =
                withIntIds
                |> Array.map (fun item ->
                    {
                        Id = NodeKey.make $"item-%d{item.Id}"
                        Label = item.Label
                    }
                )

            render (fun ctx -> (SingleSelection.make (ctx, listKey, withIntIds, selectedIndex, state, id)).Vdom)
            |> shouldEqual (
                render (fun ctx -> (SingleSelection.make (ctx, listKey, withKeyIds, selectedIndex, state, id)).Vdom)
            )

        Check.One (propConfig.WithMaxTest 200, Prop.forAll (Arb.fromGen itemsGen) prop)
