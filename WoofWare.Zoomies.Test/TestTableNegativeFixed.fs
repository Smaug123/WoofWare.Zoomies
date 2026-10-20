namespace WoofWare.Zoomies.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTableNegativeFixed =

    let private renderToString (width : int) (height : int) (vdom : Vdom<DesiredBounds>) : string =
        let console, terminal = ConsoleHarness.make' (fun () -> width) (fun () -> height)
        use renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None
        Render.oneStep renderState () (fun () -> vdom)
        ConsoleHarness.toString terminal

    let private table (cols : Column[]) (rows : Row[]) : Vdom<DesiredBounds> =
        let cells =
            Array.init rows.Length (fun r -> Array.init cols.Length (fun c -> Vdom.textContent $"r%d{r}c%d{c}"))

        Table.make (NodeKey.make "t") cells cols rows

    [<Test>]
    let ``a negative Fixed size renders like Fixed 0`` () =
        let gen =
            gen {
                let! numCols = Gen.choose (1, 4)
                let! numRows = Gen.choose (1, 3)
                let! colSizes = Gen.arrayOfLength numCols (Gen.choose (-5, 5))
                let! rowSizes = Gen.arrayOfLength numRows (Gen.choose (-5, 5))
                let! width = Gen.choose (1, 30)
                let! height = Gen.choose (1, 10)
                return colSizes, rowSizes, width, height
            }

        let prop (colSizes : int[], rowSizes : int[], width : int, height : int) =
            let actual =
                table (Array.map Column.Fixed colSizes) (Array.map Row.Fixed rowSizes)
                |> renderToString width height

            let expected =
                table (Array.map (max 0 >> Column.Fixed) colSizes) (Array.map (max 0 >> Row.Fixed) rowSizes)
                |> renderToString width height

            actual |> shouldEqual expected

        Check.One (propConfig.WithMaxTest 1000, Prop.forAll (Arb.fromGen gen) prop)
