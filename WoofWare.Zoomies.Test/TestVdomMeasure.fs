namespace WoofWare.Zoomies.Test

open NUnit.Framework
open WoofWare.Zoomies
open FsUnitTyped

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestVdomMeasure =
    [<Test>]
    let ``VdomBounds.measure returns correct measurements for text content`` () =
        let vdom = Vdom.textContent "Hello World"

        let constraints =
            {
                MaxWidth = 100
                MaxHeight = 100
            }

        let measured = VdomBounds.measure vdom constraints

        measured.MinWidth |> shouldEqual 5 // "World" is the longest word
        measured.PreferredWidth |> shouldEqual 11 // "Hello World" full length
        measured.MinHeightForWidth 11 |> shouldEqual 1
        measured.PreferredHeightForWidth 11 |> shouldEqual 1

    [<Test>]
    let ``VdomBounds.measure handles text wrapping`` () =
        let vdom = Vdom.textContent "Hello World Test"

        let constraints =
            {
                MaxWidth = 100
                MaxHeight = 100
            }

        let measured = VdomBounds.measure vdom constraints

        measured.MinWidth |> shouldEqual 5 // "Hello" or "World" is the longest word
        measured.PreferredWidth |> shouldEqual 16 // Full text length

        // When constrained to 10 chars, should wrap
        let heightAt10 = measured.PreferredHeightForWidth 10

        (heightAt10 > 1) |> shouldEqual true

    [<Test>]
    let ``VdomBounds.measure handles bordered content`` () =
        let innerVdom = Vdom.textContent "Test"
        let vdom = Vdom.bordered innerVdom

        let constraints =
            {
                MaxWidth = 100
                MaxHeight = 100
            }

        let measured = VdomBounds.measure vdom constraints

        // Border adds 2 to width (1 on each side)
        measured.MinWidth |> shouldEqual 6 // 4 + 2 for border
        measured.PreferredWidth |> shouldEqual 6 // 4 + 2 for border

        // Border adds 2 to height
        measured.MinHeightForWidth 10 |> shouldEqual 3 // 1 + 2 for border

    [<Test>]
    let ``VdomBounds.measure handles panel splits`` () =
        let left = Vdom.textContent "Left"
        let right = Vdom.textContent "Right"

        let leftKeyed = Vdom.withKey (NodeKey.make "left") left
        let rightKeyed = Vdom.withKey (NodeKey.make "right") right

        let vdom = Vdom.panelSplitAuto (SplitDirection.Vertical, leftKeyed, rightKeyed)

        let constraints =
            {
                MaxWidth = 100
                MaxHeight = 100
            }

        let measured = VdomBounds.measure vdom constraints

        // Auto split: MinWidth is sum of both
        measured.MinWidth |> shouldEqual 9 // 4 + 5 = 9
        measured.PreferredWidth |> shouldEqual 9

    [<Test>]
    let ``VdomBounds.measure handles empty vdom`` () =
        let vdom = Vdom.empty

        let constraints =
            {
                MaxWidth = 100
                MaxHeight = 100
            }

        let measured = VdomBounds.measure vdom constraints

        measured.MinWidth |> shouldEqual 0
        measured.PreferredWidth |> shouldEqual 0
        measured.MinHeightForWidth 10 |> shouldEqual 0
        measured.PreferredHeightForWidth 10 |> shouldEqual 0

    [<Test>]
    let ``VdomBounds.measure respects max width constraint`` () =
        let vdom = Vdom.textContent "This is a very long text that exceeds constraints"

        let constraints =
            {
                MaxWidth = 20
                MaxHeight = 100
            }

        let measured = VdomBounds.measure vdom constraints

        // Should be clamped to max width
        (measured.PreferredWidth <= 20) |> shouldEqual true
        (measured.MinWidth <= 20) |> shouldEqual true

    [<Test>]
    let ``VdomBounds.measure works with FlexibleContent`` () =
        let customMeasure (_ : MeasureConstraints) : MeasuredSize =
            {
                MinWidth = 5
                PreferredWidth = 10
                MaxWidth = Some 20
                MinHeightForWidth = fun _ -> 2
                PreferredHeightForWidth = fun _ -> 3
                MaxHeightForWidth = fun _ -> Some 5
            }

        let customRender (_ : Rectangle) : Vdom<DesiredBounds> = Vdom.textContent "Custom"

        let vdom = Vdom.flexibleContent customMeasure customRender

        let constraints =
            {
                MaxWidth = 100
                MaxHeight = 100
            }

        let measured = VdomBounds.measure vdom constraints

        measured.MinWidth |> shouldEqual 5
        measured.PreferredWidth |> shouldEqual 10
        measured.MinHeightForWidth 15 |> shouldEqual 2
        measured.PreferredHeightForWidth 15 |> shouldEqual 3
