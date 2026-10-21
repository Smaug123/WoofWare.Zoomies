namespace WoofWare.Zoomies.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestWrapMeasureMatchesRender =

    type private TextKind =
        | Plain
        /// StyledSpans, with the text cut into spans at these (sorted, distinct) offsets.
        | Spans of cuts : int list

    let private makeVdom (kind : TextKind) (alignment : ContentAlignment) (text : string) : Vdom<DesiredBounds> =
        match kind with
        | Plain -> Vdom.textContent (text, alignment = alignment, wrap = true)
        | Spans cuts ->
            let bounds =
                0 :: (cuts |> List.filter (fun c -> c > 0 && c < text.Length)) @ [ text.Length ]

            let spans =
                bounds
                |> List.pairwise
                |> List.map (fun (a, b) ->
                    {
                        Text = text.Substring (a, b - a)
                        Style = CellStyle.none
                    }
                )

            Vdom.styledSpans (spans, alignment = alignment, wrap = true)

    /// Render into a console of exactly the given size, returning its rows.
    let private renderRows (width : int) (height : int) (vdom : Vdom<DesiredBounds>) : string[] =
        let console, terminal = ConsoleHarness.make' (fun () -> width) (fun () -> height)
        use renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None
        Render.oneStep renderState () (fun () -> vdom)

        (ConsoleHarness.toString terminal).Split '\n'
        |> Array.filter (fun row -> row.EndsWith '|')
        |> Array.map (fun row -> row.TrimEnd '|')

    let private nonSpace (s : string) : string =
        s |> String.filter (fun c -> c <> ' ' && c <> '\n')

    let private gen =
        gen {
            let letter = Gen.elements [ 'a' ; 'b' ]
            // Start and end with a letter, so that the first and last rows of a tight render are visibly non-blank.
            let! first = letter
            let! middle = Gen.elements [ 'a' ; 'b' ; ' ' ; '\n' ] |> Gen.arrayOf
            let! last = letter
            let text = String [| yield first ; yield! middle ; yield last |]
            let! width = Gen.choose (1, 12)
            let! alignment = Gen.elements [ ContentAlignment.TopLeft ; ContentAlignment.Centered ]

            let! kind =
                Gen.oneof
                    [
                        Gen.constant Plain
                        Gen.choose (0, text.Length)
                        |> Gen.listOf
                        |> Gen.map (List.distinct >> List.sort >> Spans)
                    ]

            return text, width, alignment, kind
        }

    [<Test>]
    let ``wrapped text measures exactly as many rows as the renderer draws`` () =
        let prop (text : string, width : int, alignment : ContentAlignment, kind : TextKind) =
            let vdom = makeVdom kind alignment text

            let height =
                (VdomBounds.measure
                    vdom
                    {
                        MaxWidth = width
                        MaxHeight = 1000
                    })
                    .PreferredHeightForWidth
                    width

            let rows = renderRows width height vdom

            // Not under-measured: given exactly the measured height, every character is drawn...
            rows |> String.concat "" |> nonSpace |> shouldEqual (nonSpace text)
            // ...and not over-measured: the first and last rows are both used.
            (nonSpace rows.[0], nonSpace rows.[rows.Length - 1])
            |> fun (a, b) -> (a <> "", b <> "")
            |> shouldEqual (true, true)

        Check.One (propConfig, Prop.forAll (Arb.fromGen gen) prop)

    [<Test>]
    let ``a line exactly filling the width, then a newline, takes no extra row`` () =
        // Codex's example: at width 5, "aaa bbb cc" wraps to exactly two full rows.
        for alignment in [ ContentAlignment.TopLeft ; ContentAlignment.Centered ] do
            renderRows 5 3 (makeVdom Plain alignment "aaa bbb cc\nx")
            |> Array.map (fun row -> row.Trim ())
            |> shouldEqual [| "aaa b" ; "bb cc" ; "x" |]
