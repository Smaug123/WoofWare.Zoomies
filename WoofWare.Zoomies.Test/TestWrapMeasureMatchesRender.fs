namespace WoofWare.Zoomies.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestWrapMeasureMatchesRender =

    /// The renderer wraps each line of wrapped text into chunks of exactly the available width
    /// (see Render.fs: "Wrap long lines by breaking them into chunks of bounds.Width").
    let private renderedLineCount (text : string) (width : int) : int =
        text.Replace("\r\n", "\n").Replace("\r", "\n").Split '\n'
        |> Array.sumBy (fun line ->
            if line.Length = 0 then
                1
            else
                (line.Length + width - 1) / width
        )
        |> max 1

    let private textGen : Gen<string> =
        Gen.elements [ 'a' ; 'b' ; ' ' ; '\n' ] |> Gen.arrayOf |> Gen.map System.String

    [<Test>]
    let ``wrapped text measures as many lines as the renderer draws`` () =
        let gen =
            gen {
                let! text = textGen
                let! width = Gen.choose (1, 12)
                return text, width
            }

        let prop (text : string, width : int) =
            let measured =
                VdomBounds.measure
                    (Vdom.textContent (text, wrap = true))
                    {
                        MaxWidth = width
                        MaxHeight = 1000
                    }

            measured.PreferredHeightForWidth width
            |> shouldEqual (renderedLineCount text width)

        Check.One (propConfig, Prop.forAll (Arb.fromGen gen) prop)
