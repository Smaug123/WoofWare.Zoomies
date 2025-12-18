namespace WoofWare.Zoomies.Test

open NUnit.Framework
open FsUnitTyped
open WoofWare.Expect
open WoofWare.KnuthPlass
open WoofWare.Zoomies

[<RequireQualifiedAccess>]
module MonospaceText =
    /// Format text for monospace terminal display using Knuth-Plass line breaking.
    /// Guarantees no overfull lines (unless a single unhyphenatable word exceeds line width).
    let format (lineWidth : int) (text : string) : string =
        Text.format
            (LineBreakOptions.DefaultMonospace (float32 lineWidth))
            Text.defaultWordWidth
            Items.monospaceGlue
            Hyphenation.DEFAULT_PENALTY
            Hyphenation.simpleEnglish
            text

/// JustifiedText component copied from App for testing purposes
[<RequireQualifiedAccess>]
type JustifiedText =
    static member make
        (text : string, ?preferredWidth : int, ?expandToFill : bool, ?style : CellStyle, ?alignment : ContentAlignment)
        : Vdom<DesiredBounds>
        =
        let expandToFill = defaultArg expandToFill false

        let measure (_constraints : MeasureConstraints) : MeasuredSize =
            {
                MinWidth = 1
                PreferredWidth =
                    match expandToFill, preferredWidth with
                    | true, _
                    | false, None -> System.Int32.MaxValue
                    | false, Some preferredWidth -> preferredWidth
                MaxWidth = if expandToFill then None else preferredWidth
                MinHeightForWidth = fun _ -> 1
                PreferredHeightForWidth = fun _ -> 1
                MaxHeightForWidth = fun _ -> None
            }

        let render (bounds : Rectangle) : Vdom<DesiredBounds> =
            let width =
                match expandToFill, preferredWidth with
                | true, _
                | false, None -> bounds.Width
                | false, Some preferredWidth -> min bounds.Width preferredWidth

            let formatted = MonospaceText.format width text
            Vdom.textContent (formatted, ?style = style, ?alignment = alignment, wrap = false)

        Vdom.flexibleContent measure render

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestJustifiedText =
    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    let sampleText =
        "Mr. Utterson the lawyer was a man of a rugged countenance that was never lighted by a smile; cold, scanty and embarrassed in discourse; backward in sentiment; lean, long, dusty, dreary and yet somehow lovable."

    [<Test>]
    let ``JustifiedText renders at width 80 with expandToFill`` () =
        let console, harness = ConsoleHarness.make' (fun () -> 80) (fun () -> 10)
        let renderState = RenderState.make console MockTime.getStaticUtcNow None
        let vdom = JustifiedText.make (sampleText, expandToFill = true)

        Render.oneStep renderState () (fun _ -> vdom)

        expect {
            snapshot
                @"
Mr. Utterson the lawyer was a man of a rugged countenance that was never lighted|
by a smile; cold, scanty and embarrassed in discourse; backward in sentiment;   |
lean, long, dusty, dreary and yet somehow lovable.                              |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
"

            return ConsoleHarness.toString harness
        }

    [<Test>]
    let ``JustifiedText renders at width 120 with expandToFill`` () =
        let console, harness = ConsoleHarness.make' (fun () -> 120) (fun () -> 10)
        let renderState = RenderState.make console MockTime.getStaticUtcNow None
        let vdom = JustifiedText.make (sampleText, expandToFill = true)

        Render.oneStep renderState () (fun _ -> vdom)

        expect {
            snapshot
                @"
Mr. Utterson the lawyer was a man of a rugged countenance that was never lighted by a smile; cold, scanty and embarra-  |
ssed in discourse; backward in sentiment; lean, long, dusty, dreary and yet somehow lovable.                            |
                                                                                                                        |
                                                                                                                        |
                                                                                                                        |
                                                                                                                        |
                                                                                                                        |
                                                                                                                        |
                                                                                                                        |
                                                                                                                        |
"

            return ConsoleHarness.toString harness
        }

    [<Test>]
    let ``JustifiedText renders at width 40 with expandToFill`` () =
        let console, harness = ConsoleHarness.make' (fun () -> 40) (fun () -> 15)
        let renderState = RenderState.make console MockTime.getStaticUtcNow None
        let vdom = JustifiedText.make (sampleText, expandToFill = true)

        Render.oneStep renderState () (fun _ -> vdom)

        expect {
            snapshot
                @"
Mr. Utterson the lawyer was a man of    |
a rugged countenance that was never li- |
ghted by a smile; cold, scanty and emba-|
rrassed in discourse; backward in senti-|
ment; lean, long, dusty, dreary and yet |
somehow lovable.                        |
                                        |
                                        |
                                        |
                                        |
                                        |
                                        |
                                        |
                                        |
                                        |
"

            return ConsoleHarness.toString harness
        }

    [<Test>]
    let ``JustifiedText renders at width 80 without expandToFill`` () =
        let console, harness = ConsoleHarness.make' (fun () -> 120) (fun () -> 10)
        let renderState = RenderState.make console MockTime.getStaticUtcNow None
        let vdom = JustifiedText.make (sampleText, expandToFill = false)

        Render.oneStep renderState () (fun _ -> vdom)

        expect {
            snapshot
                @"
Mr. Utterson the lawyer was a man of a rugged countenance that was never lighted by a smile; cold, scanty and embarra-  |
ssed in discourse; backward in sentiment; lean, long, dusty, dreary and yet somehow lovable.                            |
                                                                                                                        |
                                                                                                                        |
                                                                                                                        |
                                                                                                                        |
                                                                                                                        |
                                                                                                                        |
                                                                                                                        |
                                                                                                                        |
"

            return ConsoleHarness.toString harness
        }

    [<Test>]
    let ``JustifiedText in bordered panel at width 120`` () =
        let console, harness = ConsoleHarness.make' (fun () -> 120) (fun () -> 10)
        let renderState = RenderState.make console MockTime.getStaticUtcNow None
        let vdom = JustifiedText.make (sampleText, expandToFill = true) |> Vdom.bordered

        Render.oneStep renderState () (fun _ -> vdom)

        expect {
            snapshot
                @"
┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐|
│Mr. Utterson the lawyer was a man of a rugged countenance that was never lighted by a smile; cold, scanty and embarra-│|
│ssed in discourse; backward in sentiment; lean, long, dusty, dreary and yet somehow lovable.                          │|
│                                                                                                                      │|
│                                                                                                                      │|
│                                                                                                                      │|
│                                                                                                                      │|
│                                                                                                                      │|
│                                                                                                                      │|
└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘|
"

            return ConsoleHarness.toString harness
        }

    /// Verify that MonospaceText.format never produces overfull lines
    /// (unless a single unhyphenatable word exceeds line width).
    [<Test>]
    let ``MonospaceText format produces no overfull lines`` () =
        let text =
            "Mr. Utterson the lawyer was a man of a rugged countenance that was never lighted by a smile; cold, scanty and embarrassed in discourse; backward in sentiment; lean, long, dusty, dreary and yet somehow lovable."

        for targetWidth in [ 40 ; 80 ; 118 ] do
            let formatted = MonospaceText.format targetWidth text
            let lines = formatted.Split '\n'

            for line in lines do
                Assert.That (
                    line.Length <= targetWidth,
                    sprintf "Line '%s' has length %d, exceeds target %d" line line.Length targetWidth
                )
