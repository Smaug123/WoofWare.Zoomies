namespace WoofWare.Zoomies.Test

open System.Globalization
open NUnit.Framework
open FsUnitTyped
open WoofWare.Expect
open WoofWare.KnuthPlass
open WoofWare.Zoomies

[<RequireQualifiedAccess>]
module MonospaceText =
    /// Format text for monospace terminal display using Knuth-Plass line breaking.
    /// Guarantees no overfull lines - any line exceeding the width is truncated with ellipsis.
    let format (lineWidth : int) (text : string) : string =
        let formatted =
            Text.format
                (LineBreakOptions.DefaultMonospace (float32 lineWidth))
                Text.defaultWordWidth
                Items.monospaceGlue
                Hyphenation.DEFAULT_PENALTY
                Hyphenation.simpleEnglish
                text

        // Post-process: truncate any overfull lines with ellipsis
        let ellipsis = "…" // U+2026, single character width 1
        let maxContentWidth = lineWidth - 1 // Leave room for ellipsis

        formatted.Split '\n'
        |> Array.map (fun line ->
            let info = StringInfo line

            if info.LengthInTextElements > lineWidth then
                info.SubstringByTextElements (0, maxContentWidth) + ellipsis
            else
                line
        )
        |> String.concat "\n"

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
        let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None
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
        let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None
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
        let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None
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
        let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None
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
        let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None
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

    /// Verify that MonospaceText.format never produces overfull lines.
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

    /// Verify that unhyphenatable words longer than line width are truncated with ellipsis.
    [<Test>]
    let ``MonospaceText truncates long unhyphenatable words with ellipsis`` () =
        // A word that can't be hyphenated (no vowel-consonant patterns) and exceeds line width
        let longWord = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" // 52 chars, no hyphenation points
        let targetWidth = 40

        let formatted = MonospaceText.format targetWidth longWord
        let lines = formatted.Split '\n'

        // All lines must fit
        for line in lines do
            let info = StringInfo line

            Assert.That (
                info.LengthInTextElements <= targetWidth,
                sprintf "Line '%s' has %d graphemes, exceeds target %d" line info.LengthInTextElements targetWidth
            )

        // The truncated line should end with ellipsis
        let truncatedLine = lines |> Array.find (fun l -> l.Contains "…")
        truncatedLine |> shouldContainText "…"
        (StringInfo truncatedLine).LengthInTextElements |> shouldEqual targetWidth

    /// Verify that text exactly at the limit is not truncated.
    [<Test>]
    let ``MonospaceText does not truncate text at exact limit`` () =
        // Single word that fits exactly - won't be broken by Knuth-Plass
        let text = "abcdefghijklmnopqrstuvwxyzabcdefghijk" // 37 chars
        (StringInfo text).LengthInTextElements |> shouldEqual 37

        let formatted = MonospaceText.format 40 text
        formatted |> shouldEqual text
        formatted |> shouldNotContainText "…"

    /// Verify ellipsis truncation works with multi-byte characters.
    [<Test>]
    let ``MonospaceText handles multi-byte characters near truncation point`` () =
        // Mix of ASCII and emoji - emoji are single graphemes but multi-byte
        let longWord = "https://example.com/path/with/emoji/🎉🎊🎁"
        let text = sprintf "Visit %s today!" longWord
        let targetWidth = 40

        let formatted = MonospaceText.format targetWidth text
        let lines = formatted.Split '\n'

        // All lines must fit (in grapheme count, not byte count)
        for line in lines do
            let info = StringInfo line

            Assert.That (
                info.LengthInTextElements <= targetWidth,
                sprintf "Line '%s' has %d graphemes, exceeds target %d" line info.LengthInTextElements targetWidth
            )
