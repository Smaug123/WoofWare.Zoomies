namespace WoofWare.Zoomies.App

open System.Globalization
open WoofWare.KnuthPlass
open WoofWare.Zoomies
open WoofWare.LiangHyphenation

[<RequireQualifiedAccess>]
module MonospaceText =
    let english = LanguageData.load KnownLanguage.EnGb

    /// Format text for monospace terminal display using Knuth-Plass line breaking.
    /// Guarantees no overfull lines - any line exceeding the width is truncated with ellipsis.
    ///
    /// Uses monospace glue (spaces can't stretch/shrink) and high tolerance (accepts underfull lines),
    /// which forces the algorithm to break early rather than produce lines that would overflow.
    /// Post-processes any remaining overfull lines (e.g., unhyphenatable URLs) with truncation.
    let format (lineWidth : int) (text : string) : string =
        let formatted =
            Text.format
                (LineBreakOptions.DefaultMonospace (float32 lineWidth))
                Text.defaultWordWidth
                Items.monospaceGlue
                Hyphenation.DEFAULT_PENALTY
                (fun s ->
                    Hyphenation.hyphenate english s
                    |> FilteredPriorities.fromLiangEnglish (StringInfo(s).LengthInTextElements)
                )
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

/// Component for displaying text laid out using the Knuth-Plass line-breaking algorithm.
[<RequireQualifiedAccess>]
type JustifiedText =
    /// <summary>Creates a text area where the text is laid out using Knuth-Plass justification.</summary>
    /// <param name="text">The text content to display.</param>
    /// <param name="preferredWidth">Preferred width in cells. Defaults to 80.</param>
    /// <param name="expandToFill">If true, expands to fill all available width. Defaults to false.</param>
    /// <param name="style">Optional styling for the text.</param>
    /// <param name="alignment">Optional alignment within the available space.</param>
    /// <remarks>
    /// This component uses WoofWare.KnuthPlass to perform optimal line breaking based on
    /// the available width. The text is re-laid-out whenever the available bounds change.
    ///
    /// Uses monospace-appropriate settings: spaces don't stretch/shrink, and underfull lines
    /// are preferred over overfull lines (which would be truncated in a terminal).
    /// </remarks>
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
