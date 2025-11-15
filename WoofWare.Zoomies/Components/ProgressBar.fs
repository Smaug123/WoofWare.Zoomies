namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

/// High-level progress bar component for showing completion progress.
[<RequireQualifiedAccess>]
module ProgressBar =

    type Options =
        {
            Label : string option
            ShowPercentage : bool
        }

        static member Default =
            {
                Label = None
                ShowPercentage = true
            }

        static member WithLabel (s : string) (o : Options) =
            { o with
                Label = Some s
            }

        static member WithoutPercentage (o : Options) =
            { o with
                ShowPercentage = false
            }

    /// <summary>Creates a progress bar component.</summary>
    /// <param name="options">Customise the display, e.g. by adding a text label.</param>
    /// <param name="progress">Current progress value (should be between 0.0 and 1.0; invalid values show an empty bar with "n/a%" text).</param>
    /// <param name="width">Width of the bar portion in characters, or None to automatically size. Non-positive widths default to 10.</param>
    /// <remarks>
    /// The bar uses Unicode block characters:
    /// - Filled: █ (U+2588)
    /// - Empty: ░ (U+2591)
    ///
    /// Examples:
    /// - No label: "[██████████] 100%"
    /// - No label: "[█████░░░░░] 52%"
    /// - With label: "Loading: [███░░░░░░░] 30%"
    /// - No percentage: "[█████░░░░░]"
    /// </remarks>
    let make' (options : Options) (progress : float) (width : int option) : Vdom<DesiredBounds, Unkeyed> =
        let isValid, progress =
            if progress < 0.0 || progress > 1.0 || System.Double.IsNaN progress then
                false, 0.0
            else
                true, progress

        let showPercentage = options.ShowPercentage

        let renderBarWithWidth (barWidth : int) : Vdom<DesiredBounds, Unkeyed> =
            let filledCount = int (progress * float barWidth)
            let emptyCount = barWidth - filledCount

            let filledBar = System.String ('█', filledCount)
            let emptyBar = System.String ('░', emptyCount)
            let barContent = $"[%s{filledBar}%s{emptyBar}]"

            let fullContent =
                if showPercentage then
                    let percentage =
                        if isValid then
                            " " + $"%d{int (progress * 100.0)}" + "%"
                        else
                            " n/a%"

                    barContent + percentage
                else
                    barContent

            let barVdom = Vdom.textContent false fullContent

            match options.Label with
            | Some labelText ->
                let labelVdom = Vdom.textContent false labelText
                let labelWidth = labelText.Length
                Vdom.panelSplitAbsolute (SplitDirection.Vertical, labelWidth, labelVdom, barVdom)
            | None -> barVdom

        match width with
        | Some w ->
            // Use a sensible default if width is invalid
            let barWidth = if w <= 0 then 10 else w
            renderBarWithWidth barWidth
        | None ->
            // Use flexible content to render into whatever space we get
            let measure (constraints : MeasureConstraints) =
                let labelWidth =
                    match options.Label with
                    | Some label -> label.Length
                    | None -> 0

                let percentageWidth = if showPercentage then 5 else 0 // " 100%"
                let minBarWidth = 10
                let preferredBarWidth = 40

                {
                    MinWidth = labelWidth + minBarWidth + percentageWidth + 2
                    PreferredWidth = labelWidth + preferredBarWidth + percentageWidth + 2
                    MaxWidth = None
                    MinHeightForWidth = fun _ -> 1
                    PreferredHeightForWidth = fun _ -> 1
                    MaxHeightForWidth = fun _ -> Some 1
                }

            let render (bounds : Rectangle) =
                let labelWidth =
                    match options.Label with
                    | Some label -> label.Length
                    | None -> 0

                let percentageWidth = if showPercentage then 5 else 0 // " 100%"
                // barWidth is the width of the filled/empty portion (not including brackets or percentage)
                // renderBarWithWidth will add brackets (+2) and percentage (percentageWidth)
                let barWidth = bounds.Width - labelWidth - percentageWidth - 2 // -2 for brackets
                let barWidth = max 5 barWidth // Ensure minimum bar width

                renderBarWithWidth barWidth

            Vdom.flexibleContent measure render

    /// <summary>Creates a progress bar component.</summary>
    /// <param name="progress">Current progress value (should be between 0.0 and 1.0; invalid values show an empty bar with "n/a%" text).</param>
    /// <param name="width">Width of the bar portion in characters, or None to automatically size. Non-positive widths default to 10.</param>
    /// <remarks>
    /// The bar uses Unicode block characters:
    /// - Filled: █ (U+2588)
    /// - Empty: ░ (U+2591)
    ///
    /// Examples:
    /// - "[█████░░░░░] 52%"
    /// - "[██████████] 100%"
    ///
    /// Use <see cref="make'"/> to customize the display with labels and other options.
    /// </remarks>
    let make (progress : float) (width : int option) : Vdom<DesiredBounds, Unkeyed> =
        make' Options.Default progress width
