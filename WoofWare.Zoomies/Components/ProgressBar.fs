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
            {
                o with Label = Some s
            }
        static member WithoutPercentage (o : Options) =
            {
                o with ShowPercentage = false
            }

    /// <summary>Creates a progress bar component.</summary>
    /// <param name="options">Customise the display, e.g. by adding a text label.</param>
    /// <param name="progress">Current progress value (must be between 0.0 and 1.0).</param>
    /// <param name="width">Width of the bar portion in characters (must be strictly positive), or None to automatically size.</param>
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

        if width <= 0 then
            invalidArg (nameof width) "width must be positive"

        let showPercentage = options.ShowPercentage

        let filledCount = int (progress * float width)
        let emptyCount = width - filledCount

        let filledBar = System.String('█', filledCount)
        let emptyBar = System.String('░', emptyCount)
        let barContent = $"[%s{filledBar}%s{emptyBar}]"

        let barVdom = Vdom.textContent false barContent

        let withPercentage =
            if showPercentage then
                let percentage =
                    if isValid then
                        " " + $"%.0f{progress * 100.0}" + "%"
                    else
                        " n/a%"
                let percentageVdom = Vdom.textContent false percentage
                let percentageWidth = percentage.Length
                Vdom.panelSplitAbsolute (
                    SplitDirection.Vertical,
                    -percentageWidth,
                    barVdom,
                    percentageVdom
                )
            else
                barVdom

        match options.Label with
        | Some labelText ->
            let labelVdom = Vdom.textContent false labelText
            let labelWidth = labelText.Length
            Vdom.panelSplitAbsolute (
                SplitDirection.Vertical,
                labelWidth,
                labelVdom,
                withPercentage
            )
        | None -> withPercentage

    /// <summary>Creates a progress bar component.</summary>
    /// <param name="progress">Current progress value (must be between 0.0 and 1.0).</param>
    /// <param name="width">Width of the bar portion in characters (must be strictly positive), or None to automatically size.</param>
    /// <remarks>
    /// The bar uses Unicode block characters:
    /// - Filled: █ (U+2588)
    /// - Empty: ░ (U+2591)
    ///
    /// Examples:
    /// - "[█████░░░░░] 52%"
    /// - "[██████████] 100%"
    ///
    /// Use <see cref="make'"/> to
    /// </remarks>
    let make (progress : float) (width : int option) : Vdom<DesiredBounds, Unkeyed> =
        make' Options.Default progress width
