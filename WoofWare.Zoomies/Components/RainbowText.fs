namespace WoofWare.Zoomies.Components

open System
open WoofWare.Incremental
open WoofWare.Zoomies

/// Component for displaying text with rainbow coloring and an idle pulse animation.
/// Each character is colored in repeating ROYGBIV pattern, and when idle for long enough,
/// a "light wave" sweeps across the text (lighter color variants).
///
/// This component only exposes an incremental API to ensure correct usage with animations.
/// The pulse animation requires the Incremental clock infrastructure to update properly.
[<RequireQualifiedAccess>]
module RainbowText =

    /// Configuration for the rainbow text component.
    type Config =
        {
            /// Seconds of inactivity before the pulse animation starts.
            IdleThresholdSeconds : float
            /// Width of the pulse highlight in characters.
            PulseWidthChars : int
            /// Speed of the pulse in characters per second.
            PulseSpeedCharsPerSec : float
        }

        /// Default configuration: 3 second idle threshold, 3 character pulse width, 5 chars/sec speed.
        static member Default =
            {
                IdleThresholdSeconds = 3.0
                PulseWidthChars = 3
                PulseSpeedCharsPerSec = 5.0
            }

    /// ROYGBIV colors (7 colors).
    let private roygbiv =
        [|
            ConsoleColor.Red
            ConsoleColor.DarkYellow // Orange approximation
            ConsoleColor.Yellow
            ConsoleColor.Green
            ConsoleColor.Blue
            ConsoleColor.DarkBlue // Indigo
            ConsoleColor.Magenta // Violet
        |]

    /// Lighter variants for the pulse effect.
    let private lightVariants =
        [|
            ConsoleColor.Red // Red stays red (already bright)
            ConsoleColor.Yellow // Lighter orange
            ConsoleColor.Yellow // Yellow stays yellow
            ConsoleColor.Cyan // Lighter green
            ConsoleColor.Cyan // Lighter blue
            ConsoleColor.Blue // Lighter indigo
            ConsoleColor.Magenta // Violet stays magenta
        |]

    /// <summary>Creates rainbow-colored text with optional pulse effect.</summary>
    /// <param name="text">The text to display with rainbow coloring.</param>
    /// <param name="pulsePosition">
    /// If ValueSome, the starting character index of the pulse highlight.
    /// The pulse wraps around the text. If ValueNone, no pulse is shown.
    /// </param>
    /// <param name="pulseWidth">Width of the pulse in characters.</param>
    /// <remarks>Internal: Use makeIncr for the public API.</remarks>
    let internal make (text : string) (pulsePosition : int voption) (pulseWidth : int) : Vdom<DesiredBounds> =
        if String.IsNullOrEmpty text then
            Vdom.styledSpans []
        else
            let textLength = text.Length

            let spans =
                text
                |> Seq.mapi (fun i ch ->
                    let colorIdx = i % 7

                    let inPulse =
                        match pulsePosition with
                        | ValueSome pos ->
                            // Characters from pos to pos + pulseWidth are in the pulse (wrapping)
                            let dist = (i - pos + textLength) % textLength
                            dist < pulseWidth
                        | ValueNone -> false

                    let color =
                        if inPulse then
                            lightVariants.[colorIdx]
                        else
                            roygbiv.[colorIdx]

                    {
                        Text = string<char> ch
                        Style = CellStyle.none |> CellStyle.withForeground color
                    }
                )
                |> Seq.toList

            Vdom.styledSpans spans

    /// Compute the pulse frame index given idle duration and config.
    /// Returns ValueNone if not idle long enough to show pulse.
    /// The frame wraps based on text length.
    /// <remarks>Internal: Use makeIncr for the public API.</remarks>
    let internal computePulseFrame (idleDuration : TimeSpan) (textLength : int) (config : Config) : int voption =
        if textLength <= 0 then
            ValueNone
        elif idleDuration.TotalSeconds < config.IdleThresholdSeconds then
            ValueNone
        else
            // Use integer milliseconds to avoid floating point precision issues
            let elapsedMs =
                int64 (idleDuration.TotalMilliseconds - config.IdleThresholdSeconds * 1000.0)

            let msPerChar = int64 (1000.0 / config.PulseSpeedCharsPerSec)

            if msPerChar <= 0L then
                ValueSome 0
            else
                ValueSome (int ((elapsedMs / msPerChar) % int64 textLength))

    /// <summary>Creates an incremental rainbow text node that updates based on idle duration.</summary>
    /// <param name="incr">The Incremental instance for creating nodes.</param>
    /// <param name="currentTimeNode">Node containing the current time (e.g., from VdomContext.clockDateTimeNode).</param>
    /// <param name="lastInputTimeNode">Node containing the time of the last user input (ValueNone if never).</param>
    /// <param name="text">The text to display with rainbow coloring.</param>
    /// <param name="config">Configuration for idle threshold, pulse width, and speed.</param>
    /// <returns>A Node that produces a new Vdom each time the pulse frame changes.</returns>
    let makeIncr
        (incr : Incremental)
        (currentTimeNode : DateTime Node)
        (lastInputTimeNode : DateTime voption Node)
        (text : string)
        (config : Config)
        : Vdom<DesiredBounds> Node
        =
        if String.IsNullOrEmpty text then
            incr.Return (Vdom.styledSpans [])
        else
            let textLength = text.Length

            // Create a node that computes the pulse frame based on current time and last input time
            let pulseFrameNode =
                incr.Map2
                    (fun currentTime lastInputTime ->
                        let idleDuration =
                            match lastInputTime with
                            | ValueSome t -> currentTime - t
                            | ValueNone -> TimeSpan.Zero

                        computePulseFrame idleDuration textLength config
                    )
                    currentTimeNode
                    lastInputTimeNode

            // Map the pulse frame to the final Vdom
            incr.Map (fun pulseFrame -> make text pulseFrame config.PulseWidthChars) pulseFrameNode
