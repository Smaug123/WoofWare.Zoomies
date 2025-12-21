namespace WoofWare.Zoomies.Components

open System
open WoofWare.Zoomies

/// Component for displaying text with rainbow coloring and an idle pulse animation.
/// Each character is colored in repeating ROYGBIV pattern, and when idle for long enough,
/// a "light wave" sweeps across the text (lighter color variants).
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
            ConsoleColor.Red
            ConsoleColor.Yellow // Lighter orange
            ConsoleColor.Yellow
            ConsoleColor.Green
            ConsoleColor.Cyan // Lighter blue
            ConsoleColor.Blue // Lighter indigo
            ConsoleColor.Magenta
        |]

    /// Create rainbow-colored text with optional pulse animation.
    /// The idleDuration should come from an IdleTracker.State.IdleDuration call.
    let make (idleDuration : TimeSpan) (config : Config) (text : string) : Vdom<DesiredBounds> =
        if String.IsNullOrEmpty text then
            Vdom.styledSpans []
        else
            let textLength = text.Length

            let pulsePos =
                if idleDuration.TotalSeconds < config.IdleThresholdSeconds then
                    ValueNone
                else
                    let elapsed = idleDuration.TotalSeconds - config.IdleThresholdSeconds
                    ValueSome (int (elapsed * config.PulseSpeedCharsPerSec) % textLength)

            let spans =
                text
                |> Seq.mapi (fun i ch ->
                    let colorIdx = i % 7

                    let inPulse =
                        match pulsePos with
                        | ValueSome pos ->
                            // Characters from pos to pos + pulseWidth are in the pulse
                            let dist = (i - pos + textLength) % textLength
                            dist < config.PulseWidthChars
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
