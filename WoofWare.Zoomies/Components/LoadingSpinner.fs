namespace WoofWare.Zoomies.Components

open WoofWare.Incremental
open WoofWare.Zoomies

/// Simple loading spinner component that displays a single animated character.
[<RequireQualifiedAccess>]
module LoadingSpinner =

    /// The spinner frames (braille dots).
    let Frames = [| '⠋' ; '⠙' ; '⠹' ; '⠸' ; '⠼' ; '⠴' ; '⠦' ; '⠧' ; '⠇' ; '⠏' |]

    /// Number of frames in the animation.
    let FrameCount = Frames.Length

    /// <summary>Creates a loading spinner showing the given frame.</summary>
    /// <param name="frame">Frame index. Taken modulo FrameCount, so any int works (including negative values).</param>
    let make (frame : int) : Vdom<DesiredBounds> =
        let frameIndex =
            let m = frame % FrameCount

            if m < 0 then m + FrameCount else m

        let char = Frames.[frameIndex]
        Vdom.textContent (string<char> char) |> Vdom.withTag "loading-spinner"

    /// <summary>Creates an incremental loading spinner that updates automatically based on the clock.</summary>
    /// <param name="incr">The Incremental instance for creating nodes.</param>
    /// <param name="clock">The clock to use for time-based animation.</param>
    /// <param name="fps">Frames per second for the animation (typically 10-15 for a smooth spinner).</param>
    /// <returns>A Node that produces a new Vdom each time the frame changes.</returns>
    let makeIncr (incr : Incremental) (clock : Clock) (fps : float) : Vdom<DesiredBounds> Node =
        let frameNode = IncrTime.spinnerFrameNode incr clock FrameCount fps
        incr.Map make frameNode
