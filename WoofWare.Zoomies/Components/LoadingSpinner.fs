namespace WoofWare.Zoomies.Components

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
