namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

/// Orientation for scroll bars.
[<RequireQualifiedAccess>]
type ScrollBarOrientation =
    /// Vertical scroll bar (thumb moves up/down).
    | Vertical
    /// Horizontal scroll bar (thumb moves left/right).
    | Horizontal

/// Parameters for creating a scroll bar, as passed to ScrollBar.make.
[<Struct>]
type ScrollBarParams =
    {
        /// Total number of scrollable items/units - those visible in the viewport plus those which
        /// are logically offscreen.
        TotalItems : int
        /// Number of items visible in the viewport right now.
        ViewportSize : int
        /// Index of the first visible item (0-based) within `TotalItems`.
        Offset : int
        /// Length of the scroll bar track in cells.
        TrackLength : int
    }

/// Scroll bar component for indicating scroll position within scrollable content.
[<RequireQualifiedAccess>]
module ScrollBar =

    /// Renders the scroll bar content for a given actual track length.
    let private renderContent
        (orientation : ScrollBarOrientation)
        (viewportSize : int)
        (totalItems : int)
        (offset : int)
        (actualTrackLength : int)
        : Vdom<DesiredBounds>
        =
        let trackLength = max 1 actualTrackLength

        if totalItems <= 0 then
            // No content: render empty track
            let track = System.String ('░', trackLength)

            match orientation with
            | ScrollBarOrientation.Horizontal -> Vdom.textContent (track, wrap = false)
            | ScrollBarOrientation.Vertical ->
                let lines = Array.create trackLength "░"
                Vdom.textContent (System.String.Join ("\n", lines), wrap = false)
        else if totalItems <= viewportSize then
            // All content visible: render full thumb
            let thumb = System.String ('█', trackLength)

            match orientation with
            | ScrollBarOrientation.Horizontal -> Vdom.textContent (thumb, wrap = false)
            | ScrollBarOrientation.Vertical ->
                let lines = Array.create trackLength "█"
                Vdom.textContent (System.String.Join ("\n", lines), wrap = false)
        else
            // Normal scrolling case
            let offset = max 0 (min offset (totalItems - viewportSize))

            // Calculate thumb size: proportional to viewport/total ratio, minimum 1
            let thumbRatio = float viewportSize / float totalItems
            let thumbSize = max 1 (int (thumbRatio * float trackLength))

            // Calculate thumb position: proportional to offset within scrollable range
            let scrollableRange = totalItems - viewportSize
            let thumbPositionRatio = float offset / float scrollableRange
            // Ensure thumb stays within track bounds
            let maxThumbStart = trackLength - thumbSize
            let thumbStart = int (thumbPositionRatio * float maxThumbStart)
            let thumbStart = max 0 (min thumbStart maxThumbStart)

            match orientation with
            | ScrollBarOrientation.Horizontal ->
                let chars =
                    [|
                        for i in 0 .. trackLength - 1 do
                            if i >= thumbStart && i < thumbStart + thumbSize then
                                '█'
                            else
                                '░'
                    |]

                Vdom.textContent (System.String chars, wrap = false)
            | ScrollBarOrientation.Vertical ->
                let lines =
                    [|
                        for i in 0 .. trackLength - 1 do
                            if i >= thumbStart && i < thumbStart + thumbSize then
                                "█"
                            else
                                "░"
                    |]

                Vdom.textContent (System.String.Join ("\n", lines), wrap = false)

    /// <summary>Creates a scroll bar component.</summary>
    /// <param name="orientation">Whether the scroll bar is vertical or horizontal.</param>
    /// <param name="scrollParams">Scroll bar parameters.</param>
    /// <remarks>
    /// The scroll bar uses Unicode block characters:
    /// - Track: ░ (U+2591)
    /// - Thumb: █ (U+2588)
    ///
    /// When totalItems is less than or equal to viewportSize (no scrolling needed),
    /// the entire track is shown as thumb to indicate all content is visible.
    ///
    /// The scroll bar adapts to the actual available space. If the rendering area is
    /// smaller than the requested TrackLength, the scroll bar will use the actual
    /// available width (horizontal) or height (vertical) for thumb positioning.
    ///
    /// Invalid inputs are handled gracefully:
    /// - Non-positive totalItems: renders empty track
    /// - Non-positive viewportSize: treated as 1
    /// - Negative offset: treated as 0
    /// - Offset beyond content: clamped to valid range
    /// - Non-positive trackLength: treated as 1
    /// </remarks>
    let make (orientation : ScrollBarOrientation) (scrollParams : ScrollBarParams) : Vdom<DesiredBounds> =
        let requestedTrackLength = max 1 scrollParams.TrackLength
        let viewportSize = max 1 scrollParams.ViewportSize
        let totalItems = scrollParams.TotalItems
        let offset = scrollParams.Offset

        let measure (constraints : MeasureConstraints) : MeasuredSize =
            match orientation with
            | ScrollBarOrientation.Horizontal ->
                {
                    MinWidth = 1
                    PreferredWidth = min requestedTrackLength constraints.MaxWidth
                    MaxWidth = None
                    MinHeightForWidth = fun _ -> 1
                    PreferredHeightForWidth = fun _ -> 1
                    MaxHeightForWidth = fun _ -> Some 1
                }
            | ScrollBarOrientation.Vertical ->
                let preferredHeight = min requestedTrackLength constraints.MaxHeight

                {
                    MinWidth = 1
                    PreferredWidth = 1
                    MaxWidth = Some 1
                    MinHeightForWidth = fun _ -> 1
                    PreferredHeightForWidth = fun _ -> preferredHeight
                    MaxHeightForWidth = fun _ -> None
                }

        let render (rect : Rectangle) : Vdom<DesiredBounds> =
            let availableSpace =
                match orientation with
                | ScrollBarOrientation.Horizontal -> rect.Width
                | ScrollBarOrientation.Vertical -> rect.Height

            // Use the minimum of requested track length and available space
            let actualTrackLength = min requestedTrackLength availableSpace

            renderContent orientation viewportSize totalItems offset actualTrackLength

        Vdom.flexibleContent measure render |> Vdom.withTag "scroll-bar"
