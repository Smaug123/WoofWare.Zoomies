namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

/// An item in a selection list with explicit focus and selection state (for low-level rendering).
type SelectionListItem' =
    {
        /// The label displayed next to the toggle.
        Label : string
        /// Whether this item is currently selected.
        IsSelected : bool
        /// Whether this item is currently focused (has cursor).
        IsFocused : bool
    }

/// State for a selection list with cursor navigation.
/// The cursor is the currently highlighted item for arrow-key navigation.
/// Scroll offset determines which items are visible in the viewport.
[<Struct>]
type SelectionListState =
    {
        /// Index of the first visible item (0-based).
        ScrollOffset : int
        /// Index of the cursor (highlighted item for arrow navigation).
        CursorIndex : int
    }

    /// Create state with cursor at the beginning.
    static member AtStart =
        {
            ScrollOffset = 0
            CursorIndex = 0
        }

    /// Create state with cursor at a specific index.
    static member AtIndex (index : int) =
        {
            ScrollOffset = 0
            CursorIndex = max 0 index
        }

    /// Create state with scroll offset at a specific position (cursor at 0).
    static member AtOffset (offset : int) =
        {
            ScrollOffset = max 0 offset
            CursorIndex = 0
        }

    /// Move the cursor up by one, clamping to valid range.
    member this.MoveUp (itemCount : int) : SelectionListState =
        if itemCount <= 0 then
            this
        else
            {
                ScrollOffset = this.ScrollOffset
                CursorIndex = max 0 (this.CursorIndex - 1)
            }

    /// Move the cursor down by one, clamping to valid range.
    member this.MoveDown (itemCount : int) : SelectionListState =
        if itemCount <= 0 then
            this
        else
            {
                ScrollOffset = this.ScrollOffset
                CursorIndex = min (itemCount - 1) (this.CursorIndex + 1)
            }

    /// Adjust scroll offset to ensure the cursor is visible within viewportHeight items.
    /// Uses minimal scrolling: only scrolls if the cursor is outside the current viewport.
    member this.EnsureVisible (viewportHeight : int) : SelectionListState =
        if viewportHeight <= 0 then
            this
        else
            let offset = this.ScrollOffset
            let itemIndex = this.CursorIndex

            if itemIndex < offset then
                // Cursor is above viewport - scroll up so cursor is at top
                {
                    ScrollOffset = itemIndex
                    CursorIndex = this.CursorIndex
                }
            elif itemIndex >= offset + viewportHeight then
                // Cursor is below viewport - scroll down so cursor is at bottom
                {
                    ScrollOffset = itemIndex - viewportHeight + 1
                    CursorIndex = this.CursorIndex
                }
            else
                // Cursor already visible - no change
                this

/// Information about the viewport that a selection list rendered into.
/// Posted as an event during render so callers can adjust scroll offset.
[<Struct>]
type SelectionListViewportInfo =
    {
        /// The height of the viewport in items.
        ViewportHeight : int
    }

/// Result from rendering a selection list.
type SelectionListResult =
    {
        /// The rendered Vdom.
        Vdom : Vdom<DesiredBounds>
        /// The state with clamped cursor. Note: scroll adjustment based on viewport
        /// height is communicated via PostLayoutEvent, not via this field.
        State : SelectionListState
    }

[<RequireQualifiedAccess>]
module SelectionList =

    /// Internal low-level rendering shared by MultiSelection and SingleSelection.
    /// Parameterized by the toggle renderer (Checkbox.make' or RadioButton.make').
    let internal makeInternal
        (tag : string)
        (makeToggle : bool * bool -> Vdom<DesiredBounds>)
        (keyPrefix : NodeKey)
        (items : SelectionListItem'[])
        (state : SelectionListState)
        : SelectionListResult
        =
        if Array.isEmpty items then
            {
                Vdom = Vdom.empty
                State = state
            }
        else
            let totalItems = items.Length

            let measure (constraints : MeasureConstraints) : MeasuredSize =
                // Each item is 1 line tall, 3 chars for toggle + label
                {
                    MinWidth = 4 // Minimum: toggle (3) + at least 1 char
                    PreferredWidth = constraints.MaxWidth // Take available width
                    MaxWidth = None
                    MinHeightForWidth = fun _ -> 1 // Show at least 1 item
                    PreferredHeightForWidth = fun _ -> totalItems // Prefer to show all
                    MaxHeightForWidth = fun _ -> Some totalItems // Don't grow beyond what we need
                }

            let render (rect : Rectangle) : Vdom<DesiredBounds> =
                let viewportHeight = rect.Height
                // Clamp scroll offset to valid range
                let maxOffset = max 0 (totalItems - viewportHeight)
                let offset = max 0 (min state.ScrollOffset maxOffset)
                let visibleCount = min viewportHeight (totalItems - offset)

                if visibleCount <= 0 then
                    Vdom.empty
                else
                    // Only create cells for visible items
                    let visibleItems = items |> Array.skip offset |> Array.take visibleCount

                    let cells =
                        visibleItems
                        |> Array.map (fun item ->
                            let toggle = makeToggle (item.IsSelected, item.IsFocused)
                            let label = Vdom.textContent item.Label
                            [| toggle ; label |]
                        )

                    Table.make keyPrefix cells [| Column.Fixed 3 ; Column.Proportion 1.0 |] [||]

            {
                Vdom = Vdom.flexibleContent measure render |> Vdom.withTag tag
                State = state
            }
