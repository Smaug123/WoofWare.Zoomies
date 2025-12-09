namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

/// An item in a single-selection list.
type SingleSelectionItem<'id> =
    {
        /// Unique identifier for this item, for the caller's use (e.g., correlating items with external state).
        /// Not used internally by the component; the list is a single focusable unit with index-based cursor navigation.
        Id : 'id
        /// The label displayed next to the radio button.
        Label : string
    }

/// An item in a single-selection list with explicit focus and selection state (for low-level rendering).
type SingleSelectionItem' =
    {
        /// The label displayed next to the radio button.
        Label : string
        /// Whether this item is currently selected.
        IsSelected : bool
        /// Whether this item is currently focused (has cursor).
        IsFocused : bool
    }

/// State for a single-selection list with cursor navigation.
/// The cursor is the currently highlighted item for arrow-key navigation.
/// Scroll offset determines which items are visible in the viewport.
[<Struct>]
type SingleSelectionState =
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
    member this.MoveUp (itemCount : int) : SingleSelectionState =
        if itemCount <= 0 then
            this
        else
            {
                ScrollOffset = this.ScrollOffset
                CursorIndex = max 0 (this.CursorIndex - 1)
            }

    /// Move the cursor down by one, clamping to valid range.
    member this.MoveDown (itemCount : int) : SingleSelectionState =
        if itemCount <= 0 then
            this
        else
            {
                ScrollOffset = this.ScrollOffset
                CursorIndex = min (itemCount - 1) (this.CursorIndex + 1)
            }

    /// Adjust scroll offset to ensure the cursor is visible within viewportHeight items.
    /// Uses minimal scrolling: only scrolls if the cursor is outside the current viewport.
    member this.EnsureVisible (viewportHeight : int) : SingleSelectionState =
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

/// Information about the viewport that the single-selection list rendered into.
/// Posted as an event during render so callers can adjust scroll offset.
[<Struct>]
type SingleSelectionViewportInfo =
    {
        /// The height of the viewport in items.
        ViewportHeight : int
    }

/// Result from rendering a single-selection list.
type SingleSelectionResult =
    {
        /// The rendered Vdom.
        Vdom : Vdom<DesiredBounds>
        /// The state with clamped cursor. Note: scroll adjustment based on viewport
        /// height is communicated via PostLayoutEvent, not via this field.
        State : SingleSelectionState
    }

[<RequireQualifiedAccess>]
type SingleSelection =

    /// Low-level rendering without framework integration.
    /// Each item specifies its own focus and selection state.
    /// Virtualizes rendering: only items within the viewport are rendered.
    /// The state is not automatically adjusted; caller is responsible for ensuring
    /// the cursor is visible via EnsureVisible.
    static member make'
        (keyPrefix : NodeKey, items : SingleSelectionItem'[], state : SingleSelectionState)
        : SingleSelectionResult
        =
        if Array.isEmpty items then
            {
                Vdom = Vdom.empty
                State = state
            }
        else
            let totalItems = items.Length

            let measure (constraints : MeasureConstraints) : MeasuredSize =
                // Each item is 1 line tall, 3 chars for radio button + label
                {
                    MinWidth = 4 // Minimum: radio button (3) + at least 1 char
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
                            let radioButton = RadioButton.make' (item.IsSelected, item.IsFocused)
                            let label = Vdom.textContent item.Label
                            [| radioButton ; label |]
                        )

                    Table.make keyPrefix cells [| Column.Fixed 3 ; Column.Proportion 1.0 |] [||]

            {
                Vdom = Vdom.flexibleContent measure render |> Vdom.withTag "single-selection"
                State = state
            }

    /// Framework-integrated version with cursor-based navigation.
    /// The list is a single focusable unit; use arrows to navigate within,
    /// Space to select an item. Tab moves to/from the list as a whole.
    /// Virtualizes rendering: only items within the viewport are rendered.
    /// Cursor highlight only shows when the list has focus.
    ///
    /// The component posts onViewportRendered during render with the viewport height.
    /// Handle this event in ProcessWorld to call state.EnsureVisible(viewportHeight)
    /// and keep the cursor visible.
    ///
    /// Note: This component uses the same ActionResolver as MultiSelection
    /// (ActivationResolver.multiSelection). The difference is semantic: in MultiSelection,
    /// Space toggles the item; in SingleSelection, Space selects the item (your ProcessWorld
    /// handler should set the selection to the cursor index rather than toggling).
    static member make<'appEvent>
        (
            ctx : IVdomContext<'appEvent>,
            listKey : NodeKey,
            items : SingleSelectionItem<NodeKey>[],
            selectedIndex : int option,
            state : SingleSelectionState,
            onViewportRendered : SingleSelectionViewportInfo -> 'appEvent,
            ?isFirstToFocus : bool
        )
        : SingleSelectionResult
        =
        if Array.isEmpty items then
            {
                Vdom = Vdom.empty
                State = state
            }
        else
            let totalItems = items.Length
            let listHasFocus = ctx.FocusedKey = Some listKey
            // Clamp cursor to valid range
            let cursorIndex = max 0 (min state.CursorIndex (totalItems - 1))
            // Clamp selectedIndex to valid range
            let selectedIndex =
                selectedIndex |> Option.map (fun i -> max 0 (min i (totalItems - 1)))

            let measure (constraints : MeasureConstraints) : MeasuredSize =
                // Each item is 1 line tall, 3 chars for radio button + label
                {
                    MinWidth = 4 // Minimum: radio button (3) + at least 1 char
                    PreferredWidth = constraints.MaxWidth // Take available width
                    MaxWidth = None
                    MinHeightForWidth = fun _ -> 1 // Show at least 1 item
                    PreferredHeightForWidth = fun _ -> totalItems // Prefer to show all
                    MaxHeightForWidth = fun _ -> Some totalItems // Don't grow beyond what we need
                }

            let stateWithClampedCursor =
                {
                    ScrollOffset = state.ScrollOffset
                    CursorIndex = cursorIndex
                }

            let render (rect : Rectangle) : Vdom<DesiredBounds> =
                let viewportHeight = rect.Height

                // Post the viewport height so the caller can call EnsureVisible
                ctx.PostLayoutEvent (
                    onViewportRendered
                        {
                            ViewportHeight = viewportHeight
                        }
                )

                // Clamp scroll offset to valid range for rendering
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
                        |> Array.mapi (fun visibleIdx item ->
                            let globalIdx = offset + visibleIdx
                            // Show cursor highlight only when list has focus AND this is the cursor item
                            let isCursor = listHasFocus && globalIdx = cursorIndex
                            // Show selection indicator for the selected item
                            let isSelected = selectedIndex = Some globalIdx

                            let radioButton = RadioButton.make' (isSelected, isCursor)
                            let label = Vdom.textContent item.Label
                            [| radioButton ; label |]
                        )

                    // Use a derived key for the inner table to avoid collision with the outer focusable key
                    let tableKey = NodeKey.makeTableCellKey listKey -1 None None None
                    Table.make tableKey cells [| Column.Fixed 3 ; Column.Proportion 1.0 |] [||]

            // Register the list itself as focusable (not individual items)
            let content = Vdom.flexibleContent measure render |> Vdom.withTag "single-selection"
            let focusable = content |> Vdom.withKey listKey

            {
                Vdom = Vdom.withFocusTracking (focusable, ?isFirstToFocus = isFirstToFocus)
                State = stateWithClampedCursor
            }
