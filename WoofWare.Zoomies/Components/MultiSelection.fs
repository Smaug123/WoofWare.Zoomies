namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

/// An item in a multi-selection list.
type MultiSelectionItem<'id> =
    {
        /// Unique identifier for this item, used for focus tracking and activation.
        Id : 'id
        /// The label displayed next to the checkbox.
        Label : string
        /// Whether this item is currently selected (checked).
        IsSelected : bool
    }

/// An item in a multi-selection list with explicit focus state (for low-level rendering).
type MultiSelectionItem' =
    {
        /// The label displayed next to the checkbox.
        Label : string
        /// Whether this item is currently selected (checked).
        IsSelected : bool
        /// Whether this item is currently focused.
        IsFocused : bool
    }

/// State for a multi-selection list with cursor navigation.
/// The cursor is the currently highlighted item for arrow-key navigation.
/// Scroll offset determines which items are visible in the viewport.
[<Struct>]
type MultiSelectionState =
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
    member this.MoveUp (itemCount : int) : MultiSelectionState =
        if itemCount <= 0 then
            this
        else
            {
                ScrollOffset = this.ScrollOffset
                CursorIndex = max 0 (this.CursorIndex - 1)
            }

    /// Move the cursor down by one, clamping to valid range.
    member this.MoveDown (itemCount : int) : MultiSelectionState =
        if itemCount <= 0 then
            this
        else
            {
                ScrollOffset = this.ScrollOffset
                CursorIndex = min (itemCount - 1) (this.CursorIndex + 1)
            }

    /// Adjust scroll offset to ensure the cursor is visible within viewportHeight items.
    /// Uses minimal scrolling: only scrolls if the cursor is outside the current viewport.
    member this.EnsureVisible (viewportHeight : int) : MultiSelectionState =
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

/// Result from rendering a multi-selection list.
type MultiSelectionResult =
    {
        /// The rendered Vdom.
        Vdom : Vdom<DesiredBounds>
        /// The (potentially updated) state - scroll may adjust to keep cursor visible.
        State : MultiSelectionState
    }

[<RequireQualifiedAccess>]
type MultiSelection =

    /// Low-level rendering without framework integration.
    /// Each item specifies its own focus state (cursor highlight).
    /// Virtualizes rendering: only items within the viewport are rendered.
    /// The state is not automatically adjusted; caller is responsible for ensuring
    /// the cursor is visible via EnsureVisible.
    static member make'
        (keyPrefix : NodeKey, items : MultiSelectionItem'[], state : MultiSelectionState)
        : MultiSelectionResult
        =
        if Array.isEmpty items then
            {
                Vdom = Vdom.empty
                State = state
            }
        else
            let totalItems = items.Length

            let measure (constraints : MeasureConstraints) : MeasuredSize =
                // Each item is 1 line tall, 3 chars for checkbox + label
                {
                    MinWidth = 4 // Minimum: checkbox (3) + at least 1 char
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
                            let checkbox = Checkbox.make' (item.IsSelected, item.IsFocused)
                            let label = Vdom.textContent item.Label
                            [| checkbox ; label |]
                        )

                    Table.make keyPrefix cells [| Column.Fixed 3 ; Column.Proportion 1.0 |] [||]

            {
                Vdom = Vdom.flexibleContent measure render |> Vdom.withTag "multi-selection"
                State = state
            }

    /// Framework-integrated version with cursor-based navigation.
    /// The list is a single focusable unit; use arrows to navigate within,
    /// Space to toggle items. Tab moves to/from the list as a whole.
    /// Automatically scrolls to keep the cursor visible (minimal scroll).
    /// Virtualizes rendering: only items within the viewport are rendered.
    /// Cursor highlight only shows when the list has focus.
    static member make
        (
            ctx : VdomContext,
            listKey : NodeKey,
            items : MultiSelectionItem<NodeKey>[],
            state : MultiSelectionState,
            ?isFirstToFocus : bool
        )
        : MultiSelectionResult
        =
        if Array.isEmpty items then
            {
                Vdom = Vdom.empty
                State = state
            }
        else
            let totalItems = items.Length
            let listHasFocus = VdomContext.focusedKey ctx = Some listKey
            // Clamp cursor to valid range
            let cursorIndex = max 0 (min state.CursorIndex (totalItems - 1))

            let measure (constraints : MeasureConstraints) : MeasuredSize =
                // Each item is 1 line tall, 3 chars for checkbox + label
                {
                    MinWidth = 4 // Minimum: checkbox (3) + at least 1 char
                    PreferredWidth = constraints.MaxWidth // Take available width
                    MaxWidth = None
                    MinHeightForWidth = fun _ -> 1 // Show at least 1 item
                    PreferredHeightForWidth = fun _ -> totalItems // Prefer to show all
                    MaxHeightForWidth = fun _ -> Some totalItems // Don't grow beyond what we need
                }

            // We need to capture the adjusted state from the render pass.
            // Since render is called during layout, we use a mutable ref to capture the result.
            let stateWithClampedCursor =
                {
                    ScrollOffset = state.ScrollOffset
                    CursorIndex = cursorIndex
                }

            let adjustedState = ref stateWithClampedCursor

            let render (rect : Rectangle) : Vdom<DesiredBounds> =
                let viewportHeight = rect.Height

                // Auto-scroll to keep cursor visible
                let stateAfterAutoScroll = stateWithClampedCursor.EnsureVisible viewportHeight

                // Clamp scroll offset to valid range
                let maxOffset = max 0 (totalItems - viewportHeight)
                let offset = max 0 (min stateAfterAutoScroll.ScrollOffset maxOffset)

                adjustedState.Value <-
                    {
                        ScrollOffset = offset
                        CursorIndex = cursorIndex
                    }

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

                            let checkbox = Checkbox.make' (item.IsSelected, isCursor)
                            let label = Vdom.textContent item.Label
                            [| checkbox ; label |]
                        )

                    Table.make listKey cells [| Column.Fixed 3 ; Column.Proportion 1.0 |] [||]

            // Register the list itself as focusable (not individual items)
            let content = Vdom.flexibleContent measure render |> Vdom.withTag "multi-selection"
            let focusable = content |> Vdom.withKey listKey

            {
                Vdom = Vdom.withFocusTracking (focusable, ?isFirstToFocus = isFirstToFocus)
                State = adjustedState.Value
            }
