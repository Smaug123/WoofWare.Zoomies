namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

/// An item in a multi-selection list.
type MultiSelectionItem<'id> =
    {
        /// Unique identifier for this item, for the caller's use (e.g., correlating items with external state).
        /// Not used internally by the component; the list is a single focusable unit with index-based cursor navigation.
        Id : 'id
        /// The label displayed next to the checkbox.
        Label : string
        /// Whether this item is currently selected (checked).
        IsSelected : bool
    }

[<RequireQualifiedAccess>]
type MultiSelection =

    /// Low-level rendering without framework integration.
    /// Each item specifies its own focus state (cursor highlight).
    /// Virtualizes rendering: only items within the viewport are rendered.
    /// The state is not automatically adjusted; caller is responsible for ensuring
    /// the cursor is visible via EnsureVisible.
    static member make'
        (keyPrefix : NodeKey, items : SelectionListItem'[], state : SelectionListState)
        : SelectionListResult
        =
        SelectionList.makeInternal "multi-selection" Checkbox.make' keyPrefix items state

    /// Framework-integrated version with cursor-based navigation.
    /// The list is a single focusable unit; use arrows to navigate within,
    /// Space to toggle items. Tab moves to/from the list as a whole.
    /// Virtualizes rendering: only items within the viewport are rendered.
    /// Cursor highlight only shows when the list has focus.
    ///
    /// The component posts onViewportRendered during render with the viewport height.
    /// Handle this event in ProcessWorld to call state.EnsureVisible(viewportHeight)
    /// and keep the cursor visible.
    static member make<'appEvent>
        (
            ctx : IVdomContext<'appEvent>,
            listKey : NodeKey,
            items : MultiSelectionItem<NodeKey>[],
            state : SelectionListState,
            onViewportRendered : SelectionListViewportInfo -> 'appEvent,
            ?isFirstToFocus : bool
        )
        : SelectionListResult
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

                            let checkbox = Checkbox.make' (item.IsSelected, isCursor)
                            let label = Vdom.textContent item.Label
                            [| checkbox ; label |]
                        )

                    // Use a derived key for the inner table to avoid collision with the outer focusable key
                    let tableKey = NodeKey.makeTableCellKey listKey -1 None None None
                    Table.make tableKey cells [| Column.Fixed 3 ; Column.Proportion 1.0 |] [||]

            // Register the list itself as focusable (not individual items)
            let content = Vdom.flexibleContent measure render |> Vdom.withTag "multi-selection"
            let focusable = content |> Vdom.withKey listKey

            {
                Vdom = Vdom.withFocusTracking (focusable, ?isFirstToFocus = isFirstToFocus)
                State = stateWithClampedCursor
            }
