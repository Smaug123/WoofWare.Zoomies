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

[<RequireQualifiedAccess>]
type MultiSelection =

    /// Low-level rendering without framework integration.
    /// Each item specifies its own focus state.
    static member make' (keyPrefix : NodeKey) (items : MultiSelectionItem'[]) : Vdom<DesiredBounds> =
        if Array.isEmpty items then
            Vdom.empty
        else
            let cells =
                items
                |> Array.map (fun item ->
                    let checkbox = Checkbox.make' (item.IsSelected, item.IsFocused)
                    let label = Vdom.textContent item.Label
                    [| checkbox ; label |]
                )

            Table.make keyPrefix cells [| Column.Fixed 3 ; Column.Proportion 1.0 |] [||]
            |> Vdom.withTag "multi-selection"

    /// Framework-integrated version with automatic focus handling.
    /// Focus state is derived from VdomContext based on each item's key.
    static member make
        (ctx : VdomContext, keyPrefix : NodeKey, items : MultiSelectionItem<NodeKey>[], ?isFirstToFocus : bool)
        : Vdom<DesiredBounds>
        =
        if Array.isEmpty items then
            Vdom.empty
        else
            let focusedKey = VdomContext.focusedKey ctx

            let cells =
                items
                |> Array.mapi (fun rowIdx item ->
                    let isFocused = focusedKey = Some item.Id

                    let checkbox =
                        Checkbox.make (
                            ctx,
                            item.Id,
                            item.IsSelected,
                            isFirstToFocus = (rowIdx = 0 && defaultArg isFirstToFocus false)
                        )

                    let label = Vdom.textContent item.Label
                    [| checkbox ; label |]
                )

            Table.make keyPrefix cells [| Column.Fixed 3 ; Column.Proportion 1.0 |] [||]
            |> Vdom.withTag "multi-selection"
