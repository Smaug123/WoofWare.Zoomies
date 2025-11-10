namespace WoofWare.Zoomies

/// High-level collapsible component module providing ergonomic expandable/collapsible content.
[<RequireQualifiedAccess>]
module Collapsible =

    /// State for a collapsible component.
    type State =
        {
            IsExpanded : bool
        }

        /// Return a new state with its Expanded state inverted.
        member this.ToggledExpansion () =
            {
                IsExpanded = not this.IsExpanded
            }

        static member Collapsed =
            {
                IsExpanded = false
            }

        static member Expanded =
            {
                IsExpanded = true
            }

    /// <summary>Creates a collapsible component with a toggle indicator, label, and child content.</summary>
    /// <param name="ctx">The VdomContext for focus tracking.</param>
    /// <param name="key">The NodeKey for this collapsible component.</param>
    /// <param name="state">The current state of the collapsible (expanded or collapsed).</param>
    /// <param name="label">The label text to display next to the toggle indicator.</param>
    /// <param name="child">The child content to display when expanded.</param>
    /// <remarks>
    /// When collapsed and unfocused: displays "▶ label"
    /// When collapsed and focused: displays "[▶] label"
    /// When expanded and focused: displays "[▼] label" followed by the child content
    ///
    /// Users should toggle the state (IsExpanded) in response to keystrokes when the component is focused.
    /// </remarks>
    let make
        (ctx : VdomContext)
        (key : NodeKey)
        (state : State)
        (label : string)
        (child : Vdom<DesiredBounds, Keyed>)
        : Vdom<DesiredBounds, Unkeyed>
        =
        let isFocused = VdomContext.focusedKey ctx = Some key

        let toggle =
            Vdom.toggleWithGlyph '▶' '▼' state.IsExpanded isFocused
            |> Vdom.withKey key
            |> Vdom.withFocusTracking

        let labelVdom = Vdom.textContent false label

        let headerContent =
            Vdom.panelSplitAbsolute (SplitDirection.Vertical, 3, toggle, labelVdom)

        // Always constrain header to 1 line to prevent toggle glyph from centering in excessive vertical space
        if state.IsExpanded then
            Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 1, headerContent, child)
        else
            // Constrain to 1 line even when collapsed by using a dummy bottom element
            let emptyBottom = Vdom.textContent false ""
            Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 1, headerContent, emptyBottom)
