namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

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
        (child : Vdom<DesiredBounds>)
        : Vdom<DesiredBounds>
        =
        let toggle =
            Toggle.make (ctx, key, '▶', '▼', state.IsExpanded)
            |> fun v -> Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 1, v, Vdom.empty)

        let spacer = Vdom.textContent false " "
        let labelVdom = Vdom.textContent false label

        let headerContent =
            Vdom.panelSplitAbsolute (
                SplitDirection.Vertical,
                3,
                toggle,
                Vdom.panelSplitAbsolute (SplitDirection.Vertical, 1, spacer, labelVdom)
            )

        if state.IsExpanded then
            Vdom.panelSplitAuto (SplitDirection.Horizontal, headerContent, child)
        else
            Vdom.panelSplitAuto (SplitDirection.Horizontal, headerContent, Vdom.empty)
        |> Vdom.withTag "collapsible"
