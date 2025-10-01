namespace WoofWare.Zoomies

/// Opaque identifier for stable node identity across frames
type NodeKey = private | NodeKey of string

[<RequireQualifiedAccess>]
module NodeKey =
    let make (s : string) : NodeKey = NodeKey s

/// Phantom type to track whether a node has a key
type Keyed = private | Keyed

/// Phantom type to track whether a node lacks a key
type Unkeyed = private | Unkeyed

type Direction =
    | Vertical
    | Horizontal

type Border = | Yes

type DesiredBounds = unit

[<NoComparison>]
[<ReferenceEquality>]
type Vdom<'bounds, 'keyed> =
    private
    | Bordered of Vdom<'bounds, 'keyed>
    | PanelSplit of Direction * Choice<float, int> * child1 : Vdom<'bounds, 'keyed> * child2 : Vdom<'bounds, 'keyed>
    | TextContent of string * focused : bool
    | Checkbox of isChecked : bool * isFocused : bool
    | WithKey of NodeKey * Vdom<'bounds, Unkeyed>
    | Focusable of Vdom<'bounds, Keyed>

[<RequireQualifiedAccess>]
module Vdom =

    let textContent (isFocused : bool) (s : string) : Vdom<DesiredBounds, 'keyed> =
        Vdom.TextContent (s, isFocused)

    let panelSplitProportion d p c1 c2 : Vdom<DesiredBounds, 'keyed> =
        if not (p > 0.0 && p < 1.0) then
            invalidArg "p" "proportion must be between 0 and 1"

        Vdom.PanelSplit (d, Choice1Of2 p, c1, c2)

    let panelSplitAbsolute d p c1 c2 : Vdom<DesiredBounds, 'keyed> =
        Vdom.PanelSplit (d, Choice2Of2 p, c1, c2)

    let checkbox (isFocused : bool) (isChecked : bool) : Vdom<DesiredBounds, 'keyed> =
        Vdom.Checkbox (isChecked, isFocused)

    let bordered inner : Vdom<DesiredBounds, 'keyed> = Vdom.Bordered inner

    let labelledCheckbox
        (isFocused : bool)
        (isChecked : bool)
        (label : string)
        : Vdom<DesiredBounds, 'keyed>
        =
        // TODO: centre this text horizontally so it's next to the checkbox
        panelSplitAbsolute Direction.Vertical 3 (checkbox isFocused isChecked) (textContent false label)

    /// Attach a stable key to a VDOM node
    let withKey (key : NodeKey) (vdom : Vdom<'bounds, Unkeyed>) : Vdom<'bounds, Keyed> =
        Vdom.WithKey (key, vdom)

    /// Mark a node as focusable (requires a keyed node)
    let focusable (vdom : Vdom<'bounds, Keyed>) : Vdom<'bounds, Keyed> =
        Vdom.Focusable vdom

