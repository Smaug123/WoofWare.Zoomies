namespace WoofWare.Zoomies

open TypeEquality

/// Opaque identifier for stable node identity across frames
type NodeKey = private | NodeKey of string

[<RequireQualifiedAccess>]
module NodeKey =
    /// Wraps an arbitrary user-chosen string node identifier into a key that WoofWare.Zoomies can use to identify
    /// nodes.
    let make (s : string) : NodeKey = NodeKey s

    /// Gets the original string that was used to construct this key.
    let toString (NodeKey s) : string = s

/// Phantom type to track whether a node has a key
type Keyed = private | Keyed

/// Phantom type to track whether a node lacks a key
type Unkeyed = private | Unkeyed

/// Specify the direction to split, when splitting a panel.
type SplitDirection =
    /// Split so that the divider runs vertically: one component is to the left and one is to the right.
    | Vertical
    /// Split so that the divider runs horizontally: one component is on top and one is on the bottom.
    | Horizontal

/// Determines how space is divided when a panel is split into two components.
[<RequireQualifiedAccess>]
type SplitBehaviour =
    /// Split using a proportion: the first component (that is, the top or left component) gets this fraction of the
    /// available space (must be between 0 and 1, exclusive).
    /// For example, Proportion 0.3 on a vertical split gives the left component 30% of the space and the right component 70%.
    | Proportion of float
    /// Split using an absolute cell count: the first component gets exactly this many cells, and the second gets the remainder.
    | Absolute of int
    /// Split based on content preferences: space is divided proportionally to each component's preferred width/height.
    | Auto

type Border = | Yes

type DesiredBounds = unit

type internal UnkeyedVdom<'bounds> =
    | Bordered of KeylessVdom<'bounds>
    | PanelSplit of SplitDirection * SplitBehaviour * child1 : KeylessVdom<'bounds> * child2 : KeylessVdom<'bounds>
    | TextContent of string * focused : bool
    | Checkbox of isChecked : bool * isFocused : bool
    | Focusable of isInitialFocus : bool * KeyedVdom<'bounds>

and internal KeyedVdom<'bounds> = | WithKey of NodeKey * UnkeyedVdom<'bounds>

and internal KeylessVdom<'bounds> =
    private
    | Keyed of KeyedVdom<'bounds>
    | Unkeyed of UnkeyedVdom<'bounds>

[<NoEquality ; NoComparison>]
type Vdom<'bounds, 'keyed> =
    private
    | Keyed of KeyedVdom<'bounds> * Teq<'keyed, Keyed>
    | Unkeyed of UnkeyedVdom<'bounds> * Teq<'keyed, Unkeyed>

[<RequireQualifiedAccess>]
module private VdomUtils =
    let teqUnreachable (_ : Teq<Keyed, Unkeyed>) : 'a = failwith "unreachable"
    let teqUnreachable' (_ : Teq<Unkeyed, Keyed>) : 'a = failwith "unreachable"

[<Sealed>]
type Vdom =

    /// <summary>Creates a text content component displaying the given string.</summary>
    /// <param name="isFocused">
    /// Specifies that this text area should render as if it has keyboard focus.
    /// This has nothing to do with the WoofWare.Zoomies automatic focus tracking system; it's purely a display concern.
    /// See <c>Vdom.withFocusTracking</c> for details.
    /// </param>
    /// <param name="s">The text to display within the text area. Text will be truncated if it doesn't fit.</param>
    /// <remarks>
    /// </remarks>
    static member textContent (isFocused : bool) (s : string) : Vdom<DesiredBounds, Unkeyed> =
        // TODO: create text areas which do smart truncation etc for you
        Vdom.Unkeyed (UnkeyedVdom.TextContent (s, isFocused), Teq.refl)

    /// <summary>Creates a split panel where two components share space according to a proportion.</summary>
    /// <remarks>
    /// The first component <c>c1</c> receives proportion <c>p</c> of the space, and the second component <c>c2</c> receives <c>1 - p</c>.
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="p">Proportion of the space to allocate to the first component (that is, the top or left one). Must be between 0 and 1, exclusive.</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component.</param>
    /// <exception cref="ArgumentException">The proportion <c>p</c> was not between 0 and 1, exclusive.</exception>
    static member panelSplitProportion
        (d, p, c1 : Vdom<DesiredBounds, Keyed>, c2 : Vdom<DesiredBounds, Keyed>)
        : Vdom<DesiredBounds, Unkeyed>
        =
        if not (p > 0.0 && p < 1.0) then
            invalidArg "p" "proportion must be between 0 and 1"

        match c1 with
        | Unkeyed (_, teq) -> VdomUtils.teqUnreachable teq
        | Keyed (c1, _) ->

        match c2 with
        | Unkeyed (_, teq) -> VdomUtils.teqUnreachable teq
        | Keyed (c2, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Proportion p, KeylessVdom.Keyed c1, KeylessVdom.Keyed c2),
            Teq.refl
        )

    /// <summary>Creates a split panel where two components share space according to a proportion.</summary>
    /// <remarks>
    /// The first component <c>c1</c> receives proportion <c>p</c> of the space, and the second component <c>c2</c> receives <c>1 - p</c>.
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="p">Proportion of the space to allocate to the first component (that is, the top or left one). Must be between 0 and 1, exclusive.</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component.</param>
    /// <exception cref="ArgumentException">The proportion <c>p</c> was not between 0 and 1, exclusive.</exception>
    static member panelSplitProportion
        (d, p, c1 : Vdom<DesiredBounds, Keyed>, c2 : Vdom<DesiredBounds, Unkeyed>)
        : Vdom<DesiredBounds, Unkeyed>
        =
        if not (p > 0.0 && p < 1.0) then
            invalidArg "p" "proportion must be between 0 and 1"

        match c1 with
        | Unkeyed (_, teq) -> VdomUtils.teqUnreachable teq
        | Keyed (c1, _) ->

        match c2 with
        | Keyed (_, teq) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c2, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Proportion p, KeylessVdom.Keyed c1, KeylessVdom.Unkeyed c2),
            Teq.refl
        )

    /// <summary>Creates a split panel where two components share space according to a proportion.</summary>
    /// <remarks>
    /// The first component <c>c1</c> receives proportion <c>p</c> of the space, and the second component <c>c2</c> receives <c>1 - p</c>.
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="p">Proportion of the space to allocate to the first component (that is, the top or left one). Must be between 0 and 1, exclusive.</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component.</param>
    /// <exception cref="ArgumentException">The proportion <c>p</c> was not between 0 and 1, exclusive.</exception>
    static member panelSplitProportion
        (d, p, c1 : Vdom<DesiredBounds, Unkeyed>, c2 : Vdom<DesiredBounds, Keyed>)
        : Vdom<DesiredBounds, Unkeyed>
        =
        if not (p > 0.0 && p < 1.0) then
            invalidArg "p" "proportion must be between 0 and 1"

        match c1 with
        | Keyed (_, teq) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c1, _) ->

        match c2 with
        | Unkeyed (_, teq) -> VdomUtils.teqUnreachable teq
        | Keyed (c2, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Proportion p, KeylessVdom.Unkeyed c1, KeylessVdom.Keyed c2),
            Teq.refl
        )

    /// <summary>Creates a split panel where two components share space according to a proportion.</summary>
    /// <remarks>
    /// The first component <c>c1</c> receives proportion <c>p</c> of the space, and the second component <c>c2</c> receives <c>1 - p</c>.
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="p">Proportion of the space to allocate to the first component (that is, the top or left one). Must be between 0 and 1, exclusive.</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component.</param>
    /// <exception cref="ArgumentException">The proportion <c>p</c> was not between 0 and 1, exclusive.</exception>
    static member panelSplitProportion
        (d, p, c1 : Vdom<DesiredBounds, Unkeyed>, c2 : Vdom<DesiredBounds, Unkeyed>)
        : Vdom<DesiredBounds, Unkeyed>
        =
        if not (p > 0.0 && p < 1.0) then
            invalidArg "p" "proportion must be between 0 and 1"

        match c1 with
        | Keyed (_, teq) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c1, _) ->

        match c2 with
        | Keyed (_, teq) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c2, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Proportion p, KeylessVdom.Unkeyed c1, KeylessVdom.Unkeyed c2),
            Teq.refl
        )

    /// <summary>Creates a split panel where one component receives a fixed number of cells.</summary>
    /// <remarks>
    /// If <c>p >= 0</c>, the first component <c>c1</c> receives exactly <c>p</c> cells, and the second component <c>c2</c> receives all remaining space.
    /// If <c>p &lt; 0</c>, the second component <c>c2</c> receives exactly <c>abs(p)</c> cells, and the first component <c>c1</c> receives all remaining space.
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="p">The number of cells to allocate. If positive, allocates to the first component; if negative, allocates abs(p) to the second component.</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component.</param>
    static member panelSplitAbsolute
        (d, p, c1 : Vdom<DesiredBounds, Keyed>, c2 : Vdom<DesiredBounds, Keyed>)
        : Vdom<DesiredBounds, Unkeyed>
        =
        match c1 with
        | Unkeyed (_, teq) -> VdomUtils.teqUnreachable teq
        | Keyed (c1, _) ->

        match c2 with
        | Unkeyed (_, teq) -> VdomUtils.teqUnreachable teq
        | Keyed (c2, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Absolute p, KeylessVdom.Keyed c1, KeylessVdom.Keyed c2),
            Teq.refl
        )

    /// <summary>Creates a split panel where one component receives a fixed number of cells.</summary>
    /// <remarks>
    /// If <c>p >= 0</c>, the first component <c>c1</c> receives exactly <c>p</c> cells, and the second component <c>c2</c> receives all remaining space.
    /// If <c>p &lt; 0</c>, the second component <c>c2</c> receives exactly <c>abs(p)</c> cells, and the first component <c>c1</c> receives all remaining space.
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="p">The number of cells to allocate. If positive, allocates to the first component; if negative, allocates abs(p) to the second component.</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component.</param>
    static member panelSplitAbsolute
        (d, p, c1 : Vdom<DesiredBounds, Unkeyed>, c2 : Vdom<DesiredBounds, Keyed>)
        : Vdom<DesiredBounds, Unkeyed>
        =
        match c1 with
        | Keyed (_, teq) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c1, _) ->

        match c2 with
        | Unkeyed (_, teq) -> VdomUtils.teqUnreachable teq
        | Keyed (c2, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Absolute p, KeylessVdom.Unkeyed c1, KeylessVdom.Keyed c2),
            Teq.refl
        )

    /// <summary>Creates a split panel where one component receives a fixed number of cells.</summary>
    /// <remarks>
    /// If <c>p >= 0</c>, the first component <c>c1</c> receives exactly <c>p</c> cells, and the second component <c>c2</c> receives all remaining space.
    /// If <c>p &lt; 0</c>, the second component <c>c2</c> receives exactly <c>abs(p)</c> cells, and the first component <c>c1</c> receives all remaining space.
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="p">The number of cells to allocate. If positive, allocates to the first component; if negative, allocates abs(p) to the second component.</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component.</param>
    static member panelSplitAbsolute
        (d, p, c1 : Vdom<DesiredBounds, Keyed>, c2 : Vdom<DesiredBounds, Unkeyed>)
        : Vdom<DesiredBounds, Unkeyed>
        =
        match c1 with
        | Unkeyed (_, teq) -> VdomUtils.teqUnreachable teq
        | Keyed (c1, _) ->

        match c2 with
        | Keyed (_, teq) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c2, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Absolute p, KeylessVdom.Keyed c1, KeylessVdom.Unkeyed c2),
            Teq.refl
        )

    /// <summary>Creates a split panel where one component receives a fixed number of cells.</summary>
    /// <remarks>
    /// If <c>p >= 0</c>, the first component <c>c1</c> receives exactly <c>p</c> cells, and the second component <c>c2</c> receives all remaining space.
    /// If <c>p &lt; 0</c>, the second component <c>c2</c> receives exactly <c>abs(p)</c> cells, and the first component <c>c1</c> receives all remaining space.
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="p">The number of cells to allocate. If positive, allocates to the first component; if negative, allocates abs(p) to the second component.</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component.</param>
    static member panelSplitAbsolute
        (d, p, c1 : Vdom<DesiredBounds, Unkeyed>, c2 : Vdom<DesiredBounds, Unkeyed>)
        : Vdom<DesiredBounds, Unkeyed>
        =
        match c1 with
        | Keyed (_, teq) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c1, _) ->

        match c2 with
        | Keyed (_, teq) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c2, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Absolute p, KeylessVdom.Unkeyed c1, KeylessVdom.Unkeyed c2),
            Teq.refl
        )

    /// <summary>Creates a split panel where components share space based on their content preferences.</summary>
    /// <remarks>
    /// Space is divided proportionally to each component's preferred width (for vertical splits) or height (for horizontal splits).
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component.</param>
    static member panelSplitAuto
        (d, c1 : Vdom<DesiredBounds, Keyed>, c2 : Vdom<DesiredBounds, Keyed>)
        : Vdom<DesiredBounds, Unkeyed>
        =
        match c1 with
        | Unkeyed (_, teq) -> VdomUtils.teqUnreachable teq
        | Keyed (c1, _) ->

        match c2 with
        | Unkeyed (_, teq) -> VdomUtils.teqUnreachable teq
        | Keyed (c2, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Auto, KeylessVdom.Keyed c1, KeylessVdom.Keyed c2),
            Teq.refl
        )

    /// <summary>Creates a split panel where components share space based on their content preferences.</summary>
    /// <remarks>
    /// Space is divided proportionally to each component's preferred width (for vertical splits) or height (for horizontal splits).
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component.</param>
    static member panelSplitAuto
        (d, c1 : Vdom<DesiredBounds, Keyed>, c2 : Vdom<DesiredBounds, Unkeyed>)
        : Vdom<DesiredBounds, Unkeyed>
        =
        match c1 with
        | Unkeyed (_, teq) -> VdomUtils.teqUnreachable teq
        | Keyed (c1, _) ->

        match c2 with
        | Keyed (_, teq) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c2, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Auto, KeylessVdom.Keyed c1, KeylessVdom.Unkeyed c2),
            Teq.refl
        )

    /// <summary>Creates a split panel where components share space based on their content preferences.</summary>
    /// <remarks>
    /// Space is divided proportionally to each component's preferred width (for vertical splits) or height (for horizontal splits).
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component.</param>
    static member panelSplitAuto
        (d, c1 : Vdom<DesiredBounds, Unkeyed>, c2 : Vdom<DesiredBounds, Keyed>)
        : Vdom<DesiredBounds, Unkeyed>
        =
        match c1 with
        | Keyed (_, teq) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c1, _) ->

        match c2 with
        | Unkeyed (_, teq) -> VdomUtils.teqUnreachable teq
        | Keyed (c2, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Auto, KeylessVdom.Unkeyed c1, KeylessVdom.Keyed c2),
            Teq.refl
        )

    /// <summary>Creates a split panel where components share space based on their content preferences.</summary>
    /// <remarks>
    /// Space is divided proportionally to each component's preferred width (for vertical splits) or height (for horizontal splits).
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component.</param>
    static member panelSplitAuto
        (d, c1 : Vdom<DesiredBounds, Unkeyed>, c2 : Vdom<DesiredBounds, Unkeyed>)
        : Vdom<DesiredBounds, Unkeyed>
        =
        match c1 with
        | Keyed (_, teq) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c1, _) ->

        match c2 with
        | Keyed (_, teq) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c2, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Auto, KeylessVdom.Unkeyed c1, KeylessVdom.Unkeyed c2),
            Teq.refl
        )

    /// <summary>Creates a checkbox component with the specified state.</summary>
    /// <param name="isFocused">
    /// Specifies that this checkbox should render as if it has keyboard focus.
    /// This has nothing to do with the WoofWare.Zoomies automatic focus tracking system; it's purely a display concern.
    /// See <c>Vdom.withFocusTracking</c> for details.
    /// </param>
    /// <param name="isChecked">Specifies that this checkbox is currently checked. Derive the value of this parameter
    /// from your application state.</param>
    static member checkbox (isFocused : bool) (isChecked : bool) : Vdom<DesiredBounds, Unkeyed> =
        Vdom.Unkeyed (UnkeyedVdom.Checkbox (isChecked, isFocused), Teq.refl)

    /// Creates a bordered wrapper around a component, drawing a border around its content.
    static member bordered (inner : Vdom<_, Keyed>) : Vdom<DesiredBounds, Unkeyed> =
        match inner with
        | Unkeyed (_, teq) -> VdomUtils.teqUnreachable teq
        | Keyed (inner, _) -> Vdom.Unkeyed (UnkeyedVdom.Bordered (KeylessVdom.Keyed inner), Teq.refl)

    /// Creates a bordered wrapper around a component, drawing a border around its content.
    static member bordered (inner : Vdom<_, Unkeyed>) : Vdom<DesiredBounds, Unkeyed> =
        match inner with
        | Keyed (_, teq) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (inner, _) -> Vdom.Unkeyed (UnkeyedVdom.Bordered (KeylessVdom.Unkeyed inner), Teq.refl)

    /// Creates a checkbox with a text label positioned to its right.
    static member labelledCheckbox
        (isFocused : bool)
        (isChecked : bool)
        (label : string)
        : Vdom<DesiredBounds, Unkeyed>
        =
        // TODO: centre this text horizontally so it's next to the checkbox
        Vdom.panelSplitAbsolute (
            SplitDirection.Vertical,
            3,
            Vdom.checkbox isFocused isChecked,
            Vdom.textContent false label
        )

    /// Attach a key to a VDOM node, effectively giving that node a name.
    ///
    /// You're free to arbitrarily reshuffle keys, reassign them to new nodes, etc., between renders.
    /// Doing so will invalidate the layout cache, but will not incur any repainting unless you also changed how
    /// something displays.
    ///
    /// The WoofWare.Zoomies framework will treat this key as being stable across frames for the purpose of e.g.
    /// automatic focus tracking. For example, if a node with key "foo" has focus on one frame, and on the next frame
    /// a different node has key "foo", focus will be on that different node.
    ///
    /// It's up to you to ensure that at most one component has a given key within a single Vdom.
    static member withKey (key : NodeKey) (vdom : Vdom<'bounds, Unkeyed>) : Vdom<'bounds, Keyed> =
        match vdom with
        | Keyed (_, teq) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (vdom, _) -> Vdom.Keyed (KeyedVdom.WithKey (key, vdom), Teq.refl)

    /// Mark a keyed node as focusable, for the purposes of the automatic focus tracking system.
    ///
    /// When the user hits TAB while automatic focus tracking is enabled, the WoofWare.Zoomies framework will
    /// cycle through tree nodes which are `focusable`.
    ///
    /// If `isInitialFocus` is true, this node will receive focus first when the system needs to select an initial
    /// focus target (e.g., on first render or when no focusable node currently has focus).
    /// At most one node should have `isInitialFocus = true` in a given VDOM tree.
    ///
    /// This annotation does nothing if WoofWare.Zoomies is running with automatic focus tracking turned off.
    static member withFocusTracking (vdom : Vdom<'bounds, Keyed>, ?isInitialFocus : bool) : Vdom<'bounds, Unkeyed> =
        let isInitialFocus = defaultArg isInitialFocus false

        match vdom with
        | Unkeyed (_, teq) -> VdomUtils.teqUnreachable teq
        | Keyed (vdom, _) -> Vdom.Unkeyed (UnkeyedVdom.Focusable (isInitialFocus, vdom), Teq.refl)

[<Sealed>]
type private KeylessVdom =
    static member referenceEquals (self : KeylessVdom<'bounds>, other : KeyedVdom<'bounds>) =
        match self with
        | KeylessVdom.Keyed vdom -> Object.referenceEquals vdom other
        | KeylessVdom.Unkeyed _ -> false

    static member referenceEquals (self : KeylessVdom<'bounds>, other : UnkeyedVdom<'bounds>) =
        match self with
        | KeylessVdom.Unkeyed vdom -> Object.referenceEquals vdom other
        | KeylessVdom.Keyed _ -> false
