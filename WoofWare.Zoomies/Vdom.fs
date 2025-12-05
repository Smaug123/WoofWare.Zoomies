namespace WoofWare.Zoomies

/// Global configuration for the tagging system.
[<RequireQualifiedAccess>]
module VdomTagging =
    /// When false, Vdom.withTag is a no-op and no allocations occur.
    /// Defaults to false. Set to true before constructing VDOMs to enable tagging.
    let mutable Enabled : bool = false

/// Opaque identifier for stable node identity across frames
type NodeKey =
    private
    | Custom of string
    | Table of NodeKey * row : int * toRow : int option * col : int option * toCol : int option

[<RequireQualifiedAccess>]
module NodeKey =
    /// Wraps an arbitrary user-chosen string node identifier into a key that WoofWare.Zoomies can use to identify
    /// nodes.
    let make (s : string) : NodeKey = NodeKey.Custom s

    /// Indicate a sub-key
    let makeTableCellKey (table : NodeKey) (row : int) (toRow : int option) (col : int option) (toCol : int option) =
        NodeKey.Table (table, row, toRow, col, toCol)

    /// Gets a string that represents this key for human display.
    let rec toHumanReadableString (s : NodeKey) : string =
        match s with
        | NodeKey.Custom s -> s
        | NodeKey.Table (nodeKey, row, toRow, col, toCol) ->
            let toRow =
                match toRow with
                | None -> ""
                | Some toRow -> $"to%i{toRow}"

            let toCol =
                match toCol with
                | None -> ""
                | Some toCol -> $"to%i{toCol}"

            let col =
                match col with
                | None -> ""
                | Some col -> $"_%i{col}%s{toCol}"

            $"%s{toHumanReadableString nodeKey}_%i{row}%s{toRow}%s{col}"

/// Specify the direction to split, when splitting a panel.
type SplitDirection =
    /// Split so that the divider runs vertically: one component is to the left and one is to the right.
    | Vertical
    /// Split so that the divider runs horizontally: one component is on top and one is on the bottom.
    | Horizontal

/// Determines how a component in an auto-weighted split should absorb excess space.
[<RequireQualifiedAccess>]
type ExpansionWeight =
    /// Use the component's preferred width/height as its weight for absorbing excess space.
    /// Excess is distributed proportionally to content preferences.
    | FromContent
    /// Use an explicit weight for absorbing excess space.
    /// A weight of 0.0 means "don't expand beyond preferred size".
    /// Positive weights distribute excess proportionally among components with positive weights.
    | Fixed of float

/// Determines how space is divided when a panel is split into two components.
[<RequireQualifiedAccess>]
type SplitBehaviour =
    /// Split using a proportion: the first component (that is, the top or left component) gets this fraction of the
    /// available space (must be between 0 and 1, exclusive).
    /// For example, Proportion 0.3 on a vertical split gives the left component 30% of the space and the right component 70%.
    | Proportion of float
    /// Split using an absolute cell count: the first component gets exactly this many cells, and the second gets the remainder.
    | Absolute of int
    /// Split based on content preferences, with configurable excess space distribution.
    /// Each component receives at least its preferred size. Excess space is distributed according to the weights.
    | AutoWeighted of weight1 : ExpansionWeight * weight2 : ExpansionWeight

type Border = | Yes

type DesiredBounds = unit

/// Specifies how content should be aligned within its bounds.
type ContentAlignment =
    /// Content is centered both horizontally and vertically.
    | Centered
    /// Content starts at the top-left corner and wraps.
    | TopLeft

type internal FlexibleContent =
    {
        Measure : MeasureConstraints -> MeasuredSize
        Render : Rectangle -> Vdom<DesiredBounds>
    }

and [<NoEquality ; NoComparison>] Vdom<'bounds> =
    | Keyed of KeyedVdom<'bounds>
    | Unkeyed of UnkeyedVdom<'bounds>

and KeyedVdom<'bounds> = internal | KeyedVdom of NodeKey * UnkeyedVdom<'bounds>

and UnkeyedVdom<'bounds> =
    private
    | Bordered of Vdom<'bounds>
    | PanelSplit of SplitDirection * SplitBehaviour * child1 : Vdom<'bounds> * child2 : Vdom<'bounds>
    | TextContent of content : string * style : CellStyle * alignment : ContentAlignment * focused : bool
    | Focusable of isFirstToFocus : bool * isInitiallyFocused : bool * KeyedVdom<'bounds>
    | Empty
    | FlexibleContent of FlexibleContent
    | Tag of tag : string * inner : Vdom<'bounds>

[<Sealed>]
type Vdom =

    /// A singleton.
    static member internal emptyUnkeyed : UnkeyedVdom<DesiredBounds> = UnkeyedVdom.Empty

    /// <summary>Creates a text content component displaying the given string.</summary>
    /// <param name="isFocused">
    /// Specifies that this text area should render as if it has keyboard focus.
    /// This has nothing to do with the WoofWare.Zoomies automatic focus tracking system; it's purely a display concern.
    /// See <c>Vdom.withFocusTracking</c> for details.
    /// </param>
    /// <param name="s">The text to display within the text area. Text will be truncated if it doesn't fit.</param>
    /// <param name="style">How the text should render.</param>
    /// <param name="alignment">Where in the available area to place the text.</param>
    static member textContent
        (s : string, ?isFocused : bool, ?style : CellStyle, ?alignment : ContentAlignment)
        : Vdom<DesiredBounds>
        =
        Vdom.textContent' (s, ?isFocused = isFocused, ?style = style, ?alignment = alignment)
        |> Vdom.Unkeyed

    /// This is `Vdom.textContent`, but you get back an UnkeyedVdom rather than a Vdom.
    static member textContent'
        (s : string, ?isFocused : bool, ?style : CellStyle, ?alignment : ContentAlignment)
        : UnkeyedVdom<DesiredBounds>
        =
        // TODO: create text areas which do smart truncation etc for you
        let style =
            match style with
            | None -> CellStyle.none
            | Some s -> s

        let alignment = defaultArg alignment ContentAlignment.TopLeft
        let isFocused = defaultArg isFocused false
        UnkeyedVdom.TextContent (s, style, alignment, isFocused)

    /// <summary>Creates a text content component with explicit styling.</summary>
    /// <param name="content">The text to display.</param>
    /// <param name="style">The cell styling to apply to the text.</param>
    /// <param name="alignment">Where within the panel to place the text.</param>
    static member styledText
        (content : string, style : CellStyle, ?alignment : ContentAlignment)
        : Vdom<DesiredBounds>
        =
        Vdom.Unkeyed (UnkeyedVdom.TextContent (content, style, defaultArg alignment ContentAlignment.TopLeft, false))

    /// <summary>Creates an empty zero-sized element.</summary>
    /// <remarks>
    /// This element takes up no space and is useful for positioning content within split panels.
    /// For example, use <c>Vdom.panelSplitAbsolute</c> with a negative width and <c>Vdom.empty</c> to push content
    /// to the right edge of a panel.
    /// </remarks>
    static member empty : Vdom<DesiredBounds> = Vdom.Unkeyed UnkeyedVdom.Empty

    /// You might want to use `Vdom.panelSplit{Proportion,Auto,Absolute}` instead; this is the raw primitive.
    static member panelSplit
        (d : SplitDirection, b : SplitBehaviour, c1 : Vdom<DesiredBounds>, c2 : Vdom<DesiredBounds>)
        =
        UnkeyedVdom.PanelSplit (d, b, c1, c2)

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
        (d, p, c1 : Vdom<DesiredBounds>, c2 : Vdom<DesiredBounds>)
        : Vdom<DesiredBounds>
        =
        if not (p > 0.0 && p < 1.0) then
            invalidArg "p" "proportion must be between 0 and 1"

        Vdom.panelSplit (d, SplitBehaviour.Proportion p, c1, c2) |> Vdom.Unkeyed

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
        (d : SplitDirection, p : float, c1 : KeyedVdom<DesiredBounds>, c2 : KeyedVdom<DesiredBounds>)
        =
        Vdom.panelSplitProportion (d, p, Vdom.Keyed c1, Vdom.Keyed c2)

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
        (d : SplitDirection, p : float, c1 : Vdom<DesiredBounds>, c2 : KeyedVdom<DesiredBounds>)
        =
        Vdom.panelSplitProportion (d, p, c1, Vdom.Keyed c2)

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
        (d : SplitDirection, p : float, c1 : KeyedVdom<DesiredBounds>, c2 : Vdom<DesiredBounds>)
        =
        Vdom.panelSplitProportion (d, p, Vdom.Keyed c1, c2)

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
    static member panelSplitAbsolute (d, p, c1 : Vdom<DesiredBounds>, c2 : Vdom<DesiredBounds>) : Vdom<DesiredBounds> =
        Vdom.panelSplit (d, SplitBehaviour.Absolute p, c1, c2) |> Vdom.Unkeyed

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
    static member panelSplitAbsolute (d, p, c1 : KeyedVdom<DesiredBounds>, c2 : KeyedVdom<DesiredBounds>) =
        Vdom.panelSplitAbsolute (d, p, Vdom.Keyed c1, Vdom.Keyed c2)

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
    static member panelSplitAbsolute (d, p, c1 : Vdom<DesiredBounds>, c2 : KeyedVdom<DesiredBounds>) =
        Vdom.panelSplitAbsolute (d, p, c1, Vdom.Keyed c2)

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
    static member panelSplitAbsolute (d, p, c1 : KeyedVdom<DesiredBounds>, c2 : Vdom<DesiredBounds>) =
        Vdom.panelSplitAbsolute (d, p, Vdom.Keyed c1, c2)

    /// <summary>Creates a split panel where components share space based on their content preferences.</summary>
    /// <remarks>
    /// Space is divided proportionally to each component's preferred width (for vertical splits) or height (for horizontal splits).
    /// Both excess space and insufficient space are distributed proportionally to content preferences.
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component.</param>
    static member panelSplitAuto (d, c1 : Vdom<DesiredBounds>, c2 : Vdom<DesiredBounds>) : Vdom<DesiredBounds> =
        Vdom.panelSplit (
            d,
            SplitBehaviour.AutoWeighted (ExpansionWeight.FromContent, ExpansionWeight.FromContent),
            c1,
            c2
        )
        |> Vdom.Unkeyed

    /// <summary>Creates a split panel where components share space based on their content preferences.</summary>
    /// <remarks>
    /// Space is divided proportionally to each component's preferred width (for vertical splits) or height (for horizontal splits).
    /// Both excess space and insufficient space are distributed proportionally to content preferences.
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component.</param>
    static member panelSplitAuto (d, c1 : KeyedVdom<DesiredBounds>, c2 : Vdom<DesiredBounds>) : Vdom<DesiredBounds> =
        Vdom.panelSplitAuto (d, Vdom.Keyed c1, c2)

    /// <summary>Creates a split panel where components share space based on their content preferences.</summary>
    /// <remarks>
    /// Space is divided proportionally to each component's preferred width (for vertical splits) or height (for horizontal splits).
    /// Both excess space and insufficient space are distributed proportionally to content preferences.
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component.</param>
    static member panelSplitAuto (d, c1 : Vdom<DesiredBounds>, c2 : KeyedVdom<DesiredBounds>) : Vdom<DesiredBounds> =
        Vdom.panelSplitAuto (d, c1, Vdom.Keyed c2)

    /// <summary>Creates a split panel where components share space based on their content preferences.</summary>
    /// <remarks>
    /// Space is divided proportionally to each component's preferred width (for vertical splits) or height (for horizontal splits).
    /// Both excess space and insufficient space are distributed proportionally to content preferences.
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component.</param>
    static member panelSplitAuto
        (d, c1 : KeyedVdom<DesiredBounds>, c2 : KeyedVdom<DesiredBounds>)
        : Vdom<DesiredBounds>
        =
        Vdom.panelSplitAuto (d, Vdom.Keyed c1, Vdom.Keyed c2)

    /// <summary>Creates a split panel where the first component expands to fill excess space.</summary>
    /// <remarks>
    /// Both components receive their preferred size. Any excess space goes entirely to the first component.
    /// The second component stays at its content size.
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component. This component expands to fill excess space.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component. This component stays at its content size.</param>
    static member panelSplitAutoExpand (d, c1 : Vdom<DesiredBounds>, c2 : Vdom<DesiredBounds>) : Vdom<DesiredBounds> =
        Vdom.panelSplit (d, SplitBehaviour.AutoWeighted (ExpansionWeight.Fixed 1.0, ExpansionWeight.Fixed 0.0), c1, c2)
        |> Vdom.Unkeyed

    /// <summary>Creates a split panel where the first component expands to fill excess space.</summary>
    /// <remarks>
    /// Both components receive their preferred size. Any excess space goes entirely to the first component.
    /// The second component stays at its content size.
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component. This component expands to fill excess space.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component. This component stays at its content size.</param>
    static member panelSplitAutoExpand
        (d, c1 : KeyedVdom<DesiredBounds>, c2 : Vdom<DesiredBounds>)
        : Vdom<DesiredBounds>
        =
        Vdom.panelSplitAutoExpand (d, Vdom.Keyed c1, c2)

    /// <summary>Creates a split panel where the first component expands to fill excess space.</summary>
    /// <remarks>
    /// Both components receive their preferred size. Any excess space goes entirely to the first component.
    /// The second component stays at its content size.
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component. This component expands to fill excess space.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component. This component stays at its content size.</param>
    static member panelSplitAutoExpand
        (d, c1 : Vdom<DesiredBounds>, c2 : KeyedVdom<DesiredBounds>)
        : Vdom<DesiredBounds>
        =
        Vdom.panelSplitAutoExpand (d, c1, Vdom.Keyed c2)

    /// <summary>Creates a split panel where the first component expands to fill excess space.</summary>
    /// <remarks>
    /// Both components receive their preferred size. Any excess space goes entirely to the first component.
    /// The second component stays at its content size.
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component. This component expands to fill excess space.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component. This component stays at its content size.</param>
    static member panelSplitAutoExpand
        (d, c1 : KeyedVdom<DesiredBounds>, c2 : KeyedVdom<DesiredBounds>)
        : Vdom<DesiredBounds>
        =
        Vdom.panelSplitAutoExpand (d, Vdom.Keyed c1, Vdom.Keyed c2)

    /// Creates a bordered wrapper around a component, drawing a border around its content.
    /// You can generally use `bordered` instead, which gives you a `Vdom` back instead of an `UnkeyedVdom`.
    static member bordered' (inner : Vdom<DesiredBounds>) : UnkeyedVdom<DesiredBounds> = UnkeyedVdom.Bordered inner

    /// Creates a bordered wrapper around a component, drawing a border around its content.
    static member bordered (inner : Vdom<DesiredBounds>) : Vdom<DesiredBounds> = Vdom.bordered' inner |> Vdom.Unkeyed

    /// Creates a bordered wrapper around a component, drawing a border around its content.
    static member bordered (inner : KeyedVdom<DesiredBounds>) : Vdom<DesiredBounds> =
        Vdom.bordered' (Vdom.Keyed inner) |> Vdom.Unkeyed

    /// Attach a key to a VDOM node, effectively giving that node a name.
    /// If the VDOM node is already keyed, this replaces the key.
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
    static member withKey (key : NodeKey) (vdom : Vdom<'bounds>) : KeyedVdom<'bounds> =
        match vdom with
        | Vdom.Keyed (KeyedVdom (_prevKey, vdom)) -> KeyedVdom (key, vdom)
        | Vdom.Unkeyed vdom -> KeyedVdom (key, vdom)

    /// Mark a keyed node as focusable, for the purposes of the automatic focus tracking system.
    ///
    /// When the user hits TAB while automatic focus tracking is enabled, the WoofWare.Zoomies framework will
    /// cycle through tree nodes which are `focusable`.
    ///
    /// If `isFirstToFocus` is true, this node will receive focus first when the system needs to select a
    /// focus target when no focusable node currently has focus (e.g., when TAB is pressed but nothing is focused).
    /// At most one node should have `isFirstToFocus = true` in a given VDOM tree.
    ///
    /// If `isInitiallyFocused` is true, this node will start with focus from the very first render,
    /// rather than starting with no elements focused. At most one node should have `isInitiallyFocused = true`
    /// in a given VDOM tree.
    ///
    /// This annotation does nothing if WoofWare.Zoomies is running with automatic focus tracking turned off.
    static member withFocusTracking
        (vdom : KeyedVdom<'bounds>, ?isFirstToFocus : bool, ?isInitiallyFocused : bool)
        : Vdom<'bounds>
        =
        Vdom.withFocusTracking' (vdom, ?isFirstToFocus = isFirstToFocus, ?isInitiallyFocused = isInitiallyFocused)
        |> Vdom.Unkeyed

    /// `withFocusTracking`, but gives you back an UnkeyedVdom instead of a Vdom.
    static member withFocusTracking'
        (vdom : KeyedVdom<'bounds>, ?isFirstToFocus : bool, ?isInitiallyFocused : bool)
        : UnkeyedVdom<'bounds>
        =
        let isFirstToFocus = defaultArg isFirstToFocus false
        let isInitiallyFocused = defaultArg isInitiallyFocused false

        UnkeyedVdom.Focusable (isFirstToFocus, isInitiallyFocused, vdom)

    /// <summary>Creates a flexible content component that can render different content based on allocated bounds.</summary>
    /// <param name="measure">Function that specifies size requirements given measurement constraints.</param>
    /// <param name="render">Function that produces the actual VDOM content given the allocated bounds.</param>
    /// <remarks>
    /// <para>
    /// This allows components to make region-dependent rendering decisions, such as rendering a progress bar
    /// with different levels of detail depending on available width.
    /// </para>
    ///
    /// <para>
    /// WoofWare.Zoomies displays a VDOM in three phases: a "measurement" phase, an "arrange" phase, and a "render"
    /// phase. During measurement, the various components of the VDOM indicate to WoofWare.Zoomies that they have some
    /// preferences about their size (e.g. perhaps the progress bar needs some minimum size to display a label, but it
    /// can be arbitrarily wide; or perhaps a checkbox really wants to be exactly three cells wide). Once constraints
    /// are gathered, the "arrange" phase assigns a region of the screen to every VDOM node (and the framework
    /// tries to ensure, but does not guarantee, that every component gets what it requested). Finally, the "render"
    /// phase draws each component into its region of the screen.
    /// </para>
    ///
    /// <para>
    /// During the render phase, each component gets told the region of the screen it was granted, and chooses how
    /// it's going to render. Maybe it has to make difficult decisions at this point: a progress bar may have to
    /// somehow choose how to render in a space which is only one cell wide!
    /// </para>
    ///
    /// <para>
    /// <c>Vdom.flexibleContent</c> is unusual in that it causes multiple rounds of the render algorithm.
    /// Indeed, the <c>render</c> stored in a <c>flexibleContent</c> returns a <c>Vdom</c>, which must itself be
    /// measured and arranged.
    /// There is no attempt to flow the constraints from that internal VDOM back up into the parent, though:
    /// once <c>measure</c> has been called and the constraints of the parent solved, the inner VDOM is locked into
    /// a specific rectangle on the screen.
    /// All the inner VDOM measurement/arrangement/rendering takes place entirely within that specific rectangle.
    /// </para>
    ///
    /// <para>
    /// The <c>measure</c> function is called during the measurement phase with constraints from the parent.
    /// The parent decides how much space it wants for layout, and passes that information down to the child through a
    /// <c>MeasureConstraints</c>. Then the child indicates (via the return value of <c>measure</c>) its own size
    /// requirements.
    /// </para>
    ///
    /// <para>
    /// The <c>render</c> function is called during the subsequent render phase with the actual allocated bounds from
    /// the arrange phase.
    /// It produces the final VDOM content that will be rendered into the allocated space.
    /// </para>
    ///
    /// <para>
    /// The render function may return another <c>FlexibleContent</c>, allowing nested flexible rendering.
    /// </para>
    /// </remarks>
    static member flexibleContent
        (measure : MeasureConstraints -> MeasuredSize)
        (render : Rectangle -> Vdom<DesiredBounds>)
        : Vdom<DesiredBounds>
        =
        Vdom.flexibleContent' measure render |> Vdom.Unkeyed

    /// This is `Vdom.flexibleContent`, but you get the `UnkeyedVdom` directly rather than a `Vdom`.
    static member flexibleContent'
        (measure : MeasureConstraints -> MeasuredSize)
        (render : Rectangle -> Vdom<DesiredBounds>)
        : UnkeyedVdom<DesiredBounds>
        =
        let content =
            {
                Measure = measure
                Render = render
            }

        UnkeyedVdom.FlexibleContent content

    /// A consumer almost certainly wants `Vdom.withTag` instead.
    static member internal withTag' (tag : string) (vdom : Vdom<DesiredBounds>) : UnkeyedVdom<DesiredBounds> =
        UnkeyedVdom.Tag (tag, vdom)

    /// Attach a semantic tag to this node. Tags are metadata only and do not
    /// affect rendering or layout.
    ///
    /// If VdomTagging.Enabled is false, this is a no-op returning the input unchanged.
    static member withTag (tag : string) (vdom : Vdom<'bounds>) : Vdom<'bounds> =
        if not VdomTagging.Enabled then
            vdom
        else
            match vdom with
            | Vdom.Unkeyed inner ->
                let wrappedInner = UnkeyedVdom.Tag (tag, Vdom.Unkeyed inner)
                Vdom.Unkeyed wrappedInner
            | Vdom.Keyed inner ->
                // Extract the inner content, wrap it in a Tag, then re-key it
                let (KeyedVdom (key, innerUnkeyed)) = inner
                let wrappedInner = UnkeyedVdom.Tag (tag, Vdom.Unkeyed innerUnkeyed)
                let rekeyedInner = KeyedVdom (key, wrappedInner)
                Vdom.Keyed rekeyedInner

    /// Produce a human-readable tree representation of the VDOM structure,
    /// including tags and keys.
    static member debugDump (vdom : Vdom<'bounds>) : string =
        let sb = System.Text.StringBuilder ()

        let rec dumpVdom (indent : string) (vdom : Vdom<'bounds>) : unit =
            match vdom with
            | Vdom.Keyed keyed -> dumpKeyedVdom indent keyed
            | Vdom.Unkeyed unkeyed -> dumpUnkeyedVdom indent unkeyed

        and dumpKeyedVdom (indent : string) (KeyedVdom (key, inner)) : unit =
            sb.AppendLine (sprintf "%sKey: %s" indent (NodeKey.toHumanReadableString key))
            |> ignore

            dumpUnkeyedVdom (indent + "  ") inner

        and dumpUnkeyedVdom (indent : string) (vdom : UnkeyedVdom<'bounds>) : unit =
            match vdom with
            | UnkeyedVdom.Empty -> sb.AppendLine (sprintf "%sEmpty" indent) |> ignore
            | UnkeyedVdom.TextContent (content, style, alignment, focused) ->
                let truncated =
                    if content.Length > 50 then
                        content.Substring (0, 47) + "..."
                    else
                        content

                let focusedStr = if focused then " (focused)" else ""

                let alignmentStr =
                    match alignment with
                    | ContentAlignment.Centered -> "centered"
                    | ContentAlignment.TopLeft -> "top-left"

                sb.AppendLine (
                    sprintf
                        "%sText: \"%s\" [%s]%s"
                        indent
                        (truncated.Replace("\n", "\\n").Replace ("\r", "\\r"))
                        alignmentStr
                        focusedStr
                )
                |> ignore
            | UnkeyedVdom.Bordered inner ->
                sb.AppendLine (sprintf "%sBordered:" indent) |> ignore
                dumpVdom (indent + "  ") inner
            | UnkeyedVdom.PanelSplit (direction, behaviour, c1, c2) ->
                let dirStr =
                    match direction with
                    | Vertical -> "Vertical"
                    | Horizontal -> "Horizontal"

                let behavStr =
                    match behaviour with
                    | SplitBehaviour.Proportion p -> sprintf "Proportion %.2f" p
                    | SplitBehaviour.Absolute n -> sprintf "Absolute %d" n
                    | SplitBehaviour.AutoWeighted (w1, w2) ->
                        let weightStr w =
                            match w with
                            | ExpansionWeight.FromContent -> "FromContent"
                            | ExpansionWeight.Fixed f -> sprintf "Fixed %.2f" f

                        sprintf "AutoWeighted (%s, %s)" (weightStr w1) (weightStr w2)

                sb.AppendLine (sprintf "%sPanelSplit: %s %s" indent dirStr behavStr) |> ignore
                sb.AppendLine (sprintf "%s  First:" indent) |> ignore
                dumpVdom (indent + "    ") c1
                sb.AppendLine (sprintf "%s  Second:" indent) |> ignore
                dumpVdom (indent + "    ") c2
            | UnkeyedVdom.Focusable (isFirstToFocus, isInitiallyFocused, inner) ->
                let flags = ResizeArray<string> ()

                if isFirstToFocus then
                    flags.Add "firstToFocus"

                if isInitiallyFocused then
                    flags.Add "initiallyFocused"

                let flagsStr =
                    if flags.Count > 0 then
                        sprintf " [%s]" (String.concat ", " flags)
                    else
                        ""

                sb.AppendLine (sprintf "%sFocusable%s:" indent flagsStr) |> ignore
                dumpKeyedVdom (indent + "  ") inner
            | UnkeyedVdom.FlexibleContent _ -> sb.AppendLine (sprintf "%sFlexibleContent <function>" indent) |> ignore
            | UnkeyedVdom.Tag (tag, inner) ->
                sb.AppendLine (sprintf "%sTag: %s" indent tag) |> ignore
                dumpVdom (indent + "  ") inner

        dumpVdom "" vdom
        sb.ToString ()

    /// Produce a human-readable tree representation of the VDOM structure,
    /// including tags and keys.
    static member debugDump (vdom : KeyedVdom<'bounds>) : string = Vdom.debugDump (Vdom.Keyed vdom)

[<RequireQualifiedAccess>]
module KeyedVdom =
    /// Attach a semantic tag to this node. Tags are metadata only and do not
    /// affect rendering or layout.
    ///
    /// If VdomTagging.Enabled is false, this is a no-op returning the input unchanged.
    let withTag (tag : string) (vdom : KeyedVdom<'bounds>) : KeyedVdom<'bounds> =
        if not VdomTagging.Enabled then
            vdom
        else
            let (KeyedVdom (key, innerUnkeyed)) = vdom
            let wrappedInner = UnkeyedVdom.Tag (tag, Vdom.Unkeyed innerUnkeyed)
            KeyedVdom (key, wrappedInner)

    /// Produce a human-readable tree representation of the VDOM structure,
    /// including tags and keys.
    let debugDump (vdom : KeyedVdom<'bounds>) : string = Vdom.debugDump (Vdom.Keyed vdom)
