namespace WoofWare.Zoomies

open System.Collections.Immutable
open TypeEquality

/// Global configuration for the tagging system.
[<RequireQualifiedAccess>]
module VdomTagging =
    /// When false, Vdom.withTag is a no-op and no allocations occur.
    /// Defaults to false. Set to true before constructing VDOMs to enable tagging.
    let mutable Enabled : bool = false

/// Stored in VDOM nodes. When tagging is disabled, this is always Empty.
type VdomTags = private | VdomTags of ImmutableArray<string>

[<RequireQualifiedAccess>]
module VdomTags =
    /// The empty tag collection. This is a singleton; no allocation occurs.
    let empty : VdomTags = VdomTags ImmutableArray<string>.Empty

    /// Add a tag, returning a new collection. If VdomTagging.Enabled is false,
    /// returns the input unchanged (no allocation).
    let add (tag : string) (tags : VdomTags) : VdomTags =
        if not VdomTagging.Enabled then
            tags
        else
            let (VdomTags arr) = tags
            VdomTags (arr.Add tag)

    /// Get all tags as a sequence.
    let toSeq (VdomTags arr : VdomTags) : string seq = arr :> string seq

    /// Check if a specific tag is present.
    let contains (tag : string) (VdomTags arr : VdomTags) : bool = arr.Contains tag

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

/// Specifies how content should be aligned within its bounds.
type ContentAlignment =
    /// Content is centered both horizontally and vertically.
    | Centered
    /// Content starts at the top-left corner and wraps.
    | TopLeft

type internal UnkeyedVdom<'bounds> =
    | Bordered of KeylessVdom<'bounds>
    | PanelSplit of SplitDirection * SplitBehaviour * child1 : KeylessVdom<'bounds> * child2 : KeylessVdom<'bounds>
    | TextContent of content : string * style : CellStyle * alignment : ContentAlignment * focused : bool
    | Focusable of isFirstToFocus : bool * isInitiallyFocused : bool * KeyedVdom<'bounds>
    | Empty
    | FlexibleContent of
        measure : (MeasureConstraints -> MeasuredSize) *
        render : (Rectangle -> KeylessVdom<DesiredBounds>)
    | Tag of tag : string * inner : KeylessVdom<'bounds>

and internal KeyedVdom<'bounds> = | WithKey of NodeKey * UnkeyedVdom<'bounds>

and internal KeylessVdom<'bounds> =
    private
    | Keyed of KeyedVdom<'bounds>
    | Unkeyed of UnkeyedVdom<'bounds>

[<NoEquality ; NoComparison>]
type Vdom<'bounds, 'keyed> =
    private
    | Keyed of KeyedVdom<'bounds> * Teq<'keyed, Keyed> * VdomTags
    | Unkeyed of UnkeyedVdom<'bounds> * Teq<'keyed, Unkeyed> * VdomTags

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
        Vdom.Unkeyed (
            UnkeyedVdom.TextContent (s, CellStyle.none, ContentAlignment.TopLeft, isFocused),
            Teq.refl,
            VdomTags.empty
        )

    /// <summary>Creates a text content component with explicit styling.</summary>
    /// <param name="content">The text to display.</param>
    /// <param name="style">The cell styling to apply to the text.</param>
    /// <param name="alignment">Where within the panel to place the text.</param>
    static member styledText
        (content : string, style : CellStyle, ?alignment : ContentAlignment)
        : Vdom<DesiredBounds, Unkeyed>
        =
        Vdom.Unkeyed (
            UnkeyedVdom.TextContent (content, style, defaultArg alignment ContentAlignment.TopLeft, false),
            Teq.refl,
            VdomTags.empty
        )

    /// <summary>Creates an empty zero-sized element.</summary>
    /// <remarks>
    /// This element takes up no space and is useful for positioning content within split panels.
    /// For example, use <c>Vdom.panelSplitAbsolute</c> with a negative width and <c>Vdom.empty</c> to push content
    /// to the right edge of a panel.
    /// </remarks>
    static member empty : Vdom<DesiredBounds, Unkeyed> =
        Vdom.Unkeyed (UnkeyedVdom.Empty, Teq.refl, VdomTags.empty)

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
        | Unkeyed (_, teq, _) -> VdomUtils.teqUnreachable teq
        | Keyed (c1, _, _) ->

        match c2 with
        | Unkeyed (_, teq, _) -> VdomUtils.teqUnreachable teq
        | Keyed (c2, _, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Proportion p, KeylessVdom.Keyed c1, KeylessVdom.Keyed c2),
            Teq.refl,
            VdomTags.empty
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
        | Unkeyed (_, teq, _) -> VdomUtils.teqUnreachable teq
        | Keyed (c1, _, _) ->

        match c2 with
        | Keyed (_, teq, _) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c2, _, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Proportion p, KeylessVdom.Keyed c1, KeylessVdom.Unkeyed c2),
            Teq.refl,
            VdomTags.empty
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
        | Keyed (_, teq, _) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c1, _, _) ->

        match c2 with
        | Unkeyed (_, teq, _) -> VdomUtils.teqUnreachable teq
        | Keyed (c2, _, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Proportion p, KeylessVdom.Unkeyed c1, KeylessVdom.Keyed c2),
            Teq.refl,
            VdomTags.empty
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
        | Keyed (_, teq, _) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c1, _, _) ->

        match c2 with
        | Keyed (_, teq, _) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c2, _, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Proportion p, KeylessVdom.Unkeyed c1, KeylessVdom.Unkeyed c2),
            Teq.refl,
            VdomTags.empty
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
        | Unkeyed (_, teq, _) -> VdomUtils.teqUnreachable teq
        | Keyed (c1, _, _) ->

        match c2 with
        | Unkeyed (_, teq, _) -> VdomUtils.teqUnreachable teq
        | Keyed (c2, _, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Absolute p, KeylessVdom.Keyed c1, KeylessVdom.Keyed c2),
            Teq.refl,
            VdomTags.empty
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
        | Keyed (_, teq, _) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c1, _, _) ->

        match c2 with
        | Unkeyed (_, teq, _) -> VdomUtils.teqUnreachable teq
        | Keyed (c2, _, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Absolute p, KeylessVdom.Unkeyed c1, KeylessVdom.Keyed c2),
            Teq.refl,
            VdomTags.empty
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
        | Unkeyed (_, teq, _) -> VdomUtils.teqUnreachable teq
        | Keyed (c1, _, _) ->

        match c2 with
        | Keyed (_, teq, _) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c2, _, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Absolute p, KeylessVdom.Keyed c1, KeylessVdom.Unkeyed c2),
            Teq.refl,
            VdomTags.empty
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
        | Keyed (_, teq, _) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c1, _, _) ->

        match c2 with
        | Keyed (_, teq, _) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c2, _, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Absolute p, KeylessVdom.Unkeyed c1, KeylessVdom.Unkeyed c2),
            Teq.refl,
            VdomTags.empty
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
        | Unkeyed (_, teq, _) -> VdomUtils.teqUnreachable teq
        | Keyed (c1, _, _) ->

        match c2 with
        | Unkeyed (_, teq, _) -> VdomUtils.teqUnreachable teq
        | Keyed (c2, _, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Auto, KeylessVdom.Keyed c1, KeylessVdom.Keyed c2),
            Teq.refl,
            VdomTags.empty
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
        | Unkeyed (_, teq, _) -> VdomUtils.teqUnreachable teq
        | Keyed (c1, _, _) ->

        match c2 with
        | Keyed (_, teq, _) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c2, _, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Auto, KeylessVdom.Keyed c1, KeylessVdom.Unkeyed c2),
            Teq.refl,
            VdomTags.empty
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
        | Keyed (_, teq, _) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c1, _, _) ->

        match c2 with
        | Unkeyed (_, teq, _) -> VdomUtils.teqUnreachable teq
        | Keyed (c2, _, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Auto, KeylessVdom.Unkeyed c1, KeylessVdom.Keyed c2),
            Teq.refl,
            VdomTags.empty
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
        | Keyed (_, teq, _) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c1, _, _) ->

        match c2 with
        | Keyed (_, teq, _) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (c2, _, _) ->

        Vdom.Unkeyed (
            UnkeyedVdom.PanelSplit (d, SplitBehaviour.Auto, KeylessVdom.Unkeyed c1, KeylessVdom.Unkeyed c2),
            Teq.refl,
            VdomTags.empty
        )

    /// Creates a bordered wrapper around a component, drawing a border around its content.
    static member bordered (inner : Vdom<_, Keyed>) : Vdom<DesiredBounds, Unkeyed> =
        match inner with
        | Unkeyed (_, teq, _) -> VdomUtils.teqUnreachable teq
        | Keyed (inner, _, _) -> Vdom.Unkeyed (UnkeyedVdom.Bordered (KeylessVdom.Keyed inner), Teq.refl, VdomTags.empty)

    /// Creates a bordered wrapper around a component, drawing a border around its content.
    static member bordered (inner : Vdom<_, Unkeyed>) : Vdom<DesiredBounds, Unkeyed> =
        match inner with
        | Keyed (_, teq, _) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (inner, _, _) ->
            Vdom.Unkeyed (UnkeyedVdom.Bordered (KeylessVdom.Unkeyed inner), Teq.refl, VdomTags.empty)

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
        | Keyed (_, teq, _) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (vdom, _, tags) -> Vdom.Keyed (KeyedVdom.WithKey (key, vdom), Teq.refl, tags)

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
        (vdom : Vdom<'bounds, Keyed>, ?isFirstToFocus : bool, ?isInitiallyFocused : bool)
        : Vdom<'bounds, Unkeyed>
        =
        let isFirstToFocus = defaultArg isFirstToFocus false
        let isInitiallyFocused = defaultArg isInitiallyFocused false

        match vdom with
        | Unkeyed (_, teq, _) -> VdomUtils.teqUnreachable teq
        | Keyed (vdom, _, tags) ->
            Vdom.Unkeyed (UnkeyedVdom.Focusable (isFirstToFocus, isInitiallyFocused, vdom), Teq.refl, tags)

    /// <summary>Creates a flexible content component that can render different content based on allocated bounds.</summary>
    /// <param name="measure">Function that specifies size requirements given measurement constraints.</param>
    /// <param name="render">Function that produces the actual VDOM content given the allocated bounds.</param>
    /// <remarks>
    /// This allows components to make region-dependent rendering decisions, such as rendering a progress bar
    /// with different levels of detail depending on available width.
    ///
    /// The measure function is called during the measurement phase with constraints from the parent.
    /// It should return accurate size requirements.
    ///
    /// The render function is called during the arrange phase with the actual allocated bounds.
    /// It produces the final VDOM content for those bounds.
    ///
    /// Note: The render function may return another FlexibleContent, allowing nested flexible rendering.
    /// </remarks>
    static member flexibleContent
        (measure : MeasureConstraints -> MeasuredSize)
        (render : Rectangle -> Vdom<DesiredBounds, Unkeyed>)
        : Vdom<DesiredBounds, Unkeyed>
        =
        let renderInternal (bounds : Rectangle) : KeylessVdom<DesiredBounds> =
            match render bounds with
            | Vdom.Keyed (_, teq, _) -> VdomUtils.teqUnreachable' teq
            | Vdom.Unkeyed (vdom, _, _) -> KeylessVdom.Unkeyed vdom

        Vdom.Unkeyed (UnkeyedVdom.FlexibleContent (measure, renderInternal), Teq.refl, VdomTags.empty)

    /// Attach a semantic tag to this node. Tags are metadata only and do not
    /// affect rendering or layout.
    ///
    /// If VdomTagging.Enabled is false, this is a no-op returning the input unchanged.
    static member withTag (tag : string) (vdom : Vdom<'bounds, 'keyed>) : Vdom<'bounds, 'keyed> =
        if not VdomTagging.Enabled then
            vdom
        else
            match vdom with
            | Vdom.Unkeyed (inner, teq, tags) ->
                let wrappedInner = UnkeyedVdom.Tag (tag, KeylessVdom.Unkeyed inner)
                Vdom.Unkeyed (wrappedInner, teq, tags)
            | Vdom.Keyed (inner, teq, tags) ->
                // Extract the inner content, wrap it in a Tag, then re-key it
                let (KeyedVdom.WithKey (key, innerUnkeyed)) = inner
                let wrappedInner = UnkeyedVdom.Tag (tag, KeylessVdom.Unkeyed innerUnkeyed)
                let rekeyedInner = KeyedVdom.WithKey (key, wrappedInner)
                Vdom.Keyed (rekeyedInner, teq, tags)

    /// Read the tags attached to this node.
    static member tags (vdom : Vdom<'bounds, 'keyed>) : VdomTags =
        match vdom with
        | Vdom.Keyed (_, _, tags) -> tags
        | Vdom.Unkeyed (_, _, tags) -> tags

    /// Produce a human-readable tree representation of the VDOM structure,
    /// including tags and keys.
    static member debugDump (vdom : Vdom<'bounds, 'keyed>) : string =
        let sb = System.Text.StringBuilder ()

        let formatTags (tags : VdomTags) : string =
            let tagList = tags |> VdomTags.toSeq |> Seq.toList

            if List.isEmpty tagList then
                ""
            else
                sprintf "[%s] " (String.concat ", " tagList)

        let rec dumpKeylessVdom (indent : string) (isLast : bool) (vdom : KeylessVdom<'bounds>) : unit =
            let prefix =
                if isLast then
                    "\u2514\u2500\u2500 "
                else
                    "\u251c\u2500\u2500 "

            let childIndent = indent + (if isLast then "    " else "\u2502   ")

            match vdom with
            | KeylessVdom.Keyed keyed -> dumpKeyedVdom indent isLast keyed
            | KeylessVdom.Unkeyed unkeyed -> dumpUnkeyedVdom indent isLast unkeyed

        and dumpKeyedVdom (indent : string) (isLast : bool) (KeyedVdom.WithKey (key, inner)) : unit =
            let prefix =
                if isLast then
                    "\u2514\u2500\u2500 "
                else
                    "\u251c\u2500\u2500 "

            let childIndent = indent + (if isLast then "    " else "\u2502   ")
            sb.Append indent |> ignore
            sb.Append prefix |> ignore
            sb.AppendFormat ("Keyed \"{0}\": ", NodeKey.toString key) |> ignore
            dumpUnkeyedVdomInline inner
            sb.AppendLine () |> ignore

        and dumpUnkeyedVdom (indent : string) (isLast : bool) (vdom : UnkeyedVdom<'bounds>) : unit =
            let prefix =
                if isLast then
                    "\u2514\u2500\u2500 "
                else
                    "\u251c\u2500\u2500 "

            let childIndent = indent + (if isLast then "    " else "\u2502   ")
            sb.Append indent |> ignore
            sb.Append prefix |> ignore
            dumpUnkeyedVdomInline vdom
            sb.AppendLine () |> ignore

        and dumpUnkeyedVdomInline (vdom : UnkeyedVdom<'bounds>) : unit =
            match vdom with
            | UnkeyedVdom.Empty -> sb.Append "Empty" |> ignore
            | UnkeyedVdom.TextContent (content, style, alignment, focused) ->
                let truncated =
                    if content.Length > 30 then
                        content.Substring (0, 27) + "..."
                    else
                        content

                let focusedStr = if focused then " (focused)" else ""

                sb.AppendFormat ("Text \"{0}\"{1}", truncated.Replace("\n", "\\n").Replace ("\r", "\\r"), focusedStr)
                |> ignore
            | UnkeyedVdom.Bordered inner ->
                sb.Append "Bordered: " |> ignore
                dumpKeylessVdomInline inner
            | UnkeyedVdom.PanelSplit (direction, behaviour, c1, c2) ->
                let dirStr =
                    match direction with
                    | Vertical -> "Vertical"
                    | Horizontal -> "Horizontal"

                let behavStr =
                    match behaviour with
                    | SplitBehaviour.Proportion p -> sprintf "Proportion %.2f" p
                    | SplitBehaviour.Absolute n -> sprintf "Absolute %d" n
                    | SplitBehaviour.Auto -> "Auto"

                sb.AppendFormat ("PanelSplit {0} {1}", dirStr, behavStr) |> ignore
            | UnkeyedVdom.Focusable (isFirstToFocus, isInitiallyFocused, inner) ->
                let flags = ResizeArray<string> ()

                if isFirstToFocus then
                    flags.Add "firstToFocus"

                if isInitiallyFocused then
                    flags.Add "initiallyFocused"

                let flagsStr =
                    if flags.Count > 0 then
                        sprintf " (%s)" (String.concat ", " flags)
                    else
                        ""

                sb.AppendFormat ("Focusable{0}: ", flagsStr) |> ignore
                dumpKeyedVdomInline inner
            | UnkeyedVdom.FlexibleContent _ -> sb.Append "FlexibleContent <...>" |> ignore
            | UnkeyedVdom.Tag (tag, inner) ->
                sb.AppendFormat ("[{0}] ", tag) |> ignore
                dumpKeylessVdomInline inner

        and dumpKeylessVdomInline (vdom : KeylessVdom<'bounds>) : unit =
            match vdom with
            | KeylessVdom.Keyed keyed -> dumpKeyedVdomInline keyed
            | KeylessVdom.Unkeyed unkeyed -> dumpUnkeyedVdomInline unkeyed

        and dumpKeyedVdomInline (KeyedVdom.WithKey (key, inner)) : unit =
            sb.AppendFormat ("Keyed \"{0}\": ", NodeKey.toString key) |> ignore
            dumpUnkeyedVdomInline inner

        match vdom with
        | Vdom.Keyed (keyed, _, tags) ->
            sb.Append (formatTags tags) |> ignore
            dumpKeyedVdomInline keyed
            sb.ToString ()
        | Vdom.Unkeyed (unkeyed, _, tags) ->
            sb.Append (formatTags tags) |> ignore
            dumpUnkeyedVdomInline unkeyed
            sb.ToString ()

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
