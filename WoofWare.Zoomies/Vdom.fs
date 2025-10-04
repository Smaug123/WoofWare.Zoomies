namespace WoofWare.Zoomies

open TypeEquality

/// Opaque identifier for stable node identity across frames
type NodeKey = private | NodeKey of string

[<RequireQualifiedAccess>]
module NodeKey =
    let make (s : string) : NodeKey = NodeKey s
    let toString (NodeKey s) = s

/// Phantom type to track whether a node has a key
type Keyed = private | Keyed

/// Phantom type to track whether a node lacks a key
type Unkeyed = private | Unkeyed

type Direction =
    | Vertical
    | Horizontal

type Border = | Yes

type DesiredBounds = unit

type private UnkeyedVdom<'bounds> =
    | Bordered of KeylessVdom<'bounds>
    | PanelSplit of Direction * Choice<float, int> * child1 : KeylessVdom<'bounds> * child2 : KeylessVdom<'bounds>
    | TextContent of string * focused : bool
    | Checkbox of isChecked : bool * isFocused : bool
    | Focusable of KeyedVdom<'bounds>

and private KeyedVdom<'bounds> = | WithKey of NodeKey * UnkeyedVdom<'bounds>

and private KeylessVdom<'bounds> =
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
    let teqUnreachable (t : Teq<Keyed, Unkeyed>) : 'a = failwith "unreachable"
    let teqUnreachable' (t : Teq<Unkeyed, Keyed>) : 'a = failwith "unreachable"

[<Sealed>]
type Vdom =

    static member textContent (isFocused : bool) (s : string) : Vdom<DesiredBounds, Unkeyed> =
        Vdom.Unkeyed (UnkeyedVdom.TextContent (s, isFocused), Teq.refl)

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

        Vdom.Unkeyed (UnkeyedVdom.PanelSplit (d, Choice1Of2 p, KeylessVdom.Keyed c1, KeylessVdom.Keyed c2), Teq.refl)

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

        Vdom.Unkeyed (UnkeyedVdom.PanelSplit (d, Choice1Of2 p, KeylessVdom.Keyed c1, KeylessVdom.Unkeyed c2), Teq.refl)

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

        Vdom.Unkeyed (UnkeyedVdom.PanelSplit (d, Choice1Of2 p, KeylessVdom.Unkeyed c1, KeylessVdom.Keyed c2), Teq.refl)

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
            UnkeyedVdom.PanelSplit (d, Choice1Of2 p, KeylessVdom.Unkeyed c1, KeylessVdom.Unkeyed c2),
            Teq.refl
        )

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

        Vdom.Unkeyed (UnkeyedVdom.PanelSplit (d, Choice2Of2 p, KeylessVdom.Keyed c1, KeylessVdom.Keyed c2), Teq.refl)

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

        Vdom.Unkeyed (UnkeyedVdom.PanelSplit (d, Choice2Of2 p, KeylessVdom.Unkeyed c1, KeylessVdom.Keyed c2), Teq.refl)

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

        Vdom.Unkeyed (UnkeyedVdom.PanelSplit (d, Choice2Of2 p, KeylessVdom.Keyed c1, KeylessVdom.Unkeyed c2), Teq.refl)

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
            UnkeyedVdom.PanelSplit (d, Choice2Of2 p, KeylessVdom.Unkeyed c1, KeylessVdom.Unkeyed c2),
            Teq.refl
        )

    static member checkbox (isFocused : bool) (isChecked : bool) : Vdom<DesiredBounds, Unkeyed> =
        Vdom.Unkeyed (UnkeyedVdom.Checkbox (isChecked, isFocused), Teq.refl)

    static member bordered (inner : Vdom<_, Keyed>) : Vdom<DesiredBounds, Unkeyed> =
        match inner with
        | Unkeyed (_, teq) -> VdomUtils.teqUnreachable teq
        | Keyed (inner, _) -> Vdom.Unkeyed (UnkeyedVdom.Bordered (KeylessVdom.Keyed inner), Teq.refl)

    static member bordered (inner : Vdom<_, Unkeyed>) : Vdom<DesiredBounds, Unkeyed> =
        match inner with
        | Keyed (_, teq) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (inner, _) -> Vdom.Unkeyed (UnkeyedVdom.Bordered (KeylessVdom.Unkeyed inner), Teq.refl)

    static member labelledCheckbox
        (isFocused : bool)
        (isChecked : bool)
        (label : string)
        : Vdom<DesiredBounds, Unkeyed>
        =
        // TODO: centre this text horizontally so it's next to the checkbox
        Vdom.panelSplitAbsolute (Direction.Vertical, 3, Vdom.checkbox isFocused isChecked, Vdom.textContent false label)

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
    /// This annotation does nothing if WoofWare.Zoomies is running with automatic focus tracking turned off.
    static member withFocusTracking (vdom : Vdom<'bounds, Keyed>) : Vdom<'bounds, Unkeyed> =
        match vdom with
        | Unkeyed (_, teq) -> VdomUtils.teqUnreachable teq
        | Keyed (vdom, _) -> Vdom.Unkeyed (UnkeyedVdom.Focusable vdom, Teq.refl)

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
