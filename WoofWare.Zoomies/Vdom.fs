namespace WoofWare.Zoomies

open TypeEquality

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

    /// Attach a stable key to a VDOM node
    static member withKey (key : NodeKey) (vdom : Vdom<'bounds, Unkeyed>) : Vdom<'bounds, Keyed> =
        match vdom with
        | Keyed (_, teq) -> VdomUtils.teqUnreachable' teq
        | Unkeyed (vdom, _) -> Vdom.Keyed (KeyedVdom.WithKey (key, vdom), Teq.refl)

    /// Mark a keyed node as focusable
    /// The Focusable constructor itself has keyedness 'keyed, so it can be used polymorphically
    static member focusable (vdom : Vdom<'bounds, Keyed>) : Vdom<'bounds, Unkeyed> =
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
