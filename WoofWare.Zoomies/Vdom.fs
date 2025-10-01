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
type UnkeyedVdom<'bounds> =
    private
    | Bordered of Vdom<'bounds>
    | PanelSplit of Direction * Choice<float, int> * child1 : Vdom<'bounds> * child2 : Vdom<'bounds>
    | TextContent of string * focused : bool * onReceiveFocus : (unit -> unit) option
    | Checkbox of isChecked : bool * isFocused : bool * onReceiveFocus : (unit -> unit)
    | Focusable of KeyedVdom<'bounds>

and [<NoComparison>] KeyedVdom<'bounds> =
    private
        {
            Key : NodeKey
            Inner : Vdom<'bounds>
        }

and [<NoComparison>] Vdom<'bounds> =
    private
    | Keyed of KeyedVdom<'bounds>
    | Unkeyed of UnkeyedVdom<'bounds>

type VdomCata<'retUnkeyed, 'retKeyed, 'ret> =
    abstract AtBordered : 'ret -> 'retUnkeyed
    abstract AtPanelSplit : Direction -> Choice<float, int> -> child1 : 'ret -> child2 : 'ret -> 'retUnkeyed
    abstract AtTextContent : string -> focused : bool -> onReceiveFocus : (unit -> unit) option -> 'retUnkeyed
    abstract AtCheckbox : isChecked : bool -> isFocused : bool -> onReceiveFocus : (unit -> unit) -> 'retUnkeyed
    abstract AtWithKey : NodeKey -> 'ret -> 'retKeyed
    abstract AtFocusable : 'retKeyed -> 'retUnkeyed
    abstract OfUnkeyed : 'retUnkeyed -> 'ret
    abstract OfKeyed : 'retKeyed -> 'ret

[<RequireQualifiedAccess>]
module Vdom =

    let textContent (onReceiveFocus : (unit -> unit) option) s : UnkeyedVdom<DesiredBounds> =
        UnkeyedVdom.TextContent (s, false, onReceiveFocus)

    let panelSplitProportion d p c1 c2 : UnkeyedVdom<DesiredBounds> =
        if not (p > 0.0 && p < 1.0) then
            invalidArg "p" "proportion must be between 0 and 1"

        UnkeyedVdom.PanelSplit (d, Choice1Of2 p, c1, c2)

    let panelSplitAbsolute d p c1 c2 : UnkeyedVdom<DesiredBounds> =
        UnkeyedVdom.PanelSplit (d, Choice2Of2 p, c1, c2)

    let checkbox (onReceiveFocus : unit -> unit) (isFocused : bool) isChecked : UnkeyedVdom<DesiredBounds> =
        UnkeyedVdom.Checkbox (isChecked, isFocused, onReceiveFocus)

    let bordered inner : UnkeyedVdom<DesiredBounds> = UnkeyedVdom.Bordered inner

    let labelledCheckbox
        (onReceiveFocus : unit -> unit)
        (isFocused : bool)
        (isChecked : bool)
        (label : string)
        : UnkeyedVdom<DesiredBounds>
        =
        // TODO: centre this text horizontally so it's next to the checkbox
        textContent None label
        |> Vdom.Unkeyed
        |> panelSplitAbsolute Direction.Vertical 3 (checkbox onReceiveFocus isFocused isChecked |> Vdom.Unkeyed)

    /// Attach a stable key to a VDOM node
    let withKey (key : NodeKey) (vdom : Vdom<'bounds>) : KeyedVdom<'bounds> =
        {
            Key = key
            Inner = vdom
        }

    /// Mark a node as focusable (requires a keyed node)
    let focusable (vdom : KeyedVdom<'bounds>) : UnkeyedVdom<'bounds> =
        UnkeyedVdom.Focusable vdom

    let rec cata<'bounds, 'ret, 'retUnkeyed, 'retKeyed> (c : VdomCata<'retUnkeyed, 'retKeyed, 'ret>) (vdom : Vdom<'bounds>) : 'ret =
        match vdom with
        | Keyed vdom -> cataKeyed c vdom |> c.OfKeyed
        | Unkeyed vdom -> cataUnkeyed c vdom |> c.OfUnkeyed

    and cataUnkeyed<'bounds, 'ret, 'retUnkeyed, 'retKeyed> (c : VdomCata<'retUnkeyed, 'retKeyed, 'ret>) (vdom : UnkeyedVdom<'bounds>) : 'retUnkeyed =
        match vdom with
        | UnkeyedVdom.Bordered vdom -> c.AtBordered (cata c vdom)
        | UnkeyedVdom.PanelSplit (direction, prop, child1, child2) ->
            c.AtPanelSplit direction prop (cata c child1) (cata c child2)
        | UnkeyedVdom.TextContent (s, focused, onReceiveFocus) -> c.AtTextContent s focused onReceiveFocus
        | UnkeyedVdom.Checkbox (isChecked, isFocused, onReceiveFocus) -> c.AtCheckbox isChecked isFocused onReceiveFocus
        | UnkeyedVdom.Focusable inner -> c.AtFocusable (cataKeyed c inner)

    and cataKeyed<'bounds, 'ret, 'retUnkeyed, 'retKeyed> (c : VdomCata<'retUnkeyed, 'retKeyed, 'ret>) (vdom : KeyedVdom<'bounds>) : 'retKeyed =
        c.AtWithKey vdom.Key (cata c vdom.Inner)

    let idCata<'bounds, 'keyed> : VdomCata<UnkeyedVdom<'bounds>, KeyedVdom<'bounds>, Vdom<'bounds>> =
        { new VdomCata<_, _, _> with
            member _.AtBordered v = UnkeyedVdom.Bordered v

            member _.AtCheckbox isChecked isFocused onReceiveFocus =
                UnkeyedVdom.Checkbox (isChecked, isFocused, onReceiveFocus)

            member _.AtPanelSplit dir prop child1 child2 =
                UnkeyedVdom.PanelSplit (dir, prop, child1, child2)

            member _.AtTextContent contents focused onReceiveFocus =
                UnkeyedVdom.TextContent (contents, focused, onReceiveFocus)

            member _.AtWithKey key inner= { Key = key ; Inner = inner }

            member _.AtFocusable inner =
                UnkeyedVdom.Focusable inner

            member _.OfKeyed k = Vdom.Keyed k
            member _.OfUnkeyed u = Vdom.Unkeyed u
        }

    type FocusState =
        {
            FirstUnfocusedAbsolute : (unit -> unit) option
            FirstUnfocusedAfter : (unit -> unit) option
            FocusFound : bool
        }

    let advanceFocusCata : VdomCata<FocusState, FocusState, FocusState> =
        // We assume that at most one element has focus.
        { new VdomCata<_, _, _> with
            member _.AtBordered v = v

            member _.AtCheckbox isChecked isFocused onReceiveFocus =
                if isFocused then
                    {
                        FirstUnfocusedAfter = None
                        FirstUnfocusedAbsolute = None
                        FocusFound = true
                    }
                else
                    {
                        FirstUnfocusedAfter = None
                        FirstUnfocusedAbsolute = Some onReceiveFocus
                        FocusFound = false
                    }

            member _.AtTextContent contents isFocused onReceiveFocus =
                if isFocused then
                    {
                        FirstUnfocusedAbsolute = None
                        FirstUnfocusedAfter = None
                        FocusFound = true
                    }
                else
                    {
                        FirstUnfocusedAbsolute = onReceiveFocus
                        FirstUnfocusedAfter = onReceiveFocus
                        FocusFound = false
                    }

            member _.AtPanelSplit dir prop child1 child2 =
                if child1.FocusFound then
                    {
                        FocusFound = true
                        FirstUnfocusedAfter = child1.FirstUnfocusedAfter |> Option.orElse child2.FirstUnfocusedAbsolute
                        FirstUnfocusedAbsolute =
                            child1.FirstUnfocusedAbsolute |> Option.orElse child2.FirstUnfocusedAbsolute
                    }
                elif child2.FocusFound then
                    {
                        FocusFound = true
                        FirstUnfocusedAfter = child2.FirstUnfocusedAfter
                        FirstUnfocusedAbsolute =
                            child1.FirstUnfocusedAbsolute |> Option.orElse child2.FirstUnfocusedAbsolute
                    }
                else
                    {
                        FocusFound = false
                        FirstUnfocusedAfter = None
                        FirstUnfocusedAbsolute =
                            child1.FirstUnfocusedAbsolute |> Option.orElse child2.FirstUnfocusedAbsolute
                    }

            member _.AtWithKey _key inner = inner

            member _.AtFocusable inner = inner
            member _.OfKeyed i = i
            member _.OfUnkeyed i = i
        }
