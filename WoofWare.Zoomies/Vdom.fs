namespace WoofWare.Zoomies

open System

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
    | Bordered of VdomKeyCrate<'bounds>
    | PanelSplit of Direction * Choice<float, int> * child1 : VdomKeyCrate<'bounds> * child2 : VdomKeyCrate<'bounds>
    | TextContent of string * focused : bool
    | Checkbox of isChecked : bool * isFocused : bool
    | WithKey of NodeKey * Vdom<'bounds, Unkeyed>
    | Focusable of Vdom<'bounds, Keyed>

and private VdomKeyCrate<'bounds> =
    abstract Apply<'ret> : VdomKeyEval<'bounds, 'ret> -> 'ret
    abstract ReferenceEquals<'bounds2> : VdomKeyCrate<'bounds2> -> bool
    abstract ReferenceEquals<'bounds2, 'keyed> : Vdom<'bounds2, 'keyed> -> bool

and private VdomKeyEval<'bounds, 'ret> =
    abstract Eval<'key> : Vdom<'bounds, 'key> -> 'ret

[<RequireQualifiedAccess>]
module private VdomKeyCrate =
    let make v =
        { new VdomKeyCrate<_> with
            member _.Apply e = e.Eval v
            member _.ReferenceEquals<'bounds2> (other : VdomKeyCrate<'bounds2>) =
                { new VdomKeyEval<_, _> with
                    member _.Eval other =
                        Object.ReferenceEquals (v, other)
                }
                |> other.Apply
            member _.ReferenceEquals<'bounds2, 'keyed> (other : Vdom<'bounds2, 'keyed>) =
                Object.ReferenceEquals (v, other)
        }

[<RequireQualifiedAccess>]
module Vdom =

    let textContent (isFocused : bool) (s : string) : Vdom<DesiredBounds, Unkeyed> =
        Vdom.TextContent (s, isFocused)

    let panelSplitProportion d p c1 c2 : Vdom<DesiredBounds, Unkeyed> =
        if not (p > 0.0 && p < 1.0) then
            invalidArg "p" "proportion must be between 0 and 1"

        Vdom.PanelSplit (d, Choice1Of2 p, VdomKeyCrate.make c1, VdomKeyCrate.make c2)

    let panelSplitAbsolute d p c1 c2 : Vdom<DesiredBounds, Unkeyed> =
        Vdom.PanelSplit (d, Choice2Of2 p, VdomKeyCrate.make c1, VdomKeyCrate.make c2)

    let checkbox (isFocused : bool) (isChecked : bool) : Vdom<DesiredBounds, Unkeyed> =
        Vdom.Checkbox (isChecked, isFocused)

    let bordered (inner : Vdom<_, 'keyed>) : Vdom<DesiredBounds, Unkeyed> = Vdom.Bordered (VdomKeyCrate.make inner)

    let labelledCheckbox
        (isFocused : bool)
        (isChecked : bool)
        (label : string)
        : Vdom<DesiredBounds, Unkeyed>
        =
        // TODO: centre this text horizontally so it's next to the checkbox
        panelSplitAbsolute Direction.Vertical 3 (checkbox isFocused isChecked) (textContent false label)

    /// Attach a stable key to a VDOM node
    let withKey (key : NodeKey) (vdom : Vdom<'bounds, Unkeyed>) : Vdom<'bounds, Keyed> =
        Vdom.WithKey (key, vdom)

    /// Mark a keyed node as focusable
    /// The Focusable constructor itself has keyedness 'keyed, so it can be used polymorphically
    let focusable (vdom : Vdom<'bounds, Keyed>) : Vdom<'bounds, Unkeyed> =
        Vdom.Focusable vdom

