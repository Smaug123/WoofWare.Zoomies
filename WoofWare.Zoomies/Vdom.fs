namespace WoofWare.Zoomies

type Direction =
    | Vertical
    | Horizontal

type Border = | Yes

type DesiredBounds = unit

type Vdom<'bounds> =
    | Bordered of Vdom<'bounds>
    | PanelSplit of Direction * Choice<float, int> * child1 : Vdom<'bounds> * child2 : Vdom<'bounds>
    | TextContent of string * focused : bool * onReceiveFocus : (unit -> unit) option
    | Checkbox of isChecked : bool * isFocused : bool * onReceiveFocus : (unit -> unit)

type VdomCata<'ret> =
    abstract AtBordered : 'ret -> 'ret
    abstract AtPanelSplit : Direction -> Choice<float, int> -> child1 : 'ret -> child2 : 'ret -> 'ret
    abstract AtTextContent : string -> focused : bool -> onReceiveFocus : (unit -> unit) option -> 'ret
    abstract AtCheckbox : isChecked : bool -> isFocused : bool -> onReceiveFocus : (unit -> unit) -> 'ret

[<RequireQualifiedAccess>]
module Vdom =

    let textContent (onReceiveFocus : (unit -> unit) option) s =
        Vdom.TextContent (s, false, onReceiveFocus)

    let panelSplitProportion d p c1 c2 =
        if not (p > 0.0 && p < 1.0) then
            invalidArg "p" "proportion must be between 0 and 1"

        Vdom.PanelSplit (d, Choice1Of2 p, c1, c2)

    let panelSplitAbsolute d p c1 c2 =
        Vdom.PanelSplit (d, Choice2Of2 p, c1, c2)

    let checkbox (onReceiveFocus : unit -> unit) (isFocused : bool) isChecked =
        Vdom.Checkbox (isChecked, isFocused, onReceiveFocus)

    let bordered inner = Vdom.Bordered inner

    let labelledCheckbox
        (onReceiveFocus : unit -> unit)
        (isFocused : bool)
        (isChecked : bool)
        (label : string)
        : Vdom<DesiredBounds>
        =
        // TODO: centre this text horizontally so it's next to the checkbox
        textContent None label
        |> panelSplitAbsolute Direction.Vertical 3 (checkbox onReceiveFocus isFocused isChecked)

    let rec cata<'bounds, 'ret> (c : VdomCata<'ret>) (vdom : Vdom<'bounds>) : 'ret =
        match vdom with
        | Vdom.Bordered vdom -> c.AtBordered (cata c vdom)
        | Vdom.PanelSplit (direction, prop, child1, child2) ->
            c.AtPanelSplit direction prop (cata c child1) (cata c child2)
        | Vdom.TextContent (s, focused, onReceiveFocus) -> c.AtTextContent s focused onReceiveFocus
        | Vdom.Checkbox (isChecked, isFocused, onReceiveFocus) -> c.AtCheckbox isChecked isFocused onReceiveFocus

    let idCata : VdomCata<Vdom<'bounds>> =
        { new VdomCata<_> with
            member _.AtBordered v = Vdom.Bordered v

            member _.AtCheckbox isChecked isFocused onReceiveFocus =
                Vdom.Checkbox (isChecked, isFocused, onReceiveFocus)

            member _.AtPanelSplit dir prop child1 child2 =
                Vdom.PanelSplit (dir, prop, child1, child2)

            member _.AtTextContent contents focused onReceiveFocus =
                Vdom.TextContent (contents, focused, onReceiveFocus)
        }

    type FocusState =
        {
            FirstUnfocusedAbsolute : (unit -> unit) option
            FirstUnfocusedAfter : (unit -> unit) option
            FocusFound : bool
        }

    let advanceFocusCata : VdomCata<FocusState> =
        // We assume that at most one element has focus.
        { new VdomCata<_> with
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
        }
