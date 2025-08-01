namespace WoofWare.Zoomies

type Direction =
    | Vertical
    | Horizontal

type Border = | Yes

type Vdom =
    | Bordered of Vdom
    | PanelSplit of Direction * Choice<float, int> * child1 : Vdom * child2 : Vdom
    | TextContent of string
    | Checkbox of isChecked : bool

module Vdom =

    let textContent s = Vdom.TextContent s

    let panelSplitProportion d p c1 c2 =
        if not (p > 0.0 && p < 1.0) then
            invalidArg "p" "proportion must be between 0 and 1"

        Vdom.PanelSplit (d, Choice1Of2 p, c1, c2)

    let panelSplitAbsolute d p c1 c2 =
        Vdom.PanelSplit (d, Choice2Of2 p, c1, c2)

    let checkbox isChecked = Vdom.Checkbox isChecked

    let bordered inner = Vdom.Bordered inner

    let labelledCheckbox (isChecked : bool) (label : string) : Vdom =
        // TODO: centre this text horizontally so it's next to the checkbox
        textContent label
        |> panelSplitAbsolute Direction.Vertical 3 (checkbox isChecked)
