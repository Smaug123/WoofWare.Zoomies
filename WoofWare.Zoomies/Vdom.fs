namespace WoofWare.Zoomies

type Direction =
    | Vertical
    | Horizontal

type Border = | Yes

type Vdom =
    | Bordered of Vdom
    | PanelSplit of Direction * proportion : float * child1 : Vdom * child2 : Vdom
    | TextContent of string
    | Checkbox of isChecked : bool

module Vdom =

    let textContent s = Vdom.TextContent s

    let panelSplit d p c1 c2 = Vdom.PanelSplit (d, p, c1, c2)

    let checkbox isChecked = Vdom.Checkbox isChecked

    let bordered inner = Vdom.Bordered inner

    let labelledCheckbox (isChecked : bool) (label : string) : Vdom =
        textContent label
        |> panelSplit Direction.Horizontal 0.2 (checkbox isChecked |> bordered)
