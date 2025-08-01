namespace WoofWare.Zoomies

type Direction =
    | Vertical
    | Horizontal

type Border = | Yes

type Vdom =
    | Bordered of Vdom
    | PanelSplit of Direction * proportion: float * child1: Vdom * child2: Vdom
    | TextContent of string
    | Checkbox

module Vdom =

    let textContent s = Vdom.TextContent s

    let panelSplit d p c1 c2 = Vdom.PanelSplit(d, p, c1, c2)

    let checkbox = Vdom.Checkbox

    let labelledCheckbox (label: string) : Vdom =
        textContent label |> panelSplit Direction.Horizontal 0.2 checkbox
