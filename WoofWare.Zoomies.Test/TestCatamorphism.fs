namespace WoofWare.Zoomies.Test

open NUnit.Framework
open FsUnitTyped
open WoofWare.Zoomies

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCatamorphism =

    let idCata : VdomCata'<Vdom<DesiredBounds>, KeyedVdom<DesiredBounds>, UnkeyedVdom<DesiredBounds>> =
        let vdom =
            { new VdomCataCase<_, _, _> with
                member _.Keyed x = Vdom.Keyed x
                member _.Unkeyed x = Vdom.Unkeyed x
            }

        let keyedVdom =
            { new KeyedVdomCataCase<_, _, _> with
                member _.WithKey key inner = Vdom.withKey key (Vdom.Unkeyed inner)
            }

        let unkeyedVdom =
            { new UnkeyedVdomCataCase<Vdom<DesiredBounds>, KeyedVdom<DesiredBounds>, UnkeyedVdom<DesiredBounds>> with
                member _.Bordered inner = Vdom.bordered' inner

                member _.PanelSplit dir split child1 child2 =
                    Vdom.panelSplit (dir, split, child1, child2)

                member _.TextContent content style alignment focused wrap =
                    Vdom.textContent' (content, focused, style = style, alignment = alignment, wrap = wrap)

                member _.StyledSpans spans alignment focused wrap =
                    Vdom.styledSpans' (spans, isFocused = focused, alignment = alignment, wrap = wrap)

                member _.Focusable isFirstToFocus isInitiallyFocused inner =
                    Vdom.withFocusTracking' (
                        inner,
                        isFirstToFocus = isFirstToFocus,
                        isInitiallyFocused = isInitiallyFocused
                    )

                member _.Empty = Vdom.emptyUnkeyed

                member _.FlexibleContent measure render = Vdom.flexibleContent' measure render

                member _.Tag tag inner = Vdom.withTag' tag inner
            }

        {
            Vdom = vdom
            KeyedVdom = keyedVdom
            UnkeyedVdom = unkeyedVdom
        }

    [<Test>]
    let ``PanelSplit vertical renders identically after identity catamorphism`` () =
        // Create a vertical split with "LEFT" on the left and "RIGHT" on the right
        let left = Vdom.textContent "LEFT"
        let right = Vdom.textContent "RIGHT"
        let vdom = Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, left, right)

        // Run the identity catamorphism
        let result = VdomCata.run idCata vdom

        // Render both VDOMs and compare
        let console1, terminal1 = ConsoleHarness.make ()
        let console2, terminal2 = ConsoleHarness.make ()

        let renderState1 = RenderState.make<unit> console1 MockTime.getStaticUtcNow None
        let renderState2 = RenderState.make<unit> console2 MockTime.getStaticUtcNow None

        Render.oneStep renderState1 () (fun _ -> vdom)
        Render.oneStep renderState2 () (fun _ -> result)

        let output1 = ConsoleHarness.toString terminal1
        let output2 = ConsoleHarness.toString terminal2

        output1 |> shouldEqual output2

    [<Test>]
    let ``PanelSplit horizontal renders identically after identity catamorphism`` () =
        // Create a horizontal split with "TOP" on top and "BOTTOM" on bottom
        let top = Vdom.textContent "TOP"
        let bottom = Vdom.textContent "BOTTOM"
        let vdom = Vdom.panelSplitProportion (SplitDirection.Horizontal, 0.5, top, bottom)

        // Run the identity catamorphism
        let result = VdomCata.run idCata vdom

        // Render both VDOMs and compare
        let console1, terminal1 = ConsoleHarness.make ()
        let console2, terminal2 = ConsoleHarness.make ()

        let renderState1 = RenderState.make<unit> console1 MockTime.getStaticUtcNow None
        let renderState2 = RenderState.make<unit> console2 MockTime.getStaticUtcNow None

        Render.oneStep renderState1 () (fun _ -> vdom)
        Render.oneStep renderState2 () (fun _ -> result)

        let output1 = ConsoleHarness.toString terminal1
        let output2 = ConsoleHarness.toString terminal2

        output1 |> shouldEqual output2

    [<Test>]
    let ``Nested PanelSplit renders identically after identity catamorphism`` () =
        // Create a more complex nested structure:
        //   Horizontal split:
        //     Top: Vertical split: "A" | "B"
        //     Bottom: Vertical split: "C" | "D"
        let topLeft = Vdom.textContent "A"
        let topRight = Vdom.textContent "B"

        let topHalf =
            Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, topLeft, topRight)

        let bottomLeft = Vdom.textContent "C"
        let bottomRight = Vdom.textContent "D"

        let bottomHalf =
            Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, bottomLeft, bottomRight)

        let vdom =
            Vdom.panelSplitProportion (SplitDirection.Horizontal, 0.5, topHalf, bottomHalf)

        // Run the identity catamorphism
        let result = VdomCata.run idCata vdom

        // Render both VDOMs and compare
        let console1, terminal1 = ConsoleHarness.make ()
        let console2, terminal2 = ConsoleHarness.make ()

        let renderState1 = RenderState.make<unit> console1 MockTime.getStaticUtcNow None
        let renderState2 = RenderState.make<unit> console2 MockTime.getStaticUtcNow None

        Render.oneStep renderState1 () (fun _ -> vdom)
        Render.oneStep renderState2 () (fun _ -> result)

        let output1 = ConsoleHarness.toString terminal1
        let output2 = ConsoleHarness.toString terminal2

        output1 |> shouldEqual output2
