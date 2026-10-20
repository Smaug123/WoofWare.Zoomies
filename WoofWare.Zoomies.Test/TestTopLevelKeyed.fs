namespace WoofWare.Zoomies.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTopLevelKeyed =

    let private renderToString (vdom : Vdom<DesiredBounds>) : string =
        let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 5)
        use renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None
        Render.oneStep renderState () (fun () -> vdom)
        ConsoleHarness.toString terminal

    let private examples : (string * Vdom<DesiredBounds>) list =
        [
            "text", Vdom.textContent "hello"
            "bordered", Vdom.textContent "hello" |> Vdom.bordered
            "split",
            Vdom.panelSplitProportion (SplitDirection.Vertical, 0.5, Vdom.textContent "left", Vdom.textContent "right")
        ]

    [<Test>]
    let ``a keyed top-level vdom renders exactly like its unkeyed contents`` () =
        for _name, vdom in examples do
            let keyed = Vdom.withKey (NodeKey.make "root") vdom |> Vdom.Keyed
            renderToString keyed |> shouldEqual (renderToString vdom)

    [<Test>]
    let ``re-rendering a keyed top-level vdom matches a fresh render`` () =
        let key = NodeKey.make "root"

        for _, first in examples do
            for _, second in examples do
                let console, terminal = ConsoleHarness.make' (fun () -> 30) (fun () -> 5)
                use renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None
                Render.oneStep renderState () (fun () -> Vdom.withKey key first |> Vdom.Keyed)
                Render.oneStep renderState () (fun () -> Vdom.withKey key second |> Vdom.Keyed)

                ConsoleHarness.toString terminal |> shouldEqual (renderToString second)
