namespace WoofWare.Zoomies.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.Zoomies

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestKeyedLayout =

    /// The shape of a vdom, with the keys left implicit so that we can build it both with and without them.
    type VdomSpec =
        | Text of string
        | Empty
        | Bordered of VdomSpec
        | Split of SplitDirection * SplitBehaviour * VdomSpec * VdomSpec
        | Focusable of VdomSpec
        | Tag of string * VdomSpec
        | Flexible of VdomSpec
        /// Wrap the inner node in `Vdom.withKey`.
        | Keyed of VdomSpec

    /// Build the vdom described by the spec, assigning a fresh key wherever one is needed.
    /// If `includeKeys` is false, `VdomSpec.Keyed` nodes are omitted; but the keys of focusable nodes are
    /// identical whatever the value of `includeKeys`.
    let build (includeKeys : bool) (spec : VdomSpec) : Vdom<DesiredBounds> =
        let mutable counter = 0

        let freshKey () =
            counter <- counter + 1
            NodeKey.make $"k%i{counter}"

        let rec go (spec : VdomSpec) : Vdom<DesiredBounds> =
            match spec with
            | VdomSpec.Text s -> Vdom.textContent s
            | VdomSpec.Empty -> Vdom.empty
            | VdomSpec.Bordered inner -> Vdom.bordered (go inner)
            | VdomSpec.Split (dir, behaviour, child1, child2) ->
                let child1 = go child1
                let child2 = go child2
                Vdom.panelSplit (dir, behaviour, child1, child2) |> Vdom.Unkeyed
            | VdomSpec.Focusable inner ->
                let key = freshKey ()
                Vdom.withFocusTracking (go inner |> Vdom.withKey key)
            | VdomSpec.Tag (tag, inner) -> Vdom.withTag tag (go inner)
            | VdomSpec.Flexible inner ->
                let inner = go inner
                Vdom.flexibleContent (VdomBounds.measure inner) (fun _ -> inner)
            | VdomSpec.Keyed inner ->
                let key = freshKey ()
                let inner = go inner

                if includeKeys then
                    inner |> Vdom.withKey key |> Vdom.Keyed
                else
                    inner

        go spec

    let specGen : Gen<VdomSpec> =
        let leaf =
            Gen.oneof
                [
                    Gen.elements [ "" ; "x" ; "hello" ; "hello world" ; "a\nb" ]
                    |> Gen.map VdomSpec.Text
                    Gen.constant VdomSpec.Empty
                ]

        let behaviourGen =
            Gen.oneof
                [
                    Gen.elements [ 0.25 ; 0.5 ; 0.75 ] |> Gen.map SplitBehaviour.Proportion
                    Gen.choose (-4, 4) |> Gen.map SplitBehaviour.Absolute
                    gen {
                        let weight =
                            Gen.elements
                                [
                                    ExpansionWeight.FromContent
                                    ExpansionWeight.Fixed 1.0
                                    ExpansionWeight.Fixed 2.0
                                ]

                        let! w1 = weight
                        let! w2 = weight
                        return SplitBehaviour.AutoWeighted (w1, w2)
                    }
                ]

        let rec sized (size : int) : Gen<VdomSpec> =
            if size <= 0 then
                leaf
            else
                let sub = sized (size - 1)
                let half = sized (size / 2)

                Gen.oneof
                    [
                        leaf
                        Gen.map VdomSpec.Bordered sub
                        gen {
                            let! dir = Gen.elements [ SplitDirection.Vertical ; SplitDirection.Horizontal ]
                            let! behaviour = behaviourGen
                            let! child1 = half
                            let! child2 = half
                            return VdomSpec.Split (dir, behaviour, child1, child2)
                        }
                        Gen.map VdomSpec.Focusable sub
                        Gen.map (fun inner -> VdomSpec.Tag ("tag", inner)) sub
                        Gen.map VdomSpec.Flexible sub
                        Gen.map VdomSpec.Keyed sub
                    ]

        Gen.sized (fun size -> sized (min size 8))

    let private renderFrames (width : int) (height : int) (frames : Vdom<DesiredBounds> list) : string list =
        let console, terminal = ConsoleHarness.make' (fun () -> width) (fun () -> height)
        let renderState = RenderState.make<unit> console MockTime.getStaticUtcNow None

        frames
        |> List.map (fun vdom ->
            Render.oneStep renderState () (fun () -> vdom)
            ConsoleHarness.toString terminal
        )

    [<Test>]
    let ``Keyed focusable nested in a bordered node renders`` () =
        let vdom =
            Vdom.withFocusTracking (Vdom.textContent "x" |> Vdom.withKey (NodeKey.make "a"))
            |> Vdom.withKey (NodeKey.make "b")
            |> Vdom.Keyed
            |> Vdom.bordered

        renderFrames 5 3 [ vdom ]
        |> shouldEqual
            [
                "
┌───┐|
│x  │|
└───┘|
"
            ]

    [<Test>]
    let ``Adding keys does not change what is rendered`` () =
        let property (specs : VdomSpec list) (width : int) (height : int) =
            let render includeKeys =
                specs
                |> List.map (fun spec -> build includeKeys spec |> Vdom.bordered)
                |> renderFrames width height

            render true |> shouldEqual (render false)

        let gen =
            gen {
                let! first = specGen
                let! rest = Gen.listOf specGen |> Gen.map (List.truncate 3)
                let! width = Gen.choose (1, 20)
                let! height = Gen.choose (1, 8)
                return first :: rest, width, height
            }

        Check.One (propConfig.WithMaxTest 2000, Prop.forAll (Arb.fromGen gen) (fun (s, w, h) -> property s w h))

