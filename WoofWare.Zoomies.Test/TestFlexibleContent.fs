namespace WoofWare.Zoomies.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies

type FlexibleContentState =
    {
        Width : int
    }

    static member Empty () : FlexibleContentState =
        {
            Width = 80
        }

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFlexibleContent =
    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    /// A simple progress bar that renders differently based on width
    let progressBar (fraction : float) : Vdom<DesiredBounds, Unkeyed> =
        let measure (constraints : MeasureConstraints) =
            {
                MinWidth = 10
                PreferredWidth = 50
                MaxWidth = None
                MinHeightForWidth = fun _ -> 1
                PreferredHeightForWidth = fun _ -> 1
                MaxHeightForWidth = fun _ -> Some 1
            }

        let render (bounds : Rectangle) =
            let barWidth = bounds.Width
            let filledCells = int (float (barWidth - 2) * fraction)
            let emptyCells = (barWidth - 2) - filledCells

            // Ensure we don't have negative values
            let filledCells = max 0 filledCells
            let emptyCells = max 0 emptyCells

            let bar =
                "["
                + String.replicate filledCells "="
                + (if filledCells < (barWidth - 2) then ">" else "")
                + String.replicate (emptyCells - (if filledCells < (barWidth - 2) then 1 else 0)) " "
                + "]"

            Vdom.textContent false bar

        Vdom.flexibleContent measure render

    /// A responsive layout that switches between horizontal and vertical
    let responsiveLayout (content1 : string) (content2 : string) : Vdom<DesiredBounds, Unkeyed> =
        let measure (constraints : MeasureConstraints) =
            {
                MinWidth = 20
                PreferredWidth = 80
                MaxWidth = None
                MinHeightForWidth =
                    fun w ->
                        if w < 40 then
                            10 // Vertical layout
                        else
                            5 // Horizontal layout
                PreferredHeightForWidth = fun w -> if w < 40 then 10 else 5
                MaxHeightForWidth = fun _ -> None
            }

        let render (bounds : Rectangle) =
            if bounds.Width < 40 then
                // Vertical stack layout
                Vdom.panelSplitProportion (
                    SplitDirection.Horizontal,
                    0.5,
                    Vdom.textContent false content1,
                    Vdom.textContent false content2
                )
            else
                // Horizontal side-by-side layout
                Vdom.panelSplitProportion (
                    SplitDirection.Vertical,
                    0.5,
                    Vdom.textContent false content1,
                    Vdom.textContent false content2
                )

        Vdom.flexibleContent measure render

    [<Test>]
    let ``FlexibleContent renders different content at different widths`` () =
        let terminalOps1 = ResizeArray ()
        let terminalOps2 = ResizeArray ()

        // Create console with width 80
        let console1 =
            { IConsole.defaultForTests with
                WindowWidth = fun () -> 80
                WindowHeight = fun () -> 24
                Execute = fun x -> terminalOps1.Add x
            }

        // Create console with width 20
        let console2 =
            { IConsole.defaultForTests with
                WindowWidth = fun () -> 20
                WindowHeight = fun () -> 24
                Execute = fun x -> terminalOps2.Add x
            }

        let renderState1 = RenderState.make console1 None
        let renderState2 = RenderState.make console2 None

        let vdom1 = progressBar 0.6
        let vdom2 = progressBar 0.6

        Render.oneStep renderState1 () (fun _ -> vdom1)
        Render.oneStep renderState2 () (fun _ -> vdom2)

        // The two renders should produce different output
        // (one is wide, one is narrow)
        (terminalOps1.Count > 0) |> shouldEqual true
        (terminalOps2.Count > 0) |> shouldEqual true

        // They should be different
        terminalOps1.Count |> shouldNotEqual terminalOps2.Count

    [<Test>]
    let ``FlexibleContent early cutoff works when bounds don't change`` () =
        let terminalOps = ResizeArray ()

        let console =
            { IConsole.defaultForTests with
                WindowWidth = fun () -> 80
                WindowHeight = fun () -> 24
                Execute = fun x -> terminalOps.Add x
            }

        let renderState = RenderState.make console None

        let vdom = progressBar 0.6

        // First render
        Render.oneStep renderState () (fun _ -> vdom)

        terminalOps.Clear ()

        // Second render with same vdom - should trigger early cutoff
        Render.oneStep renderState () (fun _ -> vdom)

        // Should produce no terminal operations due to early cutoff
        terminalOps.Count |> shouldEqual 0

    [<Test>]
    let ``FlexibleContent with responsive layout changes structure`` () =
        let terminalOps = ResizeArray ()

        // Start with wide console
        let mutable width = 80

        let console =
            { IConsole.defaultForTests with
                WindowWidth = fun () -> width
                WindowHeight = fun () -> 24
                Execute = fun x -> terminalOps.Add x
            }

        let renderState = RenderState.make console None

        let vdom = responsiveLayout "Left" "Right"

        // First render at width 80 (horizontal layout)
        Render.oneStep renderState () (fun _ -> vdom)

        terminalOps.Clear ()

        // Update terminal size to width 30 (should switch to vertical layout)
        width <- 30
        RenderState.refreshTerminalSize renderState

        Render.oneStep renderState () (fun _ -> vdom)

        // Should have produced output due to structural change
        (terminalOps.Count > 0) |> shouldEqual true

    [<Test>]
    let ``FlexibleContent measurement respects constraints`` () =
        let measure (constraints : MeasureConstraints) =
            {
                MinWidth = 100 // Request more than constraint
                PreferredWidth = 200
                MaxWidth = None
                MinHeightForWidth = fun _ -> 1
                PreferredHeightForWidth = fun _ -> 1
                MaxHeightForWidth = fun _ -> Some 1
            }

        let render (bounds : Rectangle) =
            Vdom.textContent false (sprintf "Width: %d" bounds.Width)

        let vdom = Vdom.flexibleContent measure render

        let terminalOps = ResizeArray ()

        let console =
            { IConsole.defaultForTests with
                WindowWidth = fun () -> 50 // Constraint is narrower than request
                WindowHeight = fun () -> 24
                Execute = fun x -> terminalOps.Add x
            }

        let renderState = RenderState.make console None

        // Should not crash even though measurement requests more than available
        Render.oneStep renderState () (fun _ -> vdom)

        (terminalOps.Count > 0) |> shouldEqual true
