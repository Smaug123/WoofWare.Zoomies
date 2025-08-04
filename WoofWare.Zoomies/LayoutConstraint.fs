namespace WoofWare.Zoomies

open System

/// Represents size constraints for a UI element
[<Struct>]
type SizeConstraints =
    {
        MinWidth : int
        MaxWidth : int
        MinHeight : int
        MaxHeight : int
    }

    static member Unconstrained =
        {
            MinWidth = 0
            MaxWidth = Int32.MaxValue
            MinHeight = 0
            MaxHeight = Int32.MaxValue
        }

    /// Clamp a size to fit within these constraints
    member this.Clamp (width : int, height : int) =
        let w = width |> max this.MinWidth |> min this.MaxWidth
        let h = height |> max this.MinHeight |> min this.MaxHeight
        w, h

    /// Check if a size satisfies these constraints
    member this.IsSatisfiedBy (width : int, height : int) =
        width >= this.MinWidth
        && width <= this.MaxWidth
        && height >= this.MinHeight
        && height <= this.MaxHeight

/// Extended VDOM node with layout information
type LayoutNode =
    {
        Vdom : Vdom
        Constraints : SizeConstraints
        Children : LayoutNode list
    }

type LayoutNoFit =
    {
        BoundWidth : int
        BoundHeight : int

        LayoutConstraints : SizeConstraints
    }

    override this.ToString () =
        $"Bounds {this.BoundWidth}x{this.BoundHeight} don't satisfy constraints {this.LayoutConstraints.MinWidth}-{this.LayoutConstraints.MaxWidth}x{this.LayoutConstraints.MinHeight}-{this.LayoutConstraints.MaxHeight}"

[<RequireQualifiedAccess>]
type LayoutFailure =
    | NoFit of LayoutNoFit
    | ChildCount of nodeType : string * required : int * got : int
    | SplitFailed

    override this.ToString () =
        match this with
        | LayoutFailure.NoFit layoutNoFit -> layoutNoFit.ToString ()
        | LayoutFailure.ChildCount (nodeType, required, got) ->
            $"%s{nodeType} node should have %i{required} child(ren) but got %i{got}"
        | LayoutFailure.SplitFailed ->
            // TODO: more info here
            "Cannot split bounds while satisfying constraints"

[<RequireQualifiedAccess>]
module ConstraintSolver =

    /// Calculate constraints for a given VDOM node (bottom-up pass)
    let rec calculateConstraints (vdom : Vdom) : LayoutNode =
        match vdom with
        | Vdom.TextContent (text, _, _) ->
            // Text needs at least 1x1, but ideally wants full text width/height
            let lines = text.Split ('\n')
            let lineCount = max 1 lines.Length

            let maxLineLength =
                if lines.Length = 0 then
                    1
                else
                    lines |> Array.map (fun l -> l.Length) |> Array.max |> max 1

            {
                Vdom = vdom
                Constraints =
                    {
                        MinWidth = 1 // Can show at least one character
                        MaxWidth = Int32.MaxValue
                        MinHeight = 1 // Can show at least one line
                        MaxHeight = Int32.MaxValue
                    }
                Children = []
            }

        | Vdom.Checkbox (_, focused, _) ->
            // Checkbox needs 1x1 minimum, 3x1 for focused state
            {
                Vdom = vdom
                Constraints =
                    {
                        MinWidth = 1 // Just the checkbox character
                        MaxWidth = Int32.MaxValue
                        MinHeight = 1
                        MaxHeight = Int32.MaxValue
                    }
                Children = []
            }

        | Vdom.Bordered child ->
            // Border needs 2 extra in each dimension
            let childLayout = calculateConstraints child

            {
                Vdom = vdom
                Constraints =
                    {
                        MinWidth = childLayout.Constraints.MinWidth + 2
                        MaxWidth =
                            if childLayout.Constraints.MaxWidth = Int32.MaxValue then
                                Int32.MaxValue
                            else
                                childLayout.Constraints.MaxWidth + 2
                        MinHeight = childLayout.Constraints.MinHeight + 2
                        MaxHeight =
                            if childLayout.Constraints.MaxHeight = Int32.MaxValue then
                                Int32.MaxValue
                            else
                                childLayout.Constraints.MaxHeight + 2
                    }
                Children = [ childLayout ]
            }

        | Vdom.PanelSplit (dir, split, child1, child2) ->
            let layout1 = calculateConstraints child1
            let layout2 = calculateConstraints child2

            let constraints =
                match dir with
                | Direction.Horizontal ->
                    // Stacked vertically
                    {
                        MinWidth = max layout1.Constraints.MinWidth layout2.Constraints.MinWidth
                        MaxWidth = min layout1.Constraints.MaxWidth layout2.Constraints.MaxWidth
                        MinHeight = layout1.Constraints.MinHeight + layout2.Constraints.MinHeight
                        MaxHeight =
                            if
                                layout1.Constraints.MaxHeight = Int32.MaxValue
                                || layout2.Constraints.MaxHeight = Int32.MaxValue
                            then
                                Int32.MaxValue
                            else
                                layout1.Constraints.MaxHeight + layout2.Constraints.MaxHeight
                    }
                | Direction.Vertical ->
                    // Side by side
                    {
                        MinWidth = layout1.Constraints.MinWidth + layout2.Constraints.MinWidth
                        MaxWidth =
                            if
                                layout1.Constraints.MaxWidth = Int32.MaxValue
                                || layout2.Constraints.MaxWidth = Int32.MaxValue
                            then
                                Int32.MaxValue
                            else
                                layout1.Constraints.MaxWidth + layout2.Constraints.MaxWidth
                        MinHeight = max layout1.Constraints.MinHeight layout2.Constraints.MinHeight
                        MaxHeight = min layout1.Constraints.MaxHeight layout2.Constraints.MaxHeight
                    }

            {
                Vdom = vdom
                Constraints = constraints
                Children = [ layout1 ; layout2 ]
            }
