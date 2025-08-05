namespace WoofWare.Zoomies

open System

[<Struct>]
type Rectangle =
    {
        TopLeftX : int
        TopLeftY : int
        Width : int
        Height : int
    }

/// So that we can do early cutoff.
type RenderedNode =
    {
        Bounds : Rectangle
        OverlaidChildren : RenderedNode list
        VDomSource : Vdom
    }

type RenderState =
    private
        {
            mutable PreviousVdom : (Vdom * RenderedNode) option
            mutable Buffer : TerminalCell voption[,]
            mutable TerminalBounds : Rectangle
            mutable CursorVisible : bool
            Output : TerminalOp -> unit
            mutable BackgroundColor : ConsoleColor
            mutable ForegroundColor : ConsoleColor
        }

[<RequireQualifiedAccess>]
module RenderState =
    let isCursorVisible (s : RenderState) = s.CursorVisible

    let setCursorVisible (s : RenderState) =
        s.Output (TerminalOp.SetCursorVisibility true)
        s.CursorVisible <- true

    let setCursorInvisible (s : RenderState) =
        s.Output (TerminalOp.SetCursorVisibility false)
        s.CursorVisible <- false

    let clearScreen (s : RenderState) = s.Output TerminalOp.ClearScreen

    let enterAlternateScreen (s : RenderState) =
        s.Output TerminalOp.EnterAlternateScreen

    let exitAlternateScreen (s : RenderState) = s.Output TerminalOp.ExitAlternateScreen

    let make' (c : IConsole) =
        let width = c.WindowWidth ()
        let height = c.WindowHeight ()

        let bounds =
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = width
                Height = height
            }

        let changeBuffer = Array2D.zeroCreate height width

        let bg = c.BackgroundColor ()
        let fg = c.ForegroundColor ()

        {
            TerminalBounds = bounds
            Buffer = changeBuffer
            PreviousVdom = None
            Output = c.Execute
            CursorVisible = true
            BackgroundColor = bg
            ForegroundColor = fg
        }

    let make () =
        let console = IConsole.make ()
        make' console

[<RequireQualifiedAccess>]
module Render =
    let private splitBounds
        (direction : Direction)
        (split : Choice<float, int>)
        (bounds : Rectangle)
        : Rectangle * Rectangle
        =
        match direction with
        | Direction.Vertical ->
            let leftWidth =
                match split with
                | Choice1Of2 proportion -> max (float bounds.Width * proportion |> int) 1
                | Choice2Of2 absolute -> if absolute < 0 then bounds.Width + absolute else absolute

            let left =
                {
                    TopLeftX = bounds.TopLeftX
                    TopLeftY = bounds.TopLeftY
                    Height = bounds.Height
                    Width = leftWidth
                }

            let right =
                {
                    TopLeftX = bounds.TopLeftX + leftWidth
                    TopLeftY = bounds.TopLeftY
                    Height = bounds.Height
                    Width = bounds.Width - leftWidth
                }

            left, right
        | Direction.Horizontal ->
            let topHeight =
                match split with
                | Choice1Of2 proportion -> max (float bounds.Height * proportion |> int) 1
                | Choice2Of2 absolute -> if absolute < 0 then bounds.Height + absolute else absolute

            let top =
                {
                    TopLeftX = bounds.TopLeftX
                    TopLeftY = bounds.TopLeftY
                    Width = bounds.Width
                    Height = topHeight
                }

            let bottom =
                {
                    TopLeftX = bounds.TopLeftX
                    TopLeftY = bounds.TopLeftY + topHeight
                    Width = bounds.Width
                    Height = bounds.Height - topHeight
                }

            top, bottom

    let private shrinkBounds (bounds : Rectangle) : Rectangle =
        {
            TopLeftX = bounds.TopLeftX + 1
            TopLeftY = bounds.TopLeftY + 1
            Width = max 0 (bounds.Width - 2)
            Height = max 0 (bounds.Height - 2)
        }

    let inline private yIndex (bounds : Rectangle) (relativeY : int) = bounds.TopLeftY + relativeY
    let inline private xIndex (bounds : Rectangle) (relativeX : int) = bounds.TopLeftX + relativeX

    let private setAtRelativeOffset (arr : 'a[,]) (bounds : Rectangle) (relativeX : int) (relativeY : int) (v : 'a) =
        arr.[yIndex bounds relativeY, xIndex bounds relativeX] <- v

    /// Apply layout with given bounds (top-down pass)
    let rec applyLayout
        (bounds : Rectangle)
        (previousVdom : (Vdom * RenderedNode) option)
        (layoutNode : LayoutNode)
        (buffer : TerminalCell voption[,])
        : Result<RenderedNode, LayoutFailure>
        =

        // First check: if vdom reference is the same and bounds haven't changed, skip entirely
        match previousVdom with
        | Some (previousVdom, previousNode) when
            bounds = previousNode.Bounds
            && Object.ReferenceEquals (previousNode.VDomSource, layoutNode.Vdom)
            ->
            Ok previousNode
        | _ ->

        // Check if bounds satisfy constraints
        if not (layoutNode.Constraints.IsSatisfiedBy (bounds.Width, bounds.Height)) then
            {
                BoundWidth = bounds.Width
                BoundHeight = bounds.Height

                LayoutConstraints = layoutNode.Constraints
            }
            |> fun x -> LayoutFailure.NoFit (x, layoutNode.Vdom)
            |> Error
        else

        match layoutNode.Vdom with
        | Vdom.TextContent (text, focused, _) ->
            // Check if we can skip re-rendering
            match previousVdom with
            | Some (Vdom.TextContent (prevText, prevFocus, _), prevNode) when
                prevNode.Bounds = bounds && prevText = text && prevFocus = focused
                ->
                {
                    Bounds = bounds
                    OverlaidChildren = []
                    VDomSource = layoutNode.Vdom
                }
                |> Ok
            | _ ->
                // Render text with wrapping/truncation
                renderText buffer bounds text

                {
                    Bounds = bounds
                    OverlaidChildren = []
                    VDomSource = layoutNode.Vdom
                }
                |> Ok

        | Vdom.Checkbox (isChecked, focused, _) ->
            // Check if we can do a minimal update
            match previousVdom with
            | Some (Vdom.Checkbox (prevChecked, prevFocus, _), prevNode) when
                prevNode.Bounds = bounds && focused = prevFocus
                ->
                if prevChecked <> isChecked then
                    // Only update the checkbox character itself
                    let checkChar = if isChecked then '☑' else '☐'
                    let centerY = bounds.Height / 2
                    let centerX = bounds.Width / 2
                    setAtRelativeOffset buffer bounds centerX centerY (ValueSome (TerminalCell.OfChar checkChar))

                {
                    Bounds = bounds
                    OverlaidChildren = []
                    VDomSource = layoutNode.Vdom
                }
                |> Ok
            | _ ->
                // Full render
                renderCheckbox buffer bounds isChecked focused

                {
                    Bounds = bounds
                    OverlaidChildren = []
                    VDomSource = layoutNode.Vdom
                }
                |> Ok

        | Vdom.Bordered child ->
            let childBounds = shrinkBounds bounds

            match layoutNode.Children with
            | [ childLayout ] ->
                match previousVdom with
                | Some (Vdom.Bordered prevInner, prevNode) when prevNode.Bounds = bounds ->
                    // Border hasn't changed, just recurse for child
                    match
                        applyLayout childBounds (Some (prevInner, prevNode.OverlaidChildren.[0])) childLayout buffer
                    with
                    | Ok childNode ->
                        {
                            Bounds = bounds
                            OverlaidChildren = [ childNode ]
                            VDomSource = layoutNode.Vdom
                        }
                        |> Ok
                    | Error reason -> Error reason
                | _ ->
                    // Draw border
                    renderBorder buffer bounds
                    // Recursively layout child
                    match applyLayout childBounds None childLayout buffer with
                    | Ok childNode ->
                        {
                            Bounds = bounds
                            OverlaidChildren = [ childNode ]
                            VDomSource = layoutNode.Vdom
                        }
                        |> Ok
                    | Error reason -> Error reason
            | l -> Error (LayoutFailure.ChildCount ("Bordered", 1, l.Length))

        | Vdom.PanelSplit (dir, split, child1, child2) ->
            match layoutNode.Children with
            | [ layout1 ; layout2 ] ->
                match previousVdom with
                | Some (Vdom.PanelSplit (prevDir, prevSplit, prevChild1, prevChild2), prevNode) when
                    split = prevSplit && prevDir = dir && bounds = prevNode.Bounds
                    ->
                    // Split hasn't changed, just recurse for children
                    let bounds1, bounds2 = splitBounds dir split bounds

                    match
                        applyLayout bounds1 (Some (prevChild1, prevNode.OverlaidChildren.[0])) layout1 buffer,
                        applyLayout bounds2 (Some (prevChild2, prevNode.OverlaidChildren.[1])) layout2 buffer
                    with
                    | Ok node1, Ok node2 ->
                        {
                            Bounds = bounds
                            OverlaidChildren = [ node1 ; node2 ]
                            VDomSource = layoutNode.Vdom
                        }
                        |> Ok
                    | Error reason, _
                    | _, Error reason -> Error reason
                | _ ->
                    match splitBoundsWithConstraints dir split bounds layout1.Constraints layout2.Constraints with
                    | Some (b1, b2) ->
                        match applyLayout b1 None layout1 buffer, applyLayout b2 None layout2 buffer with
                        | Ok node1, Ok node2 ->
                            {
                                Bounds = bounds
                                OverlaidChildren = [ node1 ; node2 ]
                                VDomSource = layoutNode.Vdom
                            }
                            |> Ok
                        | Error reason, _
                        | _, Error reason -> Error reason
                    | None -> Error LayoutFailure.SplitFailed

            | l -> LayoutFailure.ChildCount ("PanelSplit", 2, l.Length) |> Error

    /// Split bounds respecting child constraints
    /// If both children's constraints cannot be satisfied, returns None for both.
    /// The algorithm prioritizes satisfying minimum constraints over maintaining the exact split ratio.
    and private splitBoundsWithConstraints
        (direction : Direction)
        (split : Choice<float, int>)
        (bounds : Rectangle)
        (constraints1 : SizeConstraints)
        (constraints2 : SizeConstraints)
        : (Rectangle * Rectangle) option
        =

        match direction with
        | Direction.Vertical ->
            // Calculate initial split
            let targetWidth1 =
                match split with
                | Choice1Of2 proportion -> int (float bounds.Width * proportion)
                | Choice2Of2 absolute -> if absolute < 0 then bounds.Width + absolute else absolute

            // Adjust to satisfy constraints
            let width1 =
                targetWidth1
                |> max constraints1.MinWidth
                |> min constraints1.MaxWidth
                |> min (bounds.Width - constraints2.MinWidth)

            let width2 = bounds.Width - width1

            // Check if both constraints can be satisfied
            if
                width1 >= constraints1.MinWidth
                && width1 <= constraints1.MaxWidth
                && width2 >= constraints2.MinWidth
                && width2 <= constraints2.MaxWidth
            then
                let bounds1 =
                    {
                        TopLeftX = bounds.TopLeftX
                        TopLeftY = bounds.TopLeftY
                        Width = width1
                        Height = bounds.Height
                    }

                let bounds2 =
                    {
                        TopLeftX = bounds.TopLeftX + width1
                        TopLeftY = bounds.TopLeftY
                        Width = width2
                        Height = bounds.Height
                    }

                Some (bounds1, bounds2)
            else
                None

        | Direction.Horizontal ->
            // Similar logic for horizontal split
            let targetHeight1 =
                match split with
                | Choice1Of2 proportion -> int (float bounds.Height * proportion)
                | Choice2Of2 absolute -> if absolute < 0 then bounds.Height + absolute else absolute

            let height1 =
                targetHeight1
                |> max constraints1.MinHeight
                |> min constraints1.MaxHeight
                |> min (bounds.Height - constraints2.MinHeight)

            let height2 = bounds.Height - height1

            if
                height1 >= constraints1.MinHeight
                && height1 <= constraints1.MaxHeight
                && height2 >= constraints2.MinHeight
                && height2 <= constraints2.MaxHeight
            then
                let bounds1 =
                    {
                        TopLeftX = bounds.TopLeftX
                        TopLeftY = bounds.TopLeftY
                        Width = bounds.Width
                        Height = height1
                    }

                let bounds2 =
                    {
                        TopLeftX = bounds.TopLeftX
                        TopLeftY = bounds.TopLeftY + height1
                        Width = bounds.Width
                        Height = height2
                    }

                Some (bounds1, bounds2)
            else
                None

    /// Helper rendering functions with graceful degradation
    /// Note: Assumes setAtRelativeOffset is available from the Render module
    and private renderText (buffer : TerminalCell voption[,]) (bounds : Rectangle) (text : string) =
        // Clear area
        for y = 0 to bounds.Height - 1 do
            for x = 0 to bounds.Width - 1 do
                setAtRelativeOffset buffer bounds x y (ValueSome (TerminalCell.OfChar ' '))

        // Render text with wrapping
        let mutable charIndex = 0
        let mutable x = 0
        let mutable y = 0

        while charIndex < text.Length && y < bounds.Height do
            if text.[charIndex] = '\n' then
                x <- 0
                y <- y + 1
                charIndex <- charIndex + 1
            elif x < bounds.Width then
                setAtRelativeOffset buffer bounds x y (ValueSome (TerminalCell.OfChar text.[charIndex]))
                x <- x + 1
                charIndex <- charIndex + 1
            else
                // Need to wrap to next line
                x <- 0
                y <- y + 1

    and private renderCheckbox
        (buffer : TerminalCell voption[,])
        (bounds : Rectangle)
        (isChecked : bool)
        (focused : bool)
        =
        // Clear area first
        for y = 0 to bounds.Height - 1 do
            for x = 0 to bounds.Width - 1 do
                setAtRelativeOffset buffer bounds x y (ValueSome (TerminalCell.OfChar ' '))

        let checkChar = if isChecked then '☑' else '☐'
        let centerY = bounds.Height / 2

        // Graceful degradation based on available width
        match bounds.Width with
        | 0 -> () // Nothing we can do
        | 1 ->
            // Just the checkbox
            setAtRelativeOffset buffer bounds 0 centerY (ValueSome (TerminalCell.OfChar checkChar))
        | 2 ->
            // Checkbox with space
            setAtRelativeOffset buffer bounds 0 centerY (ValueSome (TerminalCell.OfChar checkChar))
        | w when w >= 3 && focused ->
            // Focused: center with brackets
            let centerX = w / 2
            setAtRelativeOffset buffer bounds (centerX - 1) centerY (ValueSome (TerminalCell.OfChar '['))
            setAtRelativeOffset buffer bounds centerX centerY (ValueSome (TerminalCell.OfChar checkChar))
            setAtRelativeOffset buffer bounds (centerX + 1) centerY (ValueSome (TerminalCell.OfChar ']'))
        | w ->
            // Unfocused: just center the checkbox
            let centerX = w / 2
            setAtRelativeOffset buffer bounds centerX centerY (ValueSome (TerminalCell.OfChar checkChar))

    and private renderBorder (buffer : TerminalCell voption[,]) (bounds : Rectangle) =
        // Degrade gracefully if bounds too small
        if bounds.Width >= 2 && bounds.Height >= 2 then
            // Full border rendering (same as original)
            setAtRelativeOffset buffer bounds 0 0 (ValueSome (TerminalCell.OfChar '┌'))
            setAtRelativeOffset buffer bounds 0 (bounds.Height - 1) (ValueSome (TerminalCell.OfChar '└'))
            setAtRelativeOffset buffer bounds (bounds.Width - 1) 0 (ValueSome (TerminalCell.OfChar '┐'))

            setAtRelativeOffset
                buffer
                bounds
                (bounds.Width - 1)
                (bounds.Height - 1)
                (ValueSome (TerminalCell.OfChar '┘'))

            for i = 1 to bounds.Width - 2 do
                setAtRelativeOffset buffer bounds i 0 (ValueSome (TerminalCell.OfChar '─'))
                setAtRelativeOffset buffer bounds i (bounds.Height - 1) (ValueSome (TerminalCell.OfChar '─'))

            for i = 1 to bounds.Height - 2 do
                setAtRelativeOffset buffer bounds 0 i (ValueSome (TerminalCell.OfChar '│'))
                setAtRelativeOffset buffer bounds (bounds.Width - 1) i (ValueSome (TerminalCell.OfChar '│'))
        elif bounds.Width = 1 && bounds.Height >= 1 then
            // Just vertical lines
            for i = 0 to bounds.Height - 1 do
                setAtRelativeOffset buffer bounds 0 i (ValueSome (TerminalCell.OfChar '│'))
        elif bounds.Height = 1 && bounds.Width >= 1 then
            // Just horizontal line
            for i = 0 to bounds.Width - 1 do
                setAtRelativeOffset buffer bounds i 0 (ValueSome (TerminalCell.OfChar '─'))

    let layoutWithConstraints
        (buffer : TerminalCell voption[,])
        (previousVdom : (Vdom * RenderedNode) option)
        (bounds : Rectangle)
        (vdom : Vdom)
        : Result<_, LayoutFailure>
        =
        let layoutTree = ConstraintSolver.calculateConstraints vdom
        applyLayout bounds previousVdom layoutTree buffer

    let writeBuffer (dirty : TerminalCell voption[,]) : TerminalOp seq =
        // TODO this is super dumb
        seq {
            for y = 0 to dirty.GetLength 0 - 1 do
                for x = 0 to dirty.GetLength 1 - 1 do
                    match dirty.[y, x] with
                    | ValueNone -> ()
                    | ValueSome cell -> yield! [ TerminalOp.MoveCursor (x, y) ; TerminalOp.WriteChar cell ]
        }

    let oneStep<'state> (renderState : RenderState) (userState : 'state) (compute : 'state -> Vdom) =
        Array.Clear renderState.Buffer
        let vdom = compute userState

        let rendered =
            match layoutWithConstraints renderState.Buffer renderState.PreviousVdom renderState.TerminalBounds vdom with
            | Ok r -> r
            | Error e ->
                // TODO: handle gracefully
                failwith $"render failure: %O{e}"

        renderState.PreviousVdom <- Some (vdom, rendered)

        let cursorFlip = RenderState.isCursorVisible renderState
        let mutable haveManipulatedCursor = false

        for y = 0 to renderState.Buffer.GetLength 0 - 1 do
            for x = 0 to renderState.Buffer.GetLength 1 - 1 do
                match renderState.Buffer.[y, x] with
                | ValueNone -> ()
                | ValueSome cell ->
                    if not haveManipulatedCursor && cursorFlip then
                        RenderState.setCursorInvisible renderState

                    renderState.Output (TerminalOp.MoveCursor (x, y))
                    renderState.Output (TerminalOp.WriteChar cell)

        if haveManipulatedCursor && cursorFlip then
            RenderState.setCursorVisible renderState
