namespace WoofWare.Zoomies

open System
open System.Collections.Generic

[<Struct>]
type Rectangle =
    {
        TopLeftX : int
        TopLeftY : int
        Width : int
        Height : int
    }

/// So that we can do early cutoff.
type RenderedNode<'keyed> =
    {
        Bounds : Rectangle
        OverlaidChildren : RenderedNode<'keyed> list
        VDomSource : Vdom<DesiredBounds, 'keyed>
        Self : Vdom<Rectangle, 'keyed>
    }

type RenderState =
    private
        {
            mutable PreviousVdom : RenderedNode<Unkeyed> option
            mutable Buffer : TerminalCell voption[,]
            mutable TerminalBounds : Rectangle
            mutable CursorVisible : bool
            Output : TerminalOp -> unit
            mutable BackgroundColor : ConsoleColor
            mutable ForegroundColor : ConsoleColor
            KeyToNode : Dictionary<NodeKey, RenderedNode<Keyed>>
            mutable FocusedKey : NodeKey option
            /// List of focusable keys in tree order (for Tab navigation)
            mutable FocusableKeys : NodeKey list
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

    let registerMouseMode (s : RenderState) = s.Output TerminalOp.RegisterMouseMode
    let unregisterMouseMode (s : RenderState) = s.Output TerminalOp.UnregisterMouseMode

    let registerBracketedPaste (s : RenderState) =
        s.Output TerminalOp.RegisterBracketedPaste

    let unregisterBracketedPaste (s : RenderState) =
        s.Output TerminalOp.UnregisterBracketedPaste

    /// Query which key had focus in the previous frame
    let focusedKey (s : RenderState) : NodeKey option = s.FocusedKey

    /// Query the rendered bounds of a keyed node
    let layoutOf (key : NodeKey) (s : RenderState) : Rectangle option =
        match s.KeyToNode.TryGetValue key with
        | true, node -> Some node.Bounds
        | false, _ -> None

    /// Advance focus to the next focusable node (Tab key)
    let advanceFocus (s : RenderState) : unit =
        match s.FocusedKey with
        | None ->
            // No current focus, focus the first focusable element
            s.FocusedKey <- s.FocusableKeys |> List.tryHead
        | Some currentKey ->
            // Find the current key in the list and move to the next one
            match s.FocusableKeys |> List.tryFindIndex ((=) currentKey) with
            | Some index ->
                let nextIndex = (index + 1) % s.FocusableKeys.Length
                s.FocusedKey <- Some s.FocusableKeys.[nextIndex]
            | None ->
                // Current key is not in the focusable list, focus the first one
                s.FocusedKey <- s.FocusableKeys |> List.tryHead

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
            KeyToNode = Dictionary<NodeKey, RenderedNode> ()
            FocusedKey = None
            FocusableKeys = []
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
            Width = bounds.Width - 2
            Height = bounds.Height - 2
        }

    let inline private yIndex (bounds : Rectangle) (relativeY : int) = bounds.TopLeftY + relativeY
    let inline private xIndex (bounds : Rectangle) (relativeX : int) = bounds.TopLeftX + relativeX

    let private setAtRelativeOffset (arr : 'a[,]) (bounds : Rectangle) (relativeX : int) (relativeY : int) (v : 'a) =
        arr.[yIndex bounds relativeY, xIndex bounds relativeX] <- v

    let rec layout
        (dirty : TerminalCell voption[,])
        (keyToNode : Dictionary<NodeKey, RenderedNode>)
        (focusableKeys : NodeKey list ref)
        (previousVdom : (Vdom<Rectangle> * RenderedNode) option)
        (bounds : Rectangle)
        (vdom : Vdom<DesiredBounds>)
        : RenderedNode
        =
        match previousVdom with
        | Some (_, previousNode) when
            bounds = previousNode.Bounds
            && Object.referenceEquals previousNode.VDomSource vdom
            ->
            previousNode
        | _ ->

        match vdom with
        | Vdom.Keyed keyedVdom ->
            // Render the inner node and track it in the dictionary
            let rendered = layout dirty keyToNode focusableKeys previousVdom bounds keyedVdom.Inner
            keyToNode.[keyedVdom.Key] <- rendered
            rendered

        | Vdom.Unkeyed unkeyedVdom ->
            match unkeyedVdom with
            | UnkeyedVdom.Focusable keyedVdom ->
                // Mark this key as focusable and render the inner node
                focusableKeys.Value <- focusableKeys.Value @ [keyedVdom.Key]
                // Render the keyed inner vdom
                let rendered = layout dirty keyToNode focusableKeys previousVdom bounds keyedVdom.Inner
                keyToNode.[keyedVdom.Key] <- rendered
                rendered

            | UnkeyedVdom.TextContent (s, focus) ->
                let vdomWithRect = Vdom.Unkeyed (UnkeyedVdom.TextContent (s, focus))
                match previousVdom with
                | Some (prev, prevNode) when
                    prevNode.Bounds = bounds && prev = vdomWithRect
                    ->
                    {
                        Bounds = bounds
                        OverlaidChildren = []
                        VDomSource = vdom
                        Self = vdomWithRect
                    }
                | _ ->
                    // TODO: can do better here if we can compute a more efficient diff
                    for y = 0 to bounds.Height - 1 do
                        for x = 0 to bounds.Width - 1 do
                            setAtRelativeOffset dirty bounds x y (ValueSome (TerminalCell.OfChar ' '))

                    // dumb implementation! could do much better
                    let mutable index = 0
                    let mutable currX = 0
                    let mutable currY = 0

                    while index < s.Length do
                        setAtRelativeOffset dirty bounds currX currY (ValueSome (TerminalCell.OfChar (s.Chars index)))

                        currX <- currX + 1

                        if currX = bounds.Width then
                            currX <- 0
                            currY <- currY + 1

                            if currY >= bounds.Height then
                                index <- s.Length

                        index <- index + 1

                    {
                        Bounds = bounds
                        OverlaidChildren = []
                        VDomSource = vdom
                        Self = vdomWithRect
                    }

            | UnkeyedVdom.PanelSplit (dir, proportion, child1, child2) ->
                let bounds1, bounds2 = splitBounds dir proportion bounds
                let rendered1 = layout dirty keyToNode focusableKeys None bounds1 child1
                let rendered2 = layout dirty keyToNode focusableKeys None bounds2 child2

                let vdomWithRect = Vdom.Unkeyed (UnkeyedVdom.PanelSplit (dir, proportion, rendered1.Self, rendered2.Self))

                match previousVdom with
                | Some (prev, prevNode) when
                    proportion = proportion && dir = dir && bounds = prevNode.Bounds
                    ->
                    {
                        Bounds = bounds
                        OverlaidChildren = [ rendered1 ; rendered2 ]
                        VDomSource = vdom
                        Self = vdomWithRect
                    }
                | _ ->
                    for y = 0 to bounds.Height - 1 do
                        for x = 0 to bounds.Width - 1 do
                            setAtRelativeOffset dirty bounds x y (ValueSome (TerminalCell.OfChar ' '))

                    {
                        Bounds = bounds
                        OverlaidChildren = [ rendered1 ; rendered2 ]
                        VDomSource = vdom
                        Self = vdomWithRect
                    }

            | UnkeyedVdom.Checkbox (isChecked, focus) ->
                let vdomWithRect = Vdom.Unkeyed (UnkeyedVdom.Checkbox (isChecked, focus))
                match previousVdom with
                | Some (prev, prevNode) when
                    prevNode.Bounds = bounds && prev = vdomWithRect
                    ->
                    {
                        Bounds = bounds
                        OverlaidChildren = []
                        VDomSource = vdom
                        Self = vdomWithRect
                    }
                | _ ->
                    // TODO: can short circuit this if focus is the only thing that's changed, too

                    if bounds.Width < 3 then
                        failwith "TODO: not enough room"

                    for y = 0 to bounds.Height - 1 do
                        for x = 0 to bounds.Width - 1 do
                            setAtRelativeOffset dirty bounds x y (ValueSome (TerminalCell.OfChar ' '))

                    let content = if isChecked then '☑' else '☐'

                    if focus then
                        setAtRelativeOffset
                            dirty
                            bounds
                            (bounds.Width / 2 - 1)
                            (bounds.Height / 2)
                            (ValueSome (TerminalCell.OfChar '['))

                        setAtRelativeOffset
                            dirty
                            bounds
                            (bounds.Width / 2 + 1)
                            (bounds.Height / 2)
                            (ValueSome (TerminalCell.OfChar ']'))

                    setAtRelativeOffset
                        dirty
                        bounds
                        (bounds.Width / 2)
                        (bounds.Height / 2)
                        (ValueSome (TerminalCell.OfChar content))

                    {
                        Bounds = bounds
                        OverlaidChildren = []
                        VDomSource = vdom
                        Self = vdomWithRect
                    }

            | UnkeyedVdom.Bordered child ->
                if bounds.Height <= 2 then
                    failwith $"TODO: too short: %O{bounds}"

                if bounds.Width <= 2 then
                    failwith $"TODO: too thin: %O{bounds}"

                let children = [ layout dirty keyToNode focusableKeys None (shrinkBounds bounds) child ]
                let vdomWithRect = Vdom.Unkeyed (UnkeyedVdom.Bordered children.[0].Self)

                match previousVdom with
                | Some (prev, prevNode) when prevNode.Bounds = bounds && prev = vdomWithRect ->
                    {
                        Bounds = bounds
                        OverlaidChildren = children
                        VDomSource = vdom
                        Self = vdomWithRect
                    }
                | _ ->
                    for y = 0 to bounds.Height - 1 do
                        for x = 0 to bounds.Width - 1 do
                            setAtRelativeOffset dirty bounds x y (ValueSome (TerminalCell.OfChar ' '))

                    setAtRelativeOffset dirty bounds 0 0 (ValueSome (TerminalCell.OfChar '┌'))

                    setAtRelativeOffset dirty bounds 0 (bounds.Height - 1) (ValueSome (TerminalCell.OfChar '└'))

                    setAtRelativeOffset dirty bounds (bounds.Width - 1) 0 (ValueSome (TerminalCell.OfChar '┐'))

                    setAtRelativeOffset
                        dirty
                        bounds
                        (bounds.Width - 1)
                        (bounds.Height - 1)
                        (ValueSome (TerminalCell.OfChar '┘'))

                    for i = 1 to bounds.Width - 2 do
                        setAtRelativeOffset dirty bounds i 0 (ValueSome (TerminalCell.OfChar '─'))

                        setAtRelativeOffset dirty bounds i (bounds.Height - 1) (ValueSome (TerminalCell.OfChar '─'))

                    for i = 1 to bounds.Height - 2 do
                        setAtRelativeOffset dirty bounds 0 i (ValueSome (TerminalCell.OfChar '│'))

                        setAtRelativeOffset dirty bounds (bounds.Width - 1) i (ValueSome (TerminalCell.OfChar '│'))

                    {
                        Bounds = bounds
                        OverlaidChildren = children
                        VDomSource = vdom
                        Self = vdomWithRect
                    }

    let writeBuffer (dirty : TerminalCell voption[,]) : TerminalOp seq =
        // TODO this is super dumb
        seq {
            for y = 0 to dirty.GetLength 0 - 1 do
                for x = 0 to dirty.GetLength 1 - 1 do
                    match dirty.[y, x] with
                    | ValueNone -> ()
                    | ValueSome cell -> yield! [ TerminalOp.MoveCursor (x, y) ; TerminalOp.WriteChar cell ]
        }

    let oneStep<'state, 'keyed> (renderState : RenderState) (userState : 'state) (compute : 'state -> Vdom<DesiredBounds>) =
        Array.Clear renderState.Buffer
        renderState.KeyToNode.Clear ()
        let focusableKeys = ref []
        let vdom = compute userState

        let rendered =
            let prev = renderState.PreviousVdom |> Option.map (fun node -> node.Self, node)
            layout renderState.Buffer renderState.KeyToNode focusableKeys prev renderState.TerminalBounds vdom

        renderState.PreviousVdom <- Some rendered
        renderState.FocusableKeys <- focusableKeys.Value

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
