namespace WoofWare.Zoomies

open System
open System.Collections.Generic
open TypeEquality

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
    private
        {
            Bounds : Rectangle
            OverlaidChildren : RenderedNode list
            VDomSource : KeylessVdom<DesiredBounds>
            Self : KeylessVdom<Rectangle>
        }

type RenderState =
    private
        {
            mutable PreviousVdom : RenderedNode option
            mutable Buffer : TerminalCell voption[,]
            mutable TerminalBounds : Rectangle
            mutable CursorVisible : bool
            Output : TerminalOp -> unit
            mutable BackgroundColor : ConsoleColor
            mutable ForegroundColor : ConsoleColor
            KeyToNode : Dictionary<NodeKey, RenderedNode>
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

    let private freshRenderTextContent
        (dirty : TerminalCell voption[,])
        (bounds : Rectangle)
        (vdom : UnkeyedVdom<DesiredBounds>)
        (content : string)
        (focus : bool)
        =
                // TODO: can do better here if we can compute a more efficient diff
                for y = 0 to bounds.Height - 1 do
                    for x = 0 to bounds.Width - 1 do
                        setAtRelativeOffset dirty bounds x y (ValueSome (TerminalCell.OfChar ' '))

                // dumb implementation! could do much better
                let mutable index = 0
                let mutable currX = 0
                let mutable currY = 0

                while index < content.Length do
                    setAtRelativeOffset dirty bounds currX currY (ValueSome (TerminalCell.OfChar (content.Chars index)))

                    currX <- currX + 1

                    if currX = bounds.Width then
                        currX <- 0
                        currY <- currY + 1

                        if currY >= bounds.Height then
                            index <- content.Length

                    index <- index + 1

                {
                    Bounds = bounds
                    OverlaidChildren = []
                    VDomSource =
                            vdom
                            |> KeylessVdom.Unkeyed
                    Self = UnkeyedVdom.TextContent (content, focus) |> KeylessVdom.Unkeyed
                }

    let private freshRenderCheckbox
        (dirty : TerminalCell voption[,])
        (bounds : Rectangle)
        (vdom : UnkeyedVdom<DesiredBounds>)
        (isChecked : bool)
        (focus : bool)
        =
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
                    VDomSource = KeylessVdom.Unkeyed vdom
                    Self = UnkeyedVdom.Checkbox (isChecked, focus) |> KeylessVdom.Unkeyed
                }

    let rec private freshRenderPanelSplit
        (keyToNode : _)
        (focusableKeys : _)
        (dirty : TerminalCell voption[,]) (bounds : Rectangle) (dir : Direction) (proportion : Choice<float, int>)
        (child1 : KeylessVdom<_>)
        (child2 : KeylessVdom<_>)
        (vdom : UnkeyedVdom<DesiredBounds>)
        =
                for y = 0 to bounds.Height - 1 do
                    for x = 0 to bounds.Width - 1 do
                        setAtRelativeOffset dirty bounds x y (ValueSome (TerminalCell.OfChar ' '))

                let bounds1, bounds2 = splitBounds dir proportion bounds
                let rendered1 =
                    layoutEither dirty keyToNode focusableKeys None bounds1 child1
                let rendered2 =
                    layoutEither dirty keyToNode focusableKeys None bounds2 child2

                {
                    Bounds = bounds
                    OverlaidChildren = [ rendered1 ; rendered2 ]
                    VDomSource = KeylessVdom.Unkeyed vdom
                    Self = UnkeyedVdom.PanelSplit (dir, proportion, rendered1.Self, rendered2.Self) |> KeylessVdom.Unkeyed
                }

    and private freshRenderBordered
        (keyToNode : _)
        (focusableKeys : _)
        (dirty : TerminalCell voption[,]) (bounds : Rectangle)
        (child : KeylessVdom<_>)
        (vdom : UnkeyedVdom<DesiredBounds>)
        =
                if bounds.Height <= 2 then
                    failwith $"TODO: too short: %O{bounds}"

                if bounds.Width <= 2 then
                    failwith $"TODO: too thin: %O{bounds}"

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

                let children =
                            [ layoutEither dirty keyToNode focusableKeys None (shrinkBounds bounds) child ]

                {
                    Bounds = bounds
                    OverlaidChildren = children
                    VDomSource =
                        vdom |> KeylessVdom.Unkeyed
                    Self = UnkeyedVdom.Bordered children.[0].Self |> KeylessVdom.Unkeyed
                }

    and private layoutEither
        (dirty : TerminalCell voption[,])
        (keyToNode : Dictionary<NodeKey, RenderedNode>)
        (focusableKeys : NodeKey list ref)
        (previousRender : RenderedNode option)
        (bounds : Rectangle)
        (vdom : KeylessVdom<DesiredBounds>)
        : RenderedNode
        =
        match vdom with
        | KeylessVdom.Keyed vdom -> layout dirty keyToNode focusableKeys previousRender bounds (Vdom.Keyed (vdom, Teq.refl))
        | KeylessVdom.Unkeyed vdom -> layout dirty keyToNode focusableKeys previousRender bounds (Vdom.Unkeyed (vdom, Teq.refl))

    and layout<'key>
        (dirty : TerminalCell voption[,])
        (keyToNode : Dictionary<NodeKey, RenderedNode>)
        (focusableKeys : NodeKey list ref)
        (previousRender : RenderedNode option)
        (bounds : Rectangle)
        (vdom : Vdom<DesiredBounds, 'key>)
        : RenderedNode
        =
        match vdom with
        | Keyed (keyedVdom, teq) ->
            match previousRender with
            | Some previousRender when
                bounds = previousRender.Bounds
                && KeylessVdom.referenceEquals (previousRender.VDomSource, keyedVdom)
                ->
                previousRender
            | _ ->

            match keyedVdom with
            | WithKey (nodeKey, unkeyedVdom) ->
                match previousRender with
                | Some previousRender when previousRender.Bounds = bounds ->
                    match previousRender.VDomSource with
                    | KeylessVdom.Keyed (KeyedVdom.WithKey (prevKey, prevVdom)) -> failwith "todo"
                    | _ -> layout dirty keyToNode focusableKeys None bounds (Vdom.Unkeyed (unkeyedVdom, Teq.refl))
                | _ -> layout dirty keyToNode focusableKeys None bounds (Vdom.Unkeyed (unkeyedVdom, Teq.refl))

        | Unkeyed (unkeyedVdom, teq) ->
            match previousRender with
            | Some previousRender when
                bounds = previousRender.Bounds
                && KeylessVdom.referenceEquals (previousRender.VDomSource, unkeyedVdom)
                ->
                previousRender
            | _ ->

            match unkeyedVdom with
            | UnkeyedVdom.TextContent (s, focus) ->
                match previousRender with
                | Some previousRender when previousRender.Bounds = bounds ->
                    match previousRender.VDomSource with
                    | KeylessVdom.Unkeyed (UnkeyedVdom.TextContent (prevText, prevFocus)) when prevText = s && prevFocus = focus ->
                        {
                            Bounds = bounds
                            OverlaidChildren = []
                            VDomSource = KeylessVdom.Unkeyed unkeyedVdom
                            Self = KeylessVdom.Unkeyed (UnkeyedVdom.TextContent (s, focus))
                        }
                    | _ ->
                        freshRenderTextContent dirty bounds unkeyedVdom s focus
                | _ ->
                    freshRenderTextContent dirty bounds unkeyedVdom s focus

            | UnkeyedVdom.PanelSplit (dir, proportion, child1, child2) ->
                match previousRender with
                | Some previousRender when previousRender.Bounds = bounds ->
                    match previousRender.VDomSource with
                    | KeylessVdom.Unkeyed (UnkeyedVdom.PanelSplit (prevDir, prevProportion, prevChild1, prevChild2)) when proportion = prevProportion && prevDir = dir ->
                        let bounds1, bounds2 = splitBounds dir proportion bounds

                        let rendered1 = layoutEither dirty keyToNode focusableKeys (Some (previousRender.OverlaidChildren.[0])) bounds1 child1
                        let rendered2 = layoutEither dirty keyToNode focusableKeys (Some (previousRender.OverlaidChildren.[1])) bounds2 child2

                        {
                            Bounds = bounds
                            OverlaidChildren = [ rendered1 ; rendered2 ]
                            VDomSource = KeylessVdom.Unkeyed unkeyedVdom
                            Self = UnkeyedVdom.PanelSplit (dir, proportion, rendered1.Self, rendered2.Self) |> KeylessVdom.Unkeyed
                        }
                    | _ -> freshRenderPanelSplit keyToNode focusableKeys dirty bounds dir proportion  child1 child2 unkeyedVdom
                | _ -> freshRenderPanelSplit keyToNode focusableKeys dirty bounds dir proportion child1 child2 unkeyedVdom

            | UnkeyedVdom.Checkbox (isChecked, focus) ->
                match previousRender with
                | Some previousRender when previousRender.Bounds = bounds ->
                    match previousRender.VDomSource with
                    | KeylessVdom.Unkeyed (UnkeyedVdom.Checkbox (prevChecked, prevFocus)) when focus = prevFocus ->
                        if prevChecked <> isChecked then
                            let content = if isChecked then '☑' else '☐'

                            setAtRelativeOffset
                                dirty
                                bounds
                                (bounds.Width / 2)
                                (bounds.Height / 2)
                                (ValueSome (TerminalCell.OfChar content))

                        {
                            Bounds = bounds
                            OverlaidChildren = []
                            VDomSource = unkeyedVdom |> KeylessVdom.Unkeyed
                            Self = UnkeyedVdom.Checkbox (isChecked, focus) |> KeylessVdom.Unkeyed
                        }
                    | _ ->
                        freshRenderCheckbox dirty bounds unkeyedVdom isChecked focus
                | _ ->
                    freshRenderCheckbox dirty bounds unkeyedVdom isChecked focus

            | UnkeyedVdom.Bordered child ->
                match previousRender with
                | Some previousRender when previousRender.Bounds = bounds ->
                    match previousRender.VDomSource with
                    | KeylessVdom.Unkeyed (UnkeyedVdom.Bordered prevInner) ->
                        let children =
                                            [
                                                layoutEither dirty keyToNode focusableKeys (Some (previousRender.OverlaidChildren.[0])) (shrinkBounds bounds) child
                                            ]

                        {
                            Bounds = bounds
                            OverlaidChildren = children
                            VDomSource = unkeyedVdom |> KeylessVdom.Unkeyed
                            Self = UnkeyedVdom.Bordered children.[0].Self |> KeylessVdom.Unkeyed
                        }

                    | _ -> freshRenderBordered keyToNode focusableKeys dirty bounds child unkeyedVdom
                | _ -> freshRenderBordered keyToNode focusableKeys dirty bounds child unkeyedVdom
            | Focusable keyedVdom ->
                match previousRender with
                | Some previousRender when previousRender.Bounds = bounds ->
                    match previousRender.VDomSource with
                    | KeylessVdom.Unkeyed (UnkeyedVdom.Focusable prevInner) ->
                        failwith "TODO"
                    | _ -> layout dirty keyToNode focusableKeys None bounds (Vdom.Keyed (keyedVdom, Teq.refl))
                | _ -> layout dirty keyToNode focusableKeys None bounds (Vdom.Keyed (keyedVdom, Teq.refl))

    let writeBuffer (dirty : TerminalCell voption[,]) : TerminalOp seq =
        // TODO this is super dumb
        seq {
            for y = 0 to dirty.GetLength 0 - 1 do
                for x = 0 to dirty.GetLength 1 - 1 do
                    match dirty.[y, x] with
                    | ValueNone -> ()
                    | ValueSome cell -> yield! [ TerminalOp.MoveCursor (x, y) ; TerminalOp.WriteChar cell ]
        }

    let oneStep<'state> (renderState : RenderState) (userState : 'state) (compute : 'state -> Vdom<DesiredBounds, Unkeyed>) =
        Array.Clear renderState.Buffer
        renderState.KeyToNode.Clear ()
        let focusableKeys = ref []
        let vdom = compute userState

        let rendered =
            layout renderState.Buffer renderState.KeyToNode focusableKeys renderState.PreviousVdom renderState.TerminalBounds vdom

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
