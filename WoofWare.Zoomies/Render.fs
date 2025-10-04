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
            FocusableKeys : OrderedSet<NodeKey>
            /// The key marked with isInitialFocus=true, if any
            InitialFocusKey : NodeKey option ref
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

    /// Query which key had focus in the previous frame, if you're using the automatic focus tracking mechanism.
    let focusedKey (s : RenderState) : NodeKey option = s.FocusedKey

    /// Query the rendered bounds of a keyed node
    let layoutOf (key : NodeKey) (s : RenderState) : Rectangle option =
        match s.KeyToNode.TryGetValue key with
        | true, node -> Some node.Bounds
        | false, _ -> None

    /// Advance focus to the next focusable node (Tab key)
    let advanceFocus (s : RenderState) : unit =
        if s.FocusableKeys.Count = 0 then
            // nothing to do, nothing can ever have focus
            s.FocusedKey <- None
        else

        match s.FocusedKey with
        | None ->
            // No current focus, use initial focus key if available, otherwise first focusable element
            match s.InitialFocusKey.Value with
            | Some initialKey when s.FocusableKeys.Contains initialKey -> s.FocusedKey <- Some initialKey
            | _ -> s.FocusedKey <- Some s.FocusableKeys.[0]
        | Some currentKey ->
            // Find the current key in the list and move to the next one
            match s.FocusableKeys |> Seq.tryFindIndex ((=) currentKey) with
            | Some index ->
                let nextIndex = (index + 1) % s.FocusableKeys.Count
                s.FocusedKey <- Some s.FocusableKeys.[nextIndex]
            | None ->
                // Current key is not in the focusable list, use initial focus key if available
                match s.InitialFocusKey.Value with
                | Some initialKey when s.FocusableKeys.Contains initialKey -> s.FocusedKey <- Some initialKey
                | _ -> s.FocusedKey <- Some s.FocusableKeys.[0]

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
            FocusableKeys = OrderedSet ()
            InitialFocusKey = ref None
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
        // TODO: work out how to display this differently when it has focus
        // TODO: test that a focusable text box can in fact gain focus, by writing a test UI that lets you type into
        // one of several text boxes
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
            VDomSource = vdom |> KeylessVdom.Unkeyed
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
        (initialFocusKey : NodeKey option ref)
        (dirty : TerminalCell voption[,])
        (bounds : Rectangle)
        (dir : Direction)
        (proportion : Choice<float, int>)
        (child1 : KeylessVdom<_>)
        (child2 : KeylessVdom<_>)
        (vdom : UnkeyedVdom<DesiredBounds>)
        =
        for y = 0 to bounds.Height - 1 do
            for x = 0 to bounds.Width - 1 do
                setAtRelativeOffset dirty bounds x y (ValueSome (TerminalCell.OfChar ' '))

        let bounds1, bounds2 = splitBounds dir proportion bounds

        let rendered1 =
            layoutEither dirty keyToNode focusableKeys initialFocusKey None bounds1 child1

        let rendered2 =
            layoutEither dirty keyToNode focusableKeys initialFocusKey None bounds2 child2

        {
            Bounds = bounds
            OverlaidChildren = [ rendered1 ; rendered2 ]
            VDomSource = KeylessVdom.Unkeyed vdom
            Self =
                UnkeyedVdom.PanelSplit (dir, proportion, rendered1.Self, rendered2.Self)
                |> KeylessVdom.Unkeyed
        }

    and private freshRenderBordered
        (keyToNode : _)
        (focusableKeys : _)
        (initialFocusKey : NodeKey option ref)
        (dirty : TerminalCell voption[,])
        (bounds : Rectangle)
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

        setAtRelativeOffset dirty bounds (bounds.Width - 1) (bounds.Height - 1) (ValueSome (TerminalCell.OfChar '┘'))

        for i = 1 to bounds.Width - 2 do
            setAtRelativeOffset dirty bounds i 0 (ValueSome (TerminalCell.OfChar '─'))

            setAtRelativeOffset dirty bounds i (bounds.Height - 1) (ValueSome (TerminalCell.OfChar '─'))

        for i = 1 to bounds.Height - 2 do
            setAtRelativeOffset dirty bounds 0 i (ValueSome (TerminalCell.OfChar '│'))

            setAtRelativeOffset dirty bounds (bounds.Width - 1) i (ValueSome (TerminalCell.OfChar '│'))

        let children =
            [
                layoutEither dirty keyToNode focusableKeys initialFocusKey None (shrinkBounds bounds) child
            ]

        {
            Bounds = bounds
            OverlaidChildren = children
            VDomSource = vdom |> KeylessVdom.Unkeyed
            Self = UnkeyedVdom.Bordered children.[0].Self |> KeylessVdom.Unkeyed
        }

    and private layoutEither
        (dirty : TerminalCell voption[,])
        (keyToNode : Dictionary<NodeKey, RenderedNode>)
        (focusableKeys : OrderedSet<NodeKey>)
        (initialFocusKey : NodeKey option ref)
        (previousRender : RenderedNode option)
        (bounds : Rectangle)
        (vdom : KeylessVdom<DesiredBounds>)
        : RenderedNode
        =
        match vdom with
        | KeylessVdom.Keyed vdom ->
            layout dirty keyToNode focusableKeys initialFocusKey previousRender bounds (Vdom.Keyed (vdom, Teq.refl))
        | KeylessVdom.Unkeyed vdom ->
            layout dirty keyToNode focusableKeys initialFocusKey previousRender bounds (Vdom.Unkeyed (vdom, Teq.refl))

    and internal layout<'key>
        (dirty : TerminalCell voption[,])
        (keyToNode : Dictionary<NodeKey, RenderedNode>)
        (focusableKeys : OrderedSet<NodeKey>)
        (initialFocusKey : NodeKey option ref)
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
                let previousRender =
                    match previousRender with
                    | Some previousRender when previousRender.Bounds = bounds ->
                        match previousRender.VDomSource with
                        | KeylessVdom.Keyed (KeyedVdom.WithKey (prevKey, _)) when prevKey = nodeKey ->
                            Some previousRender.OverlaidChildren.[0]
                        | _ -> None
                    | _ -> None

                let rendered =
                    layout
                        dirty
                        keyToNode
                        focusableKeys
                        initialFocusKey
                        previousRender
                        bounds
                        (Vdom.Unkeyed (unkeyedVdom, Teq.refl))

                keyToNode.[nodeKey] <- rendered

                {
                    Bounds = bounds
                    OverlaidChildren = [ rendered ]
                    VDomSource = keyedVdom |> KeylessVdom.Keyed
                    Self =
                        match rendered.Self with
                        | KeylessVdom.Keyed _ -> failwith "logic error: we are keyed"
                        | KeylessVdom.Unkeyed self -> KeyedVdom.WithKey (nodeKey, self) |> KeylessVdom.Keyed
                }

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
                    | KeylessVdom.Unkeyed (UnkeyedVdom.TextContent (prevText, prevFocus)) when
                        prevText = s && prevFocus = focus
                        ->
                        {
                            Bounds = bounds
                            OverlaidChildren = []
                            VDomSource = KeylessVdom.Unkeyed unkeyedVdom
                            Self = KeylessVdom.Unkeyed (UnkeyedVdom.TextContent (s, focus))
                        }
                    | _ -> freshRenderTextContent dirty bounds unkeyedVdom s focus
                | _ -> freshRenderTextContent dirty bounds unkeyedVdom s focus

            | UnkeyedVdom.PanelSplit (dir, proportion, child1, child2) ->
                match previousRender with
                | Some previousRender when previousRender.Bounds = bounds ->
                    match previousRender.VDomSource with
                    | KeylessVdom.Unkeyed (UnkeyedVdom.PanelSplit (prevDir, prevProportion, _, _)) when
                        proportion = prevProportion && prevDir = dir
                        ->
                        let bounds1, bounds2 = splitBounds dir proportion bounds

                        let rendered1 =
                            layoutEither
                                dirty
                                keyToNode
                                focusableKeys
                                initialFocusKey
                                (Some previousRender.OverlaidChildren.[0])
                                bounds1
                                child1

                        let rendered2 =
                            layoutEither
                                dirty
                                keyToNode
                                focusableKeys
                                initialFocusKey
                                (Some previousRender.OverlaidChildren.[1])
                                bounds2
                                child2

                        {
                            Bounds = bounds
                            OverlaidChildren = [ rendered1 ; rendered2 ]
                            VDomSource = KeylessVdom.Unkeyed unkeyedVdom
                            Self =
                                UnkeyedVdom.PanelSplit (dir, proportion, rendered1.Self, rendered2.Self)
                                |> KeylessVdom.Unkeyed
                        }
                    | _ ->
                        freshRenderPanelSplit
                            keyToNode
                            focusableKeys
                            initialFocusKey
                            dirty
                            bounds
                            dir
                            proportion
                            child1
                            child2
                            unkeyedVdom
                | _ ->
                    freshRenderPanelSplit
                        keyToNode
                        focusableKeys
                        initialFocusKey
                        dirty
                        bounds
                        dir
                        proportion
                        child1
                        child2
                        unkeyedVdom

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
                    | _ -> freshRenderCheckbox dirty bounds unkeyedVdom isChecked focus
                | _ -> freshRenderCheckbox dirty bounds unkeyedVdom isChecked focus

            | UnkeyedVdom.Bordered child ->
                match previousRender with
                | Some previousRender when previousRender.Bounds = bounds ->
                    match previousRender.VDomSource with
                    | KeylessVdom.Unkeyed (UnkeyedVdom.Bordered _) ->
                        let children =
                            [
                                layoutEither
                                    dirty
                                    keyToNode
                                    focusableKeys
                                    initialFocusKey
                                    (Some previousRender.OverlaidChildren.[0])
                                    (shrinkBounds bounds)
                                    child
                            ]

                        {
                            Bounds = bounds
                            OverlaidChildren = children
                            VDomSource = unkeyedVdom |> KeylessVdom.Unkeyed
                            Self = UnkeyedVdom.Bordered children.[0].Self |> KeylessVdom.Unkeyed
                        }

                    | _ -> freshRenderBordered keyToNode focusableKeys initialFocusKey dirty bounds child unkeyedVdom
                | _ -> freshRenderBordered keyToNode focusableKeys initialFocusKey dirty bounds child unkeyedVdom
            | Focusable (isInitialFocus, keyedVdom) ->
                match keyedVdom with
                | WithKey (key, _) ->
                    if not (focusableKeys.Add key) then
                        failwith "TODO: handle this gracefully depending on a global framework flag"

                    if isInitialFocus then
                        initialFocusKey.Value <- Some key

                let childPreviousRender =
                    match previousRender with
                    | Some previousRender when previousRender.Bounds = bounds ->
                        match previousRender.VDomSource with
                        | KeylessVdom.Unkeyed (UnkeyedVdom.Focusable _) -> Some previousRender.OverlaidChildren.[0]
                        | _ -> None
                    | _ -> None

                let child =
                    layout
                        dirty
                        keyToNode
                        focusableKeys
                        initialFocusKey
                        childPreviousRender
                        bounds
                        (Vdom.Keyed (keyedVdom, Teq.refl))

                {
                    Bounds = bounds
                    OverlaidChildren = [ child ]
                    VDomSource = unkeyedVdom |> KeylessVdom.Unkeyed
                    Self =
                        match child.Self with
                        | KeylessVdom.Keyed child ->
                            UnkeyedVdom.Focusable (isInitialFocus, child) |> KeylessVdom.Unkeyed
                        | KeylessVdom.Unkeyed _ -> failwith "logic error: child is keyed"
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

    let oneStep<'state>
        (renderState : RenderState)
        (userState : 'state)
        (compute : 'state -> Vdom<DesiredBounds, Unkeyed>)
        =
        Array.Clear renderState.Buffer
        renderState.KeyToNode.Clear ()
        renderState.FocusableKeys.Clear ()
        renderState.InitialFocusKey.Value <- None

        let vdom = compute userState

        let rendered =
            layout
                renderState.Buffer
                renderState.KeyToNode
                renderState.FocusableKeys
                renderState.InitialFocusKey
                renderState.PreviousVdom
                renderState.TerminalBounds
                vdom

        // If the focused element from the previous tick no longer exists, clear focused state
        match renderState.FocusedKey with
        | None -> ()
        | Some key ->
            if not (renderState.FocusableKeys.Contains key) then
                renderState.FocusedKey <- None

        renderState.PreviousVdom <- Some rendered

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
