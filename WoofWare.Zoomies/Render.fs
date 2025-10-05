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

/// Context provided to vdom construction, containing information about the layout of the previous render cycle.
/// This is mutable (although you aren't given the tools to mutate it), so don't persist it.
type VdomContext =
    private
        {
            /// The currently focused element's key, if any.
            /// If you're not using the automatic focus handling mechanism, this is always None.
            mutable _FocusedKey : NodeKey option
            /// The bounds of the terminal
            mutable _TerminalBounds : Rectangle
            mutable IsDirty : bool
        }

[<RequireQualifiedAccess>]
module VdomContext =
    let internal empty (terminalBounds : Rectangle) =
        {
            _TerminalBounds = terminalBounds
            _FocusedKey = None
            IsDirty = true
        }

    let internal setFocusedKey (key : NodeKey option) (v : VdomContext) =
        if v._FocusedKey <> key then
            v.IsDirty <- true
            v._FocusedKey <- key

    let internal setTerminalBounds (tb : Rectangle) (v : VdomContext) =
        if v._TerminalBounds <> tb then
            v.IsDirty <- true
            v._TerminalBounds <- tb

    let internal markClean (v : VdomContext) = v.IsDirty <- false

    let terminalBounds (v : VdomContext) = v._TerminalBounds
    let focusedKey (v : VdomContext) = v._FocusedKey

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
            Console : IConsole
            mutable PreviousVdom : RenderedNode option
            mutable Buffer : TerminalCell voption[,]
            mutable CursorVisible : bool
            Output : TerminalOp -> unit
            mutable BackgroundColor : ConsoleColor
            mutable ForegroundColor : ConsoleColor
            KeyToNode : Dictionary<NodeKey, RenderedNode>
            /// List of focusable keys in tree order (for Tab navigation)
            FocusableKeys : OrderedSet<NodeKey>
            /// The key marked with isInitialFocus=true, if any
            InitialFocusKey : NodeKey option ref
            /// This gets handed out to users every so often: it's the fragment of state that they will want to
            /// construct the vdom with.
            VdomContext : VdomContext
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

    /// Query the rendered bounds of a keyed node
    let layoutOf (key : NodeKey) (s : RenderState) : Rectangle option =
        match s.KeyToNode.TryGetValue key with
        | true, node -> Some node.Bounds
        | false, _ -> None

    let private getBounds (c : IConsole) : Rectangle =
        let width = c.WindowWidth ()
        let height = c.WindowHeight ()

        {
            TopLeftX = 0
            TopLeftY = 0
            Width = width
            Height = height
        }

    let refreshTerminalSize (rs : RenderState) : unit =
        VdomContext.setTerminalBounds (getBounds rs.Console) rs.VdomContext

    /// Advance focus to the next focusable node (Tab key)
    let advanceFocus (s : RenderState) : unit =
        if s.FocusableKeys.Count = 0 then
            // nothing to do, nothing can ever have focus
            VdomContext.setFocusedKey None s.VdomContext
        else

        match VdomContext.focusedKey s.VdomContext with
        | None ->
            // No current focus, use initial focus key if available, otherwise first focusable element
            match s.InitialFocusKey.Value with
            | Some initialKey when s.FocusableKeys.Contains initialKey ->
                VdomContext.setFocusedKey (Some initialKey) s.VdomContext
            | _ -> VdomContext.setFocusedKey (Some s.FocusableKeys.[0]) s.VdomContext
        | Some currentKey ->
            // Find the current key in the list and move to the next one
            match s.FocusableKeys |> Seq.tryFindIndex ((=) currentKey) with
            | Some index ->
                let nextIndex = (index + 1) % s.FocusableKeys.Count
                VdomContext.setFocusedKey (Some s.FocusableKeys.[nextIndex]) s.VdomContext
            | None ->
                // Current key is not in the focusable list, use initial focus key if available
                match s.InitialFocusKey.Value with
                | Some initialKey when s.FocusableKeys.Contains initialKey ->
                    VdomContext.setFocusedKey (Some initialKey) s.VdomContext
                | _ -> VdomContext.setFocusedKey (Some s.FocusableKeys.[0]) s.VdomContext

    /// Retreat focus to the previous focusable node (Shift+Tab key)
    let retreatFocus (s : RenderState) : unit =
        if s.FocusableKeys.Count = 0 then
            // nothing to do, nothing can ever have focus
            VdomContext.setFocusedKey None s.VdomContext
        else

        match VdomContext.focusedKey s.VdomContext with
        | None ->
            // No current focus, use initial focus key if available, otherwise last focusable element
            match s.InitialFocusKey.Value with
            | Some initialKey when s.FocusableKeys.Contains initialKey ->
                VdomContext.setFocusedKey (Some initialKey) s.VdomContext
            | _ -> VdomContext.setFocusedKey (Some s.FocusableKeys.[s.FocusableKeys.Count - 1]) s.VdomContext
        | Some currentKey ->
            // Find the current key in the list and move to the previous one
            match s.FocusableKeys |> Seq.tryFindIndex ((=) currentKey) with
            | Some index ->
                let prevIndex = (index - 1 + s.FocusableKeys.Count) % s.FocusableKeys.Count
                VdomContext.setFocusedKey (Some s.FocusableKeys.[prevIndex]) s.VdomContext
            | None ->
                // Current key is not in the focusable list, use initial focus key if available
                match s.InitialFocusKey.Value with
                | Some initialKey when s.FocusableKeys.Contains initialKey ->
                    VdomContext.setFocusedKey (Some initialKey) s.VdomContext
                | _ -> VdomContext.setFocusedKey (Some s.FocusableKeys.[s.FocusableKeys.Count - 1]) s.VdomContext

    let internal vdomContext (rs : RenderState) = rs.VdomContext

    let make' (c : IConsole) =
        let bounds = getBounds c

        let changeBuffer = Array2D.zeroCreate bounds.Height bounds.Width

        let bg = c.BackgroundColor ()
        let fg = c.ForegroundColor ()

        {
            Console = c
            Buffer = changeBuffer
            PreviousVdom = None
            Output = c.Execute
            CursorVisible = true
            BackgroundColor = bg
            ForegroundColor = fg
            KeyToNode = Dictionary<NodeKey, RenderedNode> ()
            FocusableKeys = OrderedSet ()
            InitialFocusKey = ref None
            VdomContext = VdomContext.empty bounds
        }

    let make () =
        let console = IConsole.make ()
        make' console

[<RequireQualifiedAccess>]
module Render =
    let private splitBounds
        (direction : SplitDirection)
        (split : SplitBehaviour)
        (bounds : Rectangle)
        : Rectangle * Rectangle
        =
        match direction with
        | SplitDirection.Vertical ->
            let leftWidth =
                match split with
                | SplitBehaviour.Proportion proportion -> max (float bounds.Width * proportion |> int) 1
                | SplitBehaviour.Absolute absolute -> if absolute < 0 then bounds.Width + absolute else absolute

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
        | SplitDirection.Horizontal ->
            let topHeight =
                match split with
                | SplitBehaviour.Proportion proportion -> max (float bounds.Height * proportion |> int) 1
                | SplitBehaviour.Absolute absolute -> if absolute < 0 then bounds.Height + absolute else absolute

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
        (bounds : Rectangle)
        (vdom : UnkeyedVdom<DesiredBounds>)
        (content : string)
        (focus : bool)
        =
        {
            Bounds = bounds
            OverlaidChildren = []
            VDomSource = vdom |> KeylessVdom.Unkeyed
            Self = UnkeyedVdom.TextContent (content, focus) |> KeylessVdom.Unkeyed
        }

    let private freshRenderCheckbox
        (bounds : Rectangle)
        (vdom : UnkeyedVdom<DesiredBounds>)
        (isChecked : bool)
        (focus : bool)
        =
        if bounds.Width < 3 then
            failwith "TODO: not enough room"

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
        (bounds : Rectangle)
        (dir : SplitDirection)
        (proportion : SplitBehaviour)
        (child1 : KeylessVdom<_>)
        (child2 : KeylessVdom<_>)
        (vdom : UnkeyedVdom<DesiredBounds>)
        =
        let bounds1, bounds2 = splitBounds dir proportion bounds

        let rendered1 =
            layoutEither keyToNode focusableKeys initialFocusKey None bounds1 child1

        let rendered2 =
            layoutEither keyToNode focusableKeys initialFocusKey None bounds2 child2

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
        (bounds : Rectangle)
        (child : KeylessVdom<_>)
        (vdom : UnkeyedVdom<DesiredBounds>)
        =
        if bounds.Height <= 2 then
            failwith $"TODO: too short: %O{bounds}"

        if bounds.Width <= 2 then
            failwith $"TODO: too thin: %O{bounds}"

        let children =
            [
                layoutEither keyToNode focusableKeys initialFocusKey None (shrinkBounds bounds) child
            ]

        {
            Bounds = bounds
            OverlaidChildren = children
            VDomSource = vdom |> KeylessVdom.Unkeyed
            Self = UnkeyedVdom.Bordered children.[0].Self |> KeylessVdom.Unkeyed
        }

    and private layoutEither
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
            layout keyToNode focusableKeys initialFocusKey previousRender bounds (Vdom.Keyed (vdom, Teq.refl))
        | KeylessVdom.Unkeyed vdom ->
            layout keyToNode focusableKeys initialFocusKey previousRender bounds (Vdom.Unkeyed (vdom, Teq.refl))

    and internal layout<'key>
        (keyToNode : Dictionary<NodeKey, RenderedNode>)
        (focusableKeys : OrderedSet<NodeKey>)
        (initialFocusKey : NodeKey option ref)
        (previousRender : RenderedNode option)
        (bounds : Rectangle)
        (vdom : Vdom<DesiredBounds, 'key>)
        : RenderedNode
        =
        match vdom with
        | Keyed (keyedVdom, _) ->
            match previousRender with
            | Some previousRender when
                bounds = previousRender.Bounds
                && KeylessVdom.referenceEquals (previousRender.VDomSource, keyedVdom)
                ->
                previousRender
            | _ ->

            match keyedVdom with
            | WithKey (nodeKey, unkeyedVdom) ->
                let parentPreviousRender = previousRender

                let childPreviousRender =
                    match previousRender with
                    | Some previousRender when previousRender.Bounds = bounds ->
                        match previousRender.VDomSource with
                        | KeylessVdom.Keyed (KeyedVdom.WithKey (prevKey, _)) when prevKey = nodeKey ->
                            Some previousRender.OverlaidChildren.[0]
                        | _ -> None
                    | _ -> None

                let rendered =
                    layout
                        keyToNode
                        focusableKeys
                        initialFocusKey
                        childPreviousRender
                        bounds
                        (Vdom.Unkeyed (unkeyedVdom, Teq.refl))

                keyToNode.[nodeKey] <- rendered

                // If child is unchanged (same reference), return the parent's previous render
                match parentPreviousRender, childPreviousRender with
                | Some parentPrev, Some childPrev when Object.ReferenceEquals (rendered, childPrev) -> parentPrev
                | _ ->
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
                        previousRender
                    | _ -> freshRenderTextContent bounds unkeyedVdom s focus
                | _ -> freshRenderTextContent bounds unkeyedVdom s focus

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
                                keyToNode
                                focusableKeys
                                initialFocusKey
                                (Some previousRender.OverlaidChildren.[0])
                                bounds1
                                child1

                        let rendered2 =
                            layoutEither
                                keyToNode
                                focusableKeys
                                initialFocusKey
                                (Some previousRender.OverlaidChildren.[1])
                                bounds2
                                child2

                        // If both children are unchanged (same reference), return the previous render
                        if
                            Object.ReferenceEquals (rendered1, previousRender.OverlaidChildren.[0])
                            && Object.ReferenceEquals (rendered2, previousRender.OverlaidChildren.[1])
                        then
                            previousRender
                        else
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
                    | KeylessVdom.Unkeyed (UnkeyedVdom.Checkbox (prevChecked, prevFocus)) when
                        prevChecked = isChecked && focus = prevFocus
                        ->
                        previousRender
                    | _ -> freshRenderCheckbox bounds unkeyedVdom isChecked focus
                | _ -> freshRenderCheckbox bounds unkeyedVdom isChecked focus

            | UnkeyedVdom.Bordered child ->
                match previousRender with
                | Some previousRender when previousRender.Bounds = bounds ->
                    match previousRender.VDomSource with
                    | KeylessVdom.Unkeyed (UnkeyedVdom.Bordered _) ->
                        let renderedChild =
                            layoutEither
                                keyToNode
                                focusableKeys
                                initialFocusKey
                                (Some previousRender.OverlaidChildren.[0])
                                (shrinkBounds bounds)
                                child

                        // If child is unchanged (same reference), return the previous render
                        if Object.ReferenceEquals (renderedChild, previousRender.OverlaidChildren.[0]) then
                            previousRender
                        else
                            {
                                Bounds = bounds
                                OverlaidChildren = [ renderedChild ]
                                VDomSource = unkeyedVdom |> KeylessVdom.Unkeyed
                                Self = UnkeyedVdom.Bordered renderedChild.Self |> KeylessVdom.Unkeyed
                            }

                    | _ -> freshRenderBordered keyToNode focusableKeys initialFocusKey bounds child unkeyedVdom
                | _ -> freshRenderBordered keyToNode focusableKeys initialFocusKey bounds child unkeyedVdom
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
                        keyToNode
                        focusableKeys
                        initialFocusKey
                        childPreviousRender
                        bounds
                        (Vdom.Keyed (keyedVdom, Teq.refl))

                // If child is unchanged (same reference), return the previous render
                match previousRender with
                | Some previousRender when
                    previousRender.OverlaidChildren.Length > 0
                    && Object.ReferenceEquals (child, previousRender.OverlaidChildren.[0])
                    ->
                    previousRender
                | _ ->
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

    let rec private renderToBuffer (dirty : TerminalCell voption[,]) (node : RenderedNode) : unit =
        let bounds = node.Bounds

        match node.VDomSource with
        | KeylessVdom.Keyed vdom ->
            // Keyed nodes just wrap their child, render the child
            renderToBuffer dirty node.OverlaidChildren.[0]
        | KeylessVdom.Unkeyed vdom ->
            match vdom with
            | UnkeyedVdom.TextContent (content, focus) ->
                // TODO: can do better here if we can compute a more efficient diff
                // TODO: work out how to display this differently when it has focus
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

            | UnkeyedVdom.Checkbox (isChecked, focus) ->
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

            | UnkeyedVdom.PanelSplit _ ->
                // Paint background spaces
                for y = 0 to bounds.Height - 1 do
                    for x = 0 to bounds.Width - 1 do
                        setAtRelativeOffset dirty bounds x y (ValueSome (TerminalCell.OfChar ' '))

                // Render children
                for child in node.OverlaidChildren do
                    renderToBuffer dirty child

            | UnkeyedVdom.Bordered _ ->
                // Paint background and border
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

                // Render child
                renderToBuffer dirty node.OverlaidChildren.[0]

            | UnkeyedVdom.Focusable _ ->
                // Focusable just wraps its child, render the child
                renderToBuffer dirty node.OverlaidChildren.[0]

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
        do
            let terminalHeight = VdomContext.terminalBounds(renderState.VdomContext).Height
            let terminalWidth = VdomContext.terminalBounds(renderState.VdomContext).Width

            if
                renderState.Buffer.GetLength 0 <> terminalHeight
                || renderState.Buffer.GetLength 1 <> terminalWidth
            then
                renderState.Buffer <- Array2D.zeroCreate terminalHeight terminalWidth
            else
                Array.Clear renderState.Buffer

        renderState.KeyToNode.Clear ()
        renderState.FocusableKeys.Clear ()
        renderState.InitialFocusKey.Value <- None

        let vdom = compute userState

        // Phase 1: Compute layout (bounds only)
        let layoutResult =
            layout
                renderState.KeyToNode
                renderState.FocusableKeys
                renderState.InitialFocusKey
                renderState.PreviousVdom
                (VdomContext.terminalBounds renderState.VdomContext)
                vdom

        // If the focused element from the previous tick no longer exists, clear focused state
        match VdomContext.focusedKey renderState.VdomContext with
        | None -> ()
        | Some key ->
            if not (renderState.FocusableKeys.Contains key) then
                VdomContext.setFocusedKey None renderState.VdomContext

        // Phase 2: Render to buffer (only if something changed)
        match renderState.PreviousVdom with
        | Some prev when Object.ReferenceEquals (prev, layoutResult) ->
            // Nothing changed, skip rendering
            ()
        | _ ->
            // Something changed, render to buffer
            renderToBuffer renderState.Buffer layoutResult

        renderState.PreviousVdom <- Some layoutResult

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
