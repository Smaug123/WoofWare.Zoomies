namespace WoofWare.Zoomies

open System
open System.Collections.Generic
open TypeEquality

/// So that we can do early cutoff.
type RenderedNode =
    private
        {
            Bounds : Rectangle
            OverlaidChildren : RenderedNode list
            VDomSource : KeylessVdom<DesiredBounds>
            Self : KeylessVdom<Rectangle>
            ArrangedSource : Layout.ArrangedNode option
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
        (arranged : Layout.ArrangedNode option)
        =
        {
            Bounds = bounds
            OverlaidChildren = []
            VDomSource = vdom |> KeylessVdom.Unkeyed
            Self = UnkeyedVdom.TextContent (content, focus) |> KeylessVdom.Unkeyed
            ArrangedSource = arranged
        }

    let private freshRenderCheckbox
        (bounds : Rectangle)
        (vdom : UnkeyedVdom<DesiredBounds>)
        (isChecked : bool)
        (focus : bool)
        (arranged : Layout.ArrangedNode option)
        =
        if bounds.Width < 3 then
            failwith "TODO: not enough room"

        {
            Bounds = bounds
            OverlaidChildren = []
            VDomSource = KeylessVdom.Unkeyed vdom
            Self = UnkeyedVdom.Checkbox (isChecked, focus) |> KeylessVdom.Unkeyed
            ArrangedSource = arranged
        }

    /// Convert ArrangedNode to RenderedNode, populating keyToNode and focusableKeys
    let rec private arrangedToRendered
        (keyToNode : Dictionary<NodeKey, RenderedNode>)
        (focusableKeys : OrderedSet<NodeKey>)
        (initialFocusKey : NodeKey option ref)
        (arranged : Layout.ArrangedNode)
        (originalVdom : KeylessVdom<DesiredBounds>)
        : RenderedNode
        =
        let children =
            match originalVdom with
            | KeylessVdom.Keyed (KeyedVdom.WithKey (_, unkeyedVdom)) ->
                match unkeyedVdom with
                | UnkeyedVdom.Bordered child ->
                    [
                        arrangedToRendered keyToNode focusableKeys initialFocusKey arranged.Children.[0] child
                    ]
                | UnkeyedVdom.PanelSplit (_, _, child1, child2) ->
                    [
                        arrangedToRendered keyToNode focusableKeys initialFocusKey arranged.Children.[0] child1
                        arrangedToRendered keyToNode focusableKeys initialFocusKey arranged.Children.[1] child2
                    ]
                | UnkeyedVdom.Focusable (isInitial, KeyedVdom.WithKey (key, _)) ->
                    if not (focusableKeys.Add key) then
                        failwith "TODO: handle this gracefully depending on a global framework flag"

                    if isInitial then
                        initialFocusKey.Value <- Some key

                    [
                        arrangedToRendered keyToNode focusableKeys initialFocusKey arranged.Children.[0] originalVdom
                    ]
                | UnkeyedVdom.TextContent _
                | UnkeyedVdom.Checkbox _ -> []
            | KeylessVdom.Unkeyed unkeyedVdom ->
                match unkeyedVdom with
                | UnkeyedVdom.Bordered child ->
                    [
                        arrangedToRendered keyToNode focusableKeys initialFocusKey arranged.Children.[0] child
                    ]
                | UnkeyedVdom.PanelSplit (_, _, child1, child2) ->
                    [
                        arrangedToRendered keyToNode focusableKeys initialFocusKey arranged.Children.[0] child1
                        arrangedToRendered keyToNode focusableKeys initialFocusKey arranged.Children.[1] child2
                    ]
                | UnkeyedVdom.Focusable (isInitial, KeyedVdom.WithKey (key, childVdom)) ->
                    if not (focusableKeys.Add key) then
                        failwith "TODO: handle this gracefully depending on a global framework flag"

                    if isInitial then
                        initialFocusKey.Value <- Some key

                    [
                        arrangedToRendered
                            keyToNode
                            focusableKeys
                            initialFocusKey
                            arranged.Children.[0]
                            (KeylessVdom.Keyed (KeyedVdom.WithKey (key, childVdom)))
                    ]
                | UnkeyedVdom.TextContent _
                | UnkeyedVdom.Checkbox _ -> []

        let result =
            {
                Bounds = arranged.Bounds
                OverlaidChildren = children
                VDomSource = originalVdom
                Self = arranged.Vdom
                ArrangedSource = Some arranged
            }

        // Register keyed nodes
        match originalVdom with
        | KeylessVdom.Keyed (KeyedVdom.WithKey (key, _)) -> keyToNode.[key] <- result
        | KeylessVdom.Unkeyed (UnkeyedVdom.Focusable (_, KeyedVdom.WithKey (key, _))) ->
            // Register the focusable wrapper's key
            keyToNode.[key] <- children.[0]
        | _ -> ()

        result

    /// Top-level layout function that uses the two-pass system
    let internal layout
        (keyToNode : Dictionary<NodeKey, RenderedNode>)
        (focusableKeys : OrderedSet<NodeKey>)
        (initialFocusKey : NodeKey option ref)
        (previousRender : RenderedNode option)
        (bounds : Rectangle)
        (vdom : Vdom<DesiredBounds, Unkeyed>)
        : RenderedNode
        =
        // Early cutoff: if previous render exists with same bounds, try reusing it
        // Note: We don't check VDOM reference equality at the top level because the VDOM
        // is often created fresh each frame. Instead, we run the layout and rely on
        // nested early cutoffs within the tree to preserve unchanged subtrees.
        let earlyCutoff = None

        match earlyCutoff with
        | Some result -> result
        | None ->
            // Run the two-pass layout
            let arranged = Layout.layout vdom bounds

            // Convert to RenderedNode
            match vdom with
            | Vdom.Unkeyed (unkeyedVdom, _) ->
                arrangedToRendered keyToNode focusableKeys initialFocusKey arranged (KeylessVdom.Unkeyed unkeyedVdom)
            | Vdom.Keyed _ -> failwith "Top-level vdom must be unkeyed"

    let rec private renderToBuffer
        (dirty : TerminalCell voption[,])
        (previousNode : RenderedNode option)
        (node : RenderedNode)
        : unit
        =
        // Early cutoff: if this node is the same reference as the previous one, nothing changed
        match previousNode with
        | Some prev when Object.referenceEquals prev node -> ()
        | _ ->

        let bounds = node.Bounds

        match node.VDomSource with
        | KeylessVdom.Keyed (KeyedVdom.WithKey (_, unkeyedVdom)) ->
            // Keyed nodes just wrap their child
            match unkeyedVdom with
            | UnkeyedVdom.PanelSplit _ ->
                // PanelSplit has two children - render both
                let prevChild1 =
                    match previousNode with
                    | Some prev when prev.OverlaidChildren.Length >= 2 -> Some prev.OverlaidChildren.[0]
                    | _ -> None
                let prevChild2 =
                    match previousNode with
                    | Some prev when prev.OverlaidChildren.Length >= 2 -> Some prev.OverlaidChildren.[1]
                    | _ -> None

                renderToBuffer dirty prevChild1 node.OverlaidChildren.[0]
                renderToBuffer dirty prevChild2 node.OverlaidChildren.[1]
            | _ when node.OverlaidChildren.Length > 0 ->
                // Container node (Bordered, Focusable) - render single child
                let prevChild =
                    match previousNode with
                    | Some prev when prev.OverlaidChildren.Length > 0 -> Some prev.OverlaidChildren.[0]
                    | _ -> None

                renderToBuffer dirty prevChild node.OverlaidChildren.[0]
            | _ ->
                // Leaf node (TextContent/Checkbox) - render directly
                // Note: Focusable should have a child, but if we get here, treat it as transparent
                match unkeyedVdom with
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
                | UnkeyedVdom.Focusable _ ->
                    // Focusable wrapper with no children - this means it's registered
                    // via keyToNode, but the actual rendering is done via the child
                    // Do nothing here
                    ()
                | UnkeyedVdom.Bordered _ ->
                    failwith "Keyed Bordered node should have a child in OverlaidChildren"
                | UnkeyedVdom.PanelSplit _ ->
                    failwith "Keyed PanelSplit node should have children in OverlaidChildren"
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
                // Only paint background if this is a new node or bounds changed
                match previousNode with
                | Some prev when
                    prev.Bounds = bounds
                    && match prev.VDomSource with
                       | KeylessVdom.Unkeyed (UnkeyedVdom.PanelSplit _) -> true
                       | _ -> false
                    ->
                    // Container unchanged, skip background paint
                    ()
                | _ ->
                    // New container or bounds changed, paint background
                    for y = 0 to bounds.Height - 1 do
                        for x = 0 to bounds.Width - 1 do
                            setAtRelativeOffset dirty bounds x y (ValueSome (TerminalCell.OfChar ' '))

                // Render children with their previous versions
                let prevChild1 =
                    match previousNode with
                    | Some prev when
                        prev.OverlaidChildren.Length >= 2
                        && match prev.VDomSource with
                           | KeylessVdom.Unkeyed (UnkeyedVdom.PanelSplit _) -> true
                           | _ -> false
                        ->
                        Some prev.OverlaidChildren.[0]
                    | _ -> None

                let prevChild2 =
                    match previousNode with
                    | Some prev when
                        prev.OverlaidChildren.Length >= 2
                        && match prev.VDomSource with
                           | KeylessVdom.Unkeyed (UnkeyedVdom.PanelSplit _) -> true
                           | _ -> false
                        ->
                        Some prev.OverlaidChildren.[1]
                    | _ -> None

                renderToBuffer dirty prevChild1 node.OverlaidChildren.[0]
                renderToBuffer dirty prevChild2 node.OverlaidChildren.[1]

            | UnkeyedVdom.Bordered _ ->
                // Only paint background and border if this is a new node or bounds changed
                match previousNode with
                | Some prev when
                    prev.Bounds = bounds
                    && match prev.VDomSource with
                       | KeylessVdom.Unkeyed (UnkeyedVdom.Bordered _) -> true
                       | _ -> false
                    ->
                    // Container unchanged, skip background/border paint
                    ()
                | _ ->
                    // New container or bounds changed, paint background and border
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

                // Render child with its previous version
                let prevChild =
                    match previousNode with
                    | Some prev when
                        prev.OverlaidChildren.Length > 0
                        && match prev.VDomSource with
                           | KeylessVdom.Unkeyed (UnkeyedVdom.Bordered _) -> true
                           | _ -> false
                        ->
                        Some prev.OverlaidChildren.[0]
                    | _ -> None

                renderToBuffer dirty prevChild node.OverlaidChildren.[0]

            | UnkeyedVdom.Focusable _ ->
                // Focusable just wraps its child, render the child
                let prevChild =
                    match previousNode with
                    | Some prev when prev.OverlaidChildren.Length > 0 -> Some prev.OverlaidChildren.[0]
                    | _ -> None

                renderToBuffer dirty prevChild node.OverlaidChildren.[0]

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
        | Some prev when Object.referenceEquals prev layoutResult ->
            // Nothing changed, skip rendering
            ()
        | _ ->
            // Something changed, render to buffer
            renderToBuffer renderState.Buffer renderState.PreviousVdom layoutResult

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
                        haveManipulatedCursor <- true

                    renderState.Output (TerminalOp.MoveCursor (x, y))
                    renderState.Output (TerminalOp.WriteChar cell)

        if haveManipulatedCursor && cursorFlip then
            RenderState.setCursorVisible renderState
