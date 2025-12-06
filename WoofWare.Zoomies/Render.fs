namespace WoofWare.Zoomies

open System
open System.Collections.Generic

/// So that we can do early cutoff.
type RenderedNode =
    private
        {
            Bounds : Rectangle
            OverlaidChildren : RenderedNode list
            VDomSource : Vdom<DesiredBounds>
            Self : Vdom<Rectangle>
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
            /// The key marked with isFirstToFocus=true, if any
            FirstToFocusKey : NodeKey option ref
            /// The key marked with isInitiallyFocused=true, if any
            InitiallyFocusedKey : NodeKey option ref
            /// This gets handed out to users every so often: it's the fragment of state that they will want to
            /// construct the vdom with.
            VdomContext : VdomContext
            /// Debug file writer for layout diagnostics (if WOOFWARE_ZOOMIES_DEBUG_TO_FILE is enabled)
            DebugWriter : IO.StreamWriter option
        }

    interface IDisposable with
        member this.Dispose () =
            match this.DebugWriter with
            | Some writer -> writer.Dispose ()
            | None -> ()

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
            // No current focus, use first-to-focus key if available, otherwise first focusable element
            match s.FirstToFocusKey.Value with
            | Some firstKey when s.FocusableKeys.Contains firstKey ->
                VdomContext.setFocusedKey (Some firstKey) s.VdomContext
            | _ -> VdomContext.setFocusedKey (Some s.FocusableKeys.[0]) s.VdomContext
        | Some currentKey ->
            // Find the current key in the list and move to the next one
            match s.FocusableKeys |> Seq.tryFindIndex ((=) currentKey) with
            | Some index ->
                let nextIndex = (index + 1) % s.FocusableKeys.Count
                VdomContext.setFocusedKey (Some s.FocusableKeys.[nextIndex]) s.VdomContext
            | None ->
                // Current key is not in the focusable list, use first-to-focus key if available
                match s.FirstToFocusKey.Value with
                | Some firstKey when s.FocusableKeys.Contains firstKey ->
                    VdomContext.setFocusedKey (Some firstKey) s.VdomContext
                | _ -> VdomContext.setFocusedKey (Some s.FocusableKeys.[0]) s.VdomContext

    /// Retreat focus to the previous focusable node (Shift+Tab key)
    let retreatFocus (s : RenderState) : unit =
        if s.FocusableKeys.Count = 0 then
            // nothing to do, nothing can ever have focus
            VdomContext.setFocusedKey None s.VdomContext
        else

        match VdomContext.focusedKey s.VdomContext with
        | None ->
            // No current focus, use first-to-focus key if available, otherwise last focusable element
            match s.FirstToFocusKey.Value with
            | Some firstKey when s.FocusableKeys.Contains firstKey ->
                VdomContext.setFocusedKey (Some firstKey) s.VdomContext
            | _ -> VdomContext.setFocusedKey (Some s.FocusableKeys.[s.FocusableKeys.Count - 1]) s.VdomContext
        | Some currentKey ->
            // Find the current key in the list and move to the previous one
            match s.FocusableKeys |> Seq.tryFindIndex ((=) currentKey) with
            | Some index ->
                let prevIndex = (index - 1 + s.FocusableKeys.Count) % s.FocusableKeys.Count
                VdomContext.setFocusedKey (Some s.FocusableKeys.[prevIndex]) s.VdomContext
            | None ->
                // Current key is not in the focusable list, use first-to-focus key if available
                match s.FirstToFocusKey.Value with
                | Some firstKey when s.FocusableKeys.Contains firstKey ->
                    VdomContext.setFocusedKey (Some firstKey) s.VdomContext
                | _ -> VdomContext.setFocusedKey (Some s.FocusableKeys.[s.FocusableKeys.Count - 1]) s.VdomContext

    let internal vdomContext (rs : RenderState) = rs.VdomContext

    /// Get the currently focused key, if any
    let focusedKey (rs : RenderState) : NodeKey option = VdomContext.focusedKey rs.VdomContext

    let internal make (c : IConsole) (getUtcNow : unit -> DateTime) (debugWriter : IO.StreamWriter option) =
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
            FirstToFocusKey = ref None
            InitiallyFocusedKey = ref None
            VdomContext = VdomContext.empty getUtcNow bounds
            DebugWriter = debugWriter
        }

[<RequireQualifiedAccess>]
module Render =
    let inline private yIndex (bounds : Rectangle) (relativeY : int) = bounds.TopLeftY + relativeY
    let inline private xIndex (bounds : Rectangle) (relativeX : int) = bounds.TopLeftX + relativeX

    let private setAtRelativeOffset (arr : 'a[,]) (bounds : Rectangle) (relativeX : int) (relativeY : int) (v : 'a) =
        arr.[yIndex bounds relativeY, xIndex bounds relativeX] <- v

    /// Convert ArrangedNode to RenderedNode, populating keyToNode and focusableKeys
    let rec private arrangedToRendered
        (keyToNode : Dictionary<NodeKey, RenderedNode>)
        (focusableKeys : OrderedSet<NodeKey>)
        (firstToFocusKey : NodeKey option ref)
        (initiallyFocusedKey : NodeKey option ref)
        (previousRender : RenderedNode option)
        (arranged : Layout.ArrangedNode)
        (originalVdom : Vdom<DesiredBounds>)
        : RenderedNode
        =
        // Early cutoff: check if we can reuse previousRender based on structural equality
        let earlyCutoffResult =
            match previousRender with
            | Some prev when prev.Bounds = arranged.Bounds ->
                match originalVdom, prev.VDomSource with
                // Keyed nodes with same reference - reuse previous
                | Vdom.Keyed (KeyedVdom (key1, vdom1)), Vdom.Keyed (KeyedVdom (key2, vdom2)) when key1 = key2 ->
                    match vdom1, vdom2 with
                    | UnkeyedVdom.TextContent (text1, style1, align1, focus1),
                      UnkeyedVdom.TextContent (text2, style2, align2, focus2) when
                        text1 = text2 && style1 = style2 && align1 = align2 && focus1 = focus2
                        ->
                        // Repopulate keyToNode for reused keyed node
                        keyToNode.[key1] <- prev
                        Some prev
                    | UnkeyedVdom.Empty, UnkeyedVdom.Empty ->
                        // Empty nodes are always equal, repopulate keyToNode and reuse
                        keyToNode.[key1] <- prev
                        Some prev
                    | UnkeyedVdom.Bordered child1, UnkeyedVdom.Bordered _ when prev.OverlaidChildren.Length > 0 ->
                        // Recursively check child
                        let prevChild = prev.OverlaidChildren.[0]

                        let newChild =
                            arrangedToRendered
                                keyToNode
                                focusableKeys
                                firstToFocusKey
                                initiallyFocusedKey
                                (Some prevChild)
                                arranged.Children.[0]
                                child1

                        if Object.referenceEquals newChild prevChild then
                            // Child unchanged, repopulate keyToNode and reuse parent
                            keyToNode.[key1] <- prev
                            Some prev
                        else
                            // Child changed, will create new parent below
                            None
                    | UnkeyedVdom.PanelSplit (dir1, behav1, child1a, child1b),
                      UnkeyedVdom.PanelSplit (dir2, behav2, _, _) when
                        dir1 = dir2 && behav1 = behav2 && prev.OverlaidChildren.Length >= 2
                        ->
                        // Recursively check both children
                        let prevChild1 = prev.OverlaidChildren.[0]
                        let prevChild2 = prev.OverlaidChildren.[1]

                        let newChild1 =
                            arrangedToRendered
                                keyToNode
                                focusableKeys
                                firstToFocusKey
                                initiallyFocusedKey
                                (Some prevChild1)
                                arranged.Children.[0]
                                child1a

                        let newChild2 =
                            arrangedToRendered
                                keyToNode
                                focusableKeys
                                firstToFocusKey
                                initiallyFocusedKey
                                (Some prevChild2)
                                arranged.Children.[1]
                                child1b

                        if
                            Object.referenceEquals newChild1 prevChild1
                            && Object.referenceEquals newChild2 prevChild2
                        then
                            // Both children unchanged, repopulate keyToNode and reuse parent
                            keyToNode.[key1] <- prev
                            Some prev
                        else
                            // Child changed, will create new parent below
                            None
                    | UnkeyedVdom.Focusable _, UnkeyedVdom.Focusable _ ->
                        // Focusable nodes have complex focus registration logic
                        // Skip early cutoff and use normal path to avoid issues
                        None
                    | UnkeyedVdom.Tag _, UnkeyedVdom.Tag _ when prev.OverlaidChildren.Length > 0 ->
                        // Tag is a transparent container - check if the child changed
                        let prevChild = prev.OverlaidChildren.[0]

                        let newChild =
                            arrangedToRendered
                                keyToNode
                                focusableKeys
                                firstToFocusKey
                                initiallyFocusedKey
                                (Some prevChild)
                                arranged.Children.[0]
                                arranged.Children.[0].VDomSource

                        if Object.referenceEquals newChild prevChild then
                            // Child unchanged, repopulate keyToNode and reuse parent
                            keyToNode.[key1] <- prev
                            Some prev
                        else
                            // Child changed, will create new parent below
                            None
                    | UnkeyedVdom.FlexibleContent _, UnkeyedVdom.FlexibleContent _ when prev.OverlaidChildren.Length > 0 ->
                        // FlexibleContent is a transparent container - check if the child changed
                        let prevChild = prev.OverlaidChildren.[0]

                        let newChild =
                            arrangedToRendered
                                keyToNode
                                focusableKeys
                                firstToFocusKey
                                initiallyFocusedKey
                                (Some prevChild)
                                arranged.Children.[0]
                                arranged.Children.[0].VDomSource

                        if Object.referenceEquals newChild prevChild then
                            // Child unchanged, repopulate keyToNode and reuse parent
                            keyToNode.[key1] <- prev
                            Some prev
                        else
                            // Child changed, will create new parent below
                            None
                    | _ -> None
                // Unkeyed leaf nodes
                | Vdom.Unkeyed (UnkeyedVdom.TextContent (text1, style1, align1, focus1)),
                  Vdom.Unkeyed (UnkeyedVdom.TextContent (text2, style2, align2, focus2)) when
                    text1 = text2 && style1 = style2 && align1 = align2 && focus1 = focus2
                    ->
                    Some prev
                | Vdom.Unkeyed UnkeyedVdom.Empty, Vdom.Unkeyed UnkeyedVdom.Empty ->
                    // Empty nodes are always equal
                    Some prev
                // Container nodes need recursive checks
                | Vdom.Unkeyed (UnkeyedVdom.Bordered child1), Vdom.Unkeyed (UnkeyedVdom.Bordered _) when
                    prev.OverlaidChildren.Length > 0
                    ->
                    // Recursively check child
                    let prevChild = prev.OverlaidChildren.[0]

                    let newChild =
                        arrangedToRendered
                            keyToNode
                            focusableKeys
                            firstToFocusKey
                            initiallyFocusedKey
                            (Some prevChild)
                            arranged.Children.[0]
                            child1

                    if Object.referenceEquals newChild prevChild then
                        // Child unchanged, reuse parent
                        Some prev
                    else
                        // Child changed, will create new parent below
                        None
                | Vdom.Unkeyed (UnkeyedVdom.PanelSplit (dir1, behav1, child1a, child1b)),
                  Vdom.Unkeyed (UnkeyedVdom.PanelSplit (dir2, behav2, _, _)) when
                    dir1 = dir2 && behav1 = behav2 && prev.OverlaidChildren.Length >= 2
                    ->
                    // Recursively check both children
                    let prevChild1 = prev.OverlaidChildren.[0]
                    let prevChild2 = prev.OverlaidChildren.[1]

                    let newChild1 =
                        arrangedToRendered
                            keyToNode
                            focusableKeys
                            firstToFocusKey
                            initiallyFocusedKey
                            (Some prevChild1)
                            arranged.Children.[0]
                            child1a

                    let newChild2 =
                        arrangedToRendered
                            keyToNode
                            focusableKeys
                            firstToFocusKey
                            initiallyFocusedKey
                            (Some prevChild2)
                            arranged.Children.[1]
                            child1b

                    if
                        Object.referenceEquals newChild1 prevChild1
                        && Object.referenceEquals newChild2 prevChild2
                    then
                        // Both children unchanged, reuse parent
                        Some prev
                    else
                        // At least one child changed, will create new parent below
                        None
                | Vdom.Unkeyed (UnkeyedVdom.Focusable _), Vdom.Unkeyed (UnkeyedVdom.Focusable _) ->
                    // Focusable nodes have complex focus registration logic
                    // Skip early cutoff and use normal path to avoid issues
                    None
                | Vdom.Unkeyed (UnkeyedVdom.Tag _), Vdom.Unkeyed (UnkeyedVdom.Tag _) when
                    prev.OverlaidChildren.Length > 0
                    ->
                    // Tag is a transparent container - check if the child changed
                    let prevChild = prev.OverlaidChildren.[0]

                    let newChild =
                        arrangedToRendered
                            keyToNode
                            focusableKeys
                            firstToFocusKey
                            initiallyFocusedKey
                            (Some prevChild)
                            arranged.Children.[0]
                            arranged.Children.[0].VDomSource

                    if Object.referenceEquals newChild prevChild then
                        // Child unchanged, reuse parent
                        Some prev
                    else
                        // Child changed, will create new parent below
                        None
                | Vdom.Unkeyed (UnkeyedVdom.FlexibleContent _), Vdom.Unkeyed (UnkeyedVdom.FlexibleContent _) when
                    prev.OverlaidChildren.Length > 0
                    ->
                    // FlexibleContent is a transparent container - check if the child changed
                    let prevChild = prev.OverlaidChildren.[0]

                    let newChild =
                        arrangedToRendered
                            keyToNode
                            focusableKeys
                            firstToFocusKey
                            initiallyFocusedKey
                            (Some prevChild)
                            arranged.Children.[0]
                            arranged.Children.[0].VDomSource

                    if Object.referenceEquals newChild prevChild then
                        // Child unchanged, reuse parent
                        Some prev
                    else
                        // Child changed, will create new parent below
                        None
                | _ -> None
            | _ -> None

        match earlyCutoffResult with
        | Some reused -> reused
        | None ->

        let children =
            match originalVdom with
            | Vdom.Keyed (KeyedVdom (_, unkeyedVdom)) ->
                match unkeyedVdom with
                | UnkeyedVdom.Bordered child ->
                    let prevChild =
                        match previousRender with
                        | Some prev when prev.OverlaidChildren.Length > 0 -> Some prev.OverlaidChildren.[0]
                        | _ -> None

                    [
                        arrangedToRendered
                            keyToNode
                            focusableKeys
                            firstToFocusKey
                            initiallyFocusedKey
                            prevChild
                            arranged.Children.[0]
                            child
                    ]
                | UnkeyedVdom.PanelSplit (_, _, child1, child2) ->
                    let prevChild1, prevChild2 =
                        match previousRender with
                        | Some prev when prev.OverlaidChildren.Length >= 2 ->
                            Some prev.OverlaidChildren.[0], Some prev.OverlaidChildren.[1]
                        | _ -> None, None

                    [
                        arrangedToRendered
                            keyToNode
                            focusableKeys
                            firstToFocusKey
                            initiallyFocusedKey
                            prevChild1
                            arranged.Children.[0]
                            child1
                        arrangedToRendered
                            keyToNode
                            focusableKeys
                            firstToFocusKey
                            initiallyFocusedKey
                            prevChild2
                            arranged.Children.[1]
                            child2
                    ]
                | UnkeyedVdom.Focusable (isFirstToFocus, isInitiallyFocused, KeyedVdom (key, childVdom)) ->
                    // Try to add the key; if it's already there (from early cutoff), ignore
                    let _ = focusableKeys.Add key

                    if isFirstToFocus && firstToFocusKey.Value.IsNone then
                        firstToFocusKey.Value <- Some key

                    if isInitiallyFocused && initiallyFocusedKey.Value.IsNone then
                        initiallyFocusedKey.Value <- Some key

                    let prevChild =
                        match previousRender with
                        | Some prev when prev.OverlaidChildren.Length > 0 -> Some prev.OverlaidChildren.[0]
                        | _ -> None

                    [
                        arrangedToRendered
                            keyToNode
                            focusableKeys
                            firstToFocusKey
                            initiallyFocusedKey
                            prevChild
                            arranged.Children.[0]
                            (Vdom.Keyed (KeyedVdom (key, childVdom)))
                    ]
                | UnkeyedVdom.Tag (_, _) ->
                    // Tag is a transparent container - render the single child
                    let prevChild =
                        match previousRender with
                        | Some prev when prev.OverlaidChildren.Length > 0 -> Some prev.OverlaidChildren.[0]
                        | _ -> None

                    [
                        arrangedToRendered
                            keyToNode
                            focusableKeys
                            firstToFocusKey
                            initiallyFocusedKey
                            prevChild
                            arranged.Children.[0]
                            arranged.Children.[0].VDomSource
                    ]
                | UnkeyedVdom.FlexibleContent _ ->
                    // FlexibleContent is a transparent container - render the single child
                    let prevChild =
                        match previousRender with
                        | Some prev when prev.OverlaidChildren.Length > 0 -> Some prev.OverlaidChildren.[0]
                        | _ -> None

                    [
                        arrangedToRendered
                            keyToNode
                            focusableKeys
                            firstToFocusKey
                            initiallyFocusedKey
                            prevChild
                            arranged.Children.[0]
                            arranged.Children.[0].VDomSource
                    ]
                | UnkeyedVdom.TextContent _
                | UnkeyedVdom.Empty -> []
            | Vdom.Unkeyed unkeyedVdom ->
                match unkeyedVdom with
                | UnkeyedVdom.Bordered child ->
                    let prevChild =
                        match previousRender with
                        | Some prev when prev.OverlaidChildren.Length > 0 -> Some prev.OverlaidChildren.[0]
                        | _ -> None

                    [
                        arrangedToRendered
                            keyToNode
                            focusableKeys
                            firstToFocusKey
                            initiallyFocusedKey
                            prevChild
                            arranged.Children.[0]
                            child
                    ]
                | UnkeyedVdom.PanelSplit (_, _, child1, child2) ->
                    let prevChild1, prevChild2 =
                        match previousRender with
                        | Some prev when prev.OverlaidChildren.Length >= 2 ->
                            Some prev.OverlaidChildren.[0], Some prev.OverlaidChildren.[1]
                        | _ -> None, None

                    [
                        arrangedToRendered
                            keyToNode
                            focusableKeys
                            firstToFocusKey
                            initiallyFocusedKey
                            prevChild1
                            arranged.Children.[0]
                            child1
                        arrangedToRendered
                            keyToNode
                            focusableKeys
                            firstToFocusKey
                            initiallyFocusedKey
                            prevChild2
                            arranged.Children.[1]
                            child2
                    ]
                | UnkeyedVdom.Focusable (isFirstToFocus, isInitiallyFocused, KeyedVdom (key, childVdom)) ->
                    // Try to add the key; if it's already there (from early cutoff), ignore
                    let _ = focusableKeys.Add key

                    if isFirstToFocus && firstToFocusKey.Value.IsNone then
                        firstToFocusKey.Value <- Some key

                    if isInitiallyFocused && initiallyFocusedKey.Value.IsNone then
                        initiallyFocusedKey.Value <- Some key

                    let prevChild =
                        match previousRender with
                        | Some prev when prev.OverlaidChildren.Length > 0 -> Some prev.OverlaidChildren.[0]
                        | _ -> None

                    [
                        arrangedToRendered
                            keyToNode
                            focusableKeys
                            firstToFocusKey
                            initiallyFocusedKey
                            prevChild
                            arranged.Children.[0]
                            (Vdom.Keyed (KeyedVdom (key, childVdom)))
                    ]
                | UnkeyedVdom.Tag (_, _) ->
                    // Tag is a transparent container - render the single child
                    let prevChild =
                        match previousRender with
                        | Some prev when prev.OverlaidChildren.Length > 0 -> Some prev.OverlaidChildren.[0]
                        | _ -> None

                    [
                        arrangedToRendered
                            keyToNode
                            focusableKeys
                            firstToFocusKey
                            initiallyFocusedKey
                            prevChild
                            arranged.Children.[0]
                            arranged.Children.[0].VDomSource
                    ]
                | UnkeyedVdom.FlexibleContent _ ->
                    // FlexibleContent is a transparent container - render the single child
                    let prevChild =
                        match previousRender with
                        | Some prev when prev.OverlaidChildren.Length > 0 -> Some prev.OverlaidChildren.[0]
                        | _ -> None

                    [
                        arrangedToRendered
                            keyToNode
                            focusableKeys
                            firstToFocusKey
                            initiallyFocusedKey
                            prevChild
                            arranged.Children.[0]
                            arranged.Children.[0].VDomSource
                    ]
                | UnkeyedVdom.TextContent _
                | UnkeyedVdom.Empty -> []

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
        | Vdom.Keyed (KeyedVdom (key, unkeyedVdom)) ->
            match unkeyedVdom with
            | UnkeyedVdom.Focusable (_, _, KeyedVdom (childKey, _)) ->
                // For focusable wrappers, don't register the wrapper key.
                // The child's key is already registered by the recursive call.
                ()
            | _ ->
                // For all other keyed nodes (including Tag wrappers), register the key to this node
                keyToNode.[key] <- result
        | Vdom.Unkeyed (UnkeyedVdom.Focusable (_, _, KeyedVdom (key, _))) ->
            // Register the focusable child's key
            keyToNode.[key] <- children.[0]
        | _ -> ()

        result

    let rec private dump (writer : IO.TextWriter) (indent : int) (node : Layout.ArrangedNode) =
        let prefix = String.replicate indent "  "

        fprintf
            writer
            "%sBounds={X=%d Y=%d W=%d H=%d} "
            prefix
            node.Bounds.TopLeftX
            node.Bounds.TopLeftY
            node.Bounds.Width
            node.Bounds.Height

        match node.Vdom with
        | Vdom.Unkeyed (UnkeyedVdom.TextContent _) -> fprintf writer "TextContent"
        | Vdom.Unkeyed UnkeyedVdom.Empty -> fprintf writer "Empty"
        | Vdom.Unkeyed (UnkeyedVdom.Bordered _) -> fprintf writer "Bordered"
        | Vdom.Unkeyed (UnkeyedVdom.PanelSplit (direction, behaviour, _, _)) ->
            let dirStr =
                if direction = SplitDirection.Vertical then
                    "Vertical"
                else
                    "Horizontal"

            let behavStr =
                match behaviour with
                | SplitBehaviour.Proportion p -> $"Proportion({p})"
                | SplitBehaviour.Absolute n -> $"Absolute({n})"
                | SplitBehaviour.Auto -> "Auto"

            fprintf writer $"PanelSplit(%s{dirStr}, %s{behavStr})"
        | Vdom.Unkeyed (UnkeyedVdom.Focusable (isFirstToFocus, isInitiallyFocused, _)) ->
            fprintf writer $"Focusable(isFirstToFocus=%b{isFirstToFocus}, isInitiallyFocused=%b{isInitiallyFocused})"
        | Vdom.Unkeyed (UnkeyedVdom.Tag (tag, _)) -> fprintf writer $"Tag(\"%s{tag}\")"
        | Vdom.Unkeyed (UnkeyedVdom.FlexibleContent _) -> fprintf writer "FlexibleContent"
        | Vdom.Keyed _ -> fprintf writer "Keyed"

        fprintfn writer ""

        for child in node.Children do
            dump writer (indent + 1) child

    /// Top-level layout function that uses the two-pass system
    let internal layout
        (keyToNode : Dictionary<NodeKey, RenderedNode>)
        (focusableKeys : OrderedSet<NodeKey>)
        (firstToFocusKey : NodeKey option ref)
        (initiallyFocusedKey : NodeKey option ref)
        (previousRender : RenderedNode option)
        (bounds : Rectangle)
        (vdom : Vdom<DesiredBounds>)
        (debugWriter : IO.StreamWriter option)
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

            // Debug: dump layout to file if enabled
            match debugWriter with
            | Some writer ->
                fprintfn writer "=== Frame %s ===" (DateTime.Now.ToString "HH:mm:ss.fff")
                dump writer 0 arranged
                fprintfn writer ""
            | None -> ()

            // Convert to RenderedNode
            match vdom with
            | Vdom.Unkeyed unkeyedVdom ->
                arrangedToRendered
                    keyToNode
                    focusableKeys
                    firstToFocusKey
                    initiallyFocusedKey
                    previousRender
                    arranged
                    (Vdom.Unkeyed unkeyedVdom)
            | Vdom.Keyed _ -> failwith "Top-level vdom must be unkeyed"

    /// Clear a rectangular region by filling it with spaces
    let private clearBoundsWithSpaces (dirty : TerminalCell voption[,]) (bounds : Rectangle) : unit =
        for y = 0 to bounds.Height - 1 do
            for x = 0 to bounds.Width - 1 do
                setAtRelativeOffset dirty bounds x y (ValueSome (TerminalCell.OfChar ' '))

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

        // Extract the UnkeyedVdomNode from either Keyed or Unkeyed wrapper
        let unkeyedVdom =
            match node.VDomSource with
            | Vdom.Keyed (KeyedVdom (_, vdom)) -> vdom
            | Vdom.Unkeyed vdom -> vdom

        // Helper to check if previous node matches the current node type for container optimization
        let previousMatchesContainer nodeType =
            match previousNode with
            | Some prev when prev.Bounds = bounds ->
                match prev.VDomSource with
                | Vdom.Keyed (KeyedVdom (_, prevVdom))
                | Vdom.Unkeyed prevVdom ->
                    match prevVdom, nodeType with
                    | UnkeyedVdom.PanelSplit _, UnkeyedVdom.PanelSplit _ -> true
                    | UnkeyedVdom.Bordered _, UnkeyedVdom.Bordered _ -> true
                    | _ -> false
            | _ -> false

        match unkeyedVdom with
        | UnkeyedVdom.PanelSplit _ ->
            // Only paint background if this is a new node or bounds changed
            let previousNode =
                if previousMatchesContainer unkeyedVdom then
                    previousNode
                else
                    // Container changed: clear and invalidate children
                    clearBoundsWithSpaces dirty bounds
                    None

            // Extract child previous state (None if we cleared)
            let prevChild1, prevChild2 =
                match previousNode with
                | Some prev when prev.OverlaidChildren.Length >= 2 ->
                    Some prev.OverlaidChildren.[0], Some prev.OverlaidChildren.[1]
                | _ -> None, None

            renderToBuffer dirty prevChild1 node.OverlaidChildren.[0]
            renderToBuffer dirty prevChild2 node.OverlaidChildren.[1]

        | UnkeyedVdom.Bordered _ ->
            // Only paint background and border if this is a new node or bounds changed
            let previousNode =
                if previousMatchesContainer unkeyedVdom then
                    previousNode
                else
                    // New container or bounds changed, paint background and border
                    clearBoundsWithSpaces dirty bounds

                    // Only draw border if bounds are large enough (need at least 2x2)
                    if bounds.Width >= 2 && bounds.Height >= 2 then
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

                    None // Invalidate children

            // Extract child previous state (None if we cleared)
            let prevChild =
                match previousNode with
                | Some prev when prev.OverlaidChildren.Length > 0 -> Some prev.OverlaidChildren.[0]
                | _ -> None

            renderToBuffer dirty prevChild node.OverlaidChildren.[0]

        | UnkeyedVdom.Tag (_, _) ->
            // Tag is a transparent container - just render the child
            let prevChild =
                match previousNode with
                | Some prev when prev.OverlaidChildren.Length > 0 -> Some prev.OverlaidChildren.[0]
                | _ -> None

            renderToBuffer dirty prevChild node.OverlaidChildren.[0]

        | UnkeyedVdom.FlexibleContent _ ->
            // FlexibleContent is a transparent container - just render the child
            let prevChild =
                match previousNode with
                | Some prev when prev.OverlaidChildren.Length > 0 -> Some prev.OverlaidChildren.[0]
                | _ -> None

            renderToBuffer dirty prevChild node.OverlaidChildren.[0]

        | UnkeyedVdom.Focusable _ ->
            // Focusable just wraps its child, render the child
            let prevChild =
                match previousNode with
                | Some prev when prev.OverlaidChildren.Length > 0 -> Some prev.OverlaidChildren.[0]
                | _ -> None

            renderToBuffer dirty prevChild node.OverlaidChildren.[0]

        | UnkeyedVdom.TextContent (content, style, alignment, focus) ->
            // TODO: can do better here if we can compute a more efficient diff
            // TODO: work out how to display this differently when it has focus
            clearBoundsWithSpaces dirty bounds

            // Only render text if we have space (width and height both > 0)
            if bounds.Width > 0 && bounds.Height > 0 then
                match alignment with
                | ContentAlignment.Centered ->
                    // Center the text horizontally and vertically within bounds
                    // Normalize line endings: CRLF -> LF, lone CR -> LF
                    let content = content.Replace("\r\n", "\n").Replace ("\r", "\n")
                    let lines = content.Split '\n'
                    let lineCount = lines.Length
                    // Vertically center the block of lines
                    // Formula: for block center to be at height/2, startY = (height - lineCount + 1) / 2
                    let startY = max 0 ((bounds.Height - lineCount + 1) / 2)

                    for lineIndex = 0 to lines.Length - 1 do
                        let line = lines.[lineIndex]
                        let y = startY + lineIndex

                        if y < bounds.Height then
                            let startX = max 0 ((bounds.Width - line.Length) / 2)
                            let mutable x = startX

                            for ch in line do
                                if x < bounds.Width then
                                    let cell =
                                        {
                                            Char = ch
                                            BackgroundColor = style.Background
                                            TextColor = style.Foreground
                                        }

                                    setAtRelativeOffset dirty bounds x y (ValueSome cell)
                                    x <- x + 1
                | ContentAlignment.TopLeft ->
                    // Render from top-left, wrapping to next line
                    // Normalize line endings: CRLF -> LF, lone CR -> LF
                    let content = content.Replace("\r\n", "\n").Replace ("\r", "\n")
                    let mutable index = 0
                    let mutable currX = 0
                    let mutable currY = 0

                    while index < content.Length do
                        let ch = content.Chars index

                        if ch = '\n' then
                            // Newline: move to start of next line without rendering
                            currX <- 0
                            currY <- currY + 1

                            if currY >= bounds.Height then
                                index <- content.Length
                        else
                            let cell =
                                {
                                    Char = ch
                                    BackgroundColor = style.Background
                                    TextColor = style.Foreground
                                }

                            setAtRelativeOffset dirty bounds currX currY (ValueSome cell)

                            currX <- currX + 1

                            if currX = bounds.Width then
                                currX <- 0
                                currY <- currY + 1

                                if currY >= bounds.Height then
                                    index <- content.Length

                        index <- index + 1

        | UnkeyedVdom.Empty ->
            // Empty nodes render nothing, but need to clear if replacing a previous node
            match previousNode with
            | Some _ -> clearBoundsWithSpaces dirty bounds
            | None -> ()

    let writeBuffer (dirty : TerminalCell voption[,]) : TerminalOp seq =
        // TODO this is super dumb
        seq {
            for y = 0 to dirty.GetLength 0 - 1 do
                for x = 0 to dirty.GetLength 1 - 1 do
                    match dirty.[y, x] with
                    | ValueNone -> ()
                    | ValueSome cell -> yield! [ TerminalOp.MoveCursor (x, y) ; TerminalOp.WriteChar cell ]
        }

    let oneStep<'state> (renderState : RenderState) (userState : 'state) (compute : 'state -> Vdom<DesiredBounds>) =
        do
            let bounds = VdomContext.terminalBounds renderState.VdomContext
            let terminalHeight = bounds.Height
            let terminalWidth = bounds.Width

            if
                renderState.Buffer.GetLength 0 <> terminalHeight
                || renderState.Buffer.GetLength 1 <> terminalWidth
            then
                renderState.Buffer <- Array2D.zeroCreate terminalHeight terminalWidth
            else
                Array.Clear renderState.Buffer

        renderState.KeyToNode.Clear ()
        renderState.FocusableKeys.Clear ()
        renderState.FirstToFocusKey.Value <- None
        renderState.InitiallyFocusedKey.Value <- None

        let vdom = compute userState

        // Phase 1: Compute layout (bounds only)
        let layoutResult =
            layout
                renderState.KeyToNode
                renderState.FocusableKeys
                renderState.FirstToFocusKey
                renderState.InitiallyFocusedKey
                renderState.PreviousVdom
                (VdomContext.terminalBounds renderState.VdomContext)
                vdom
                renderState.DebugWriter

        // If the focused element from the previous tick no longer exists, clear focused state
        match VdomContext.focusedKey renderState.VdomContext with
        | None -> ()
        | Some key ->
            if not (renderState.FocusableKeys.Contains key) then
                VdomContext.setFocusedKey None renderState.VdomContext

        // Set initial focus to the isInitiallyFocused element if nothing is currently focused
        let needsRecompute =
            match VdomContext.focusedKey renderState.VdomContext with
            | None ->
                match renderState.InitiallyFocusedKey.Value with
                | Some initialKey when renderState.FocusableKeys.Contains initialKey ->
                    VdomContext.setFocusedKey (Some initialKey) renderState.VdomContext
                    true // Need to recompute vdom with the new focus state
                | _ -> false
            | Some _ -> false

        // Recompute vdom if we just set initial focus
        let layoutResult =
            if needsRecompute then
                // Clear and recompute to get correct focus rendering
                renderState.KeyToNode.Clear ()
                renderState.FocusableKeys.Clear ()
                renderState.FirstToFocusKey.Value <- None
                renderState.InitiallyFocusedKey.Value <- None

                let vdom = compute userState

                layout
                    renderState.KeyToNode
                    renderState.FocusableKeys
                    renderState.FirstToFocusKey
                    renderState.InitiallyFocusedKey
                    renderState.PreviousVdom
                    (VdomContext.terminalBounds renderState.VdomContext)
                    vdom
                    renderState.DebugWriter
            else
                layoutResult

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
