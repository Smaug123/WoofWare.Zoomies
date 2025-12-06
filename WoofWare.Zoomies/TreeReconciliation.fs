namespace WoofWare.Zoomies

open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
module internal TreeReconciliation =

    /// Convert ArrangedNode to RenderedNode, populating keyToNode and focusableKeys
    let rec arrangedToRendered
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
                    | UnkeyedVdom.TextContent (text1, style1, align1, focus1, wrap1),
                      UnkeyedVdom.TextContent (text2, style2, align2, focus2, wrap2) when
                        text1 = text2
                        && style1 = style2
                        && align1 = align2
                        && focus1 = focus2
                        && wrap1 = wrap2
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
                | Vdom.Unkeyed (UnkeyedVdom.TextContent (text1, style1, align1, focus1, wrap1)),
                  Vdom.Unkeyed (UnkeyedVdom.TextContent (text2, style2, align2, focus2, wrap2)) when
                    text1 = text2
                    && style1 = style2
                    && align1 = align2
                    && focus1 = focus2
                    && wrap1 = wrap2
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
                | SplitBehaviour.Proportion p -> $"Proportion(%f{p})"
                | SplitBehaviour.Absolute n -> $"Absolute(%d{n})"
                | SplitBehaviour.AutoWeighted (w1, w2) ->
                    let weightStr w =
                        match w with
                        | ExpansionWeight.FromContent -> "FromContent"
                        | ExpansionWeight.Fixed f -> sprintf "Fixed %.2f" f

                    $"AutoWeighted (%s{weightStr w1}, %s{weightStr w2})"

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
    let layout
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
