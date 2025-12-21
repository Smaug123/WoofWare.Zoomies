namespace WoofWare.Zoomies

open System
open System.Text

[<RequireQualifiedAccess>]
module Render =
    let inline private yIndex (bounds : Rectangle) (relativeY : int) = bounds.TopLeftY + relativeY
    let inline private xIndex (bounds : Rectangle) (relativeX : int) = bounds.TopLeftX + relativeX

    let private setAtRelativeOffset (arr : 'a[,]) (bounds : Rectangle) (relativeX : int) (relativeY : int) (v : 'a) =
        arr.[yIndex bounds relativeY, xIndex bounds relativeX] <- v

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
        | UnkeyedVdom.PanelSplit (direction, _, _, _) ->
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

            let child1 = node.OverlaidChildren.[0]
            let child2 = node.OverlaidChildren.[1]

            renderToBuffer dirty prevChild1 child1
            renderToBuffer dirty prevChild2 child2

            // Clear any unallocated space in the container that isn't covered by children
            // This handles the case where max height/width constraints leave unused space
            match direction with
            | SplitDirection.Horizontal ->
                // Children are stacked vertically (first=top, second=bottom)
                // Unallocated space is after child2
                let usedHeight = child1.Bounds.Height + child2.Bounds.Height

                if usedHeight < bounds.Height then
                    let unallocatedBounds =
                        {
                            TopLeftX = bounds.TopLeftX
                            TopLeftY = bounds.TopLeftY + usedHeight
                            Width = bounds.Width
                            Height = bounds.Height - usedHeight
                        }

                    clearBoundsWithSpaces dirty unallocatedBounds
            | SplitDirection.Vertical ->
                // Children are side-by-side (first=left, second=right)
                // Unallocated space is after child2
                let usedWidth = child1.Bounds.Width + child2.Bounds.Width

                if usedWidth < bounds.Width then
                    let unallocatedBounds =
                        {
                            TopLeftX = bounds.TopLeftX + usedWidth
                            TopLeftY = bounds.TopLeftY
                            Width = bounds.Width - usedWidth
                            Height = bounds.Height
                        }

                    clearBoundsWithSpaces dirty unallocatedBounds

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

        | UnkeyedVdom.Tag _ ->
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

        | UnkeyedVdom.TextContent (content, style, alignment, focus, wrap) ->
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
                    let inputLines = content.Split '\n'

                    // Process lines based on wrap setting
                    let lines =
                        if wrap && bounds.Width > 0 then
                            // Wrap long lines by breaking them into chunks of bounds.Width
                            inputLines
                            |> Array.collect (fun line ->
                                if line.Length <= bounds.Width then
                                    [| line |]
                                else
                                    let chunks = ResizeArray ()

                                    let mutable i = 0

                                    while i < line.Length do
                                        let chunkLen = min bounds.Width (line.Length - i)
                                        chunks.Add (line.Substring (i, chunkLen))
                                        i <- i + bounds.Width

                                    chunks.ToArray ()
                            )
                        else
                            inputLines

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
                    // Render from top-left
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
                                if wrap then
                                    // Wrap to next line
                                    currX <- 0
                                    currY <- currY + 1

                                    if currY >= bounds.Height then
                                        index <- content.Length
                                else
                                    // Truncate: skip remaining characters until newline
                                    while index + 1 < content.Length && content.Chars (index + 1) <> '\n' do
                                        index <- index + 1

                        index <- index + 1

        | UnkeyedVdom.StyledSpans (spans, alignment, _focus, wrap) ->
            // Similar to TextContent but with per-span styling
            clearBoundsWithSpaces dirty bounds

            // Only render text if we have space (width and height both > 0)
            if bounds.Width > 0 && bounds.Height > 0 then
                // Concatenate all spans into a single string for layout calculations
                let content = spans |> List.map (fun s -> s.Text) |> String.concat ""
                // Build a style lookup: for each character index, which style applies
                let styleAtIndex =
                    let arr = ResizeArray<CellStyle> ()

                    for span in spans do
                        for _ in span.Text do
                            arr.Add span.Style

                    arr

                match alignment with
                | ContentAlignment.Centered ->
                    // Center the text horizontally and vertically within bounds
                    // Normalize line endings: CRLF -> LF, lone CR -> LF
                    let content = content.Replace("\r\n", "\n").Replace ("\r", "\n")
                    let inputLines = content.Split '\n'

                    // Process lines based on wrap setting
                    // Also track character index ranges for each wrapped line
                    let linesWithOffsets =
                        if wrap && bounds.Width > 0 then
                            let result = ResizeArray<string * int> ()
                            let mutable globalOffset = 0

                            for line in inputLines do
                                if line.Length <= bounds.Width then
                                    result.Add (line, globalOffset)
                                    globalOffset <- globalOffset + line.Length
                                else
                                    let mutable i = 0

                                    while i < line.Length do
                                        let chunkLen = min bounds.Width (line.Length - i)
                                        result.Add (line.Substring (i, chunkLen), globalOffset + i)
                                        i <- i + bounds.Width

                                    globalOffset <- globalOffset + line.Length

                                globalOffset <- globalOffset + 1 // account for newline

                            result.ToArray ()
                        else
                            let mutable offset = 0
                            let result = ResizeArray<string * int> ()

                            for line in inputLines do
                                result.Add (line, offset)
                                offset <- offset + line.Length + 1 // +1 for newline

                            result.ToArray ()

                    let lineCount = linesWithOffsets.Length
                    // Vertically center the block of lines
                    let startY = max 0 ((bounds.Height - lineCount + 1) / 2)

                    for lineIndex = 0 to linesWithOffsets.Length - 1 do
                        let (line, lineStartOffset) = linesWithOffsets.[lineIndex]
                        let y = startY + lineIndex

                        if y < bounds.Height then
                            let startX = max 0 ((bounds.Width - line.Length) / 2)
                            let mutable x = startX

                            for charIdx = 0 to line.Length - 1 do
                                if x < bounds.Width then
                                    let ch = line.[charIdx]
                                    let globalCharIdx = lineStartOffset + charIdx

                                    let style =
                                        if globalCharIdx < styleAtIndex.Count then
                                            styleAtIndex.[globalCharIdx]
                                        else
                                            CellStyle.none

                                    let cell =
                                        {
                                            Char = ch
                                            BackgroundColor = style.Background
                                            TextColor = style.Foreground
                                        }

                                    setAtRelativeOffset dirty bounds x y (ValueSome cell)
                                    x <- x + 1

                | ContentAlignment.TopLeft ->
                    // Render from top-left
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
                            let style =
                                if index < styleAtIndex.Count then
                                    styleAtIndex.[index]
                                else
                                    CellStyle.none

                            let cell =
                                {
                                    Char = ch
                                    BackgroundColor = style.Background
                                    TextColor = style.Foreground
                                }

                            setAtRelativeOffset dirty bounds currX currY (ValueSome cell)

                            currX <- currX + 1

                            if currX = bounds.Width then
                                if wrap then
                                    // Wrap to next line
                                    currX <- 0
                                    currY <- currY + 1

                                    if currY >= bounds.Height then
                                        index <- content.Length
                                else
                                    // Truncate: skip remaining characters until newline
                                    while index + 1 < content.Length && content.Chars (index + 1) <> '\n' do
                                        index <- index + 1

                        index <- index + 1

        | UnkeyedVdom.Empty ->
            // Empty nodes render nothing, but need to clear if replacing a previous node
            match previousNode with
            | Some _ -> clearBoundsWithSpaces dirty bounds
            | None -> ()

    let writeBuffer (dirty : TerminalCell voption[,]) : TerminalOp seq =
        seq {
            // Track cursor position to avoid redundant MoveCursor operations.
            // After writing characters, the cursor automatically advances right.
            let mutable cursorX = -1
            let mutable cursorY = -1
            let width = dirty.GetLength 1
            let height = dirty.GetLength 0

            for y = 0 to height - 1 do
                let mutable x = 0

                while x < width do
                    match dirty.[y, x] with
                    | ValueNone -> x <- x + 1
                    | ValueSome startCell ->
                        // Found a cell to write - start collecting a run of consecutive
                        // cells with the same foreground and background colors
                        if cursorX <> x || cursorY <> y then
                            yield TerminalOp.MoveCursor (x, y)

                        let bg = startCell.BackgroundColor
                        let fg = startCell.TextColor
                        let runChars = StringBuilder ()
                        runChars.Append startCell.Char |> ignore
                        x <- x + 1

                        // Keep adding consecutive cells with matching style
                        let mutable continueRun = true

                        while continueRun && x < width do
                            match dirty.[y, x] with
                            | ValueSome nextCell when nextCell.BackgroundColor = bg && nextCell.TextColor = fg ->
                                runChars.Append nextCell.Char |> ignore
                                x <- x + 1
                            | _ -> continueRun <- false

                        yield TerminalOp.WriteRun (runChars.ToString (), bg, fg)

                        // After writing, cursor advances right. But at row end, terminal behavior
                        // varies (may wrap, stay put, etc.), so mark position as unknown.
                        if x < width then
                            cursorX <- x
                            cursorY <- y
                        else
                            cursorX <- -1
                            cursorY <- -1
        }

    /// Render to the output buffer without flushing to the terminal.
    /// Call `flush` afterwards to actually paint the result to the screen.
    let oneStepNoFlush<'state, 'event>
        (renderState : RenderState<'event>)
        (userState : 'state)
        (compute : 'state -> Vdom<DesiredBounds>)
        =
        let buffer = RenderState.buffer renderState
        let vdomContext = RenderState.vdomContext renderState

        do
            let bounds = VdomContext.terminalBounds vdomContext
            let terminalHeight = bounds.Height
            let terminalWidth = bounds.Width

            if buffer.GetLength 0 <> terminalHeight || buffer.GetLength 1 <> terminalWidth then
                RenderState.setBuffer (Array2D.zeroCreate terminalHeight terminalWidth) renderState
            else
                Array.Clear buffer

        let keyToNode = RenderState.keyToNode renderState
        let focusableKeys = RenderState.focusableKeys renderState
        let firstToFocusKey = RenderState.firstToFocusKey renderState
        let initiallyFocusedKey = RenderState.initiallyFocusedKey renderState

        keyToNode.Clear ()
        focusableKeys.Clear ()
        firstToFocusKey.Value <- None
        initiallyFocusedKey.Value <- None

        let vdom = compute userState

        // Phase 1: Compute layout (bounds only)
        let layoutResult =
            TreeReconciliation.layout
                keyToNode
                focusableKeys
                firstToFocusKey
                initiallyFocusedKey
                (RenderState.previousVdom renderState)
                (VdomContext.terminalBounds vdomContext)
                vdom
                (RenderState.debugWriter renderState)

        // If the focused element from the previous tick no longer exists, clear focused state
        match VdomContext.focusedKey vdomContext with
        | None -> ()
        | Some key ->
            if not (focusableKeys.Contains key) then
                VdomContext.setFocusedKey None vdomContext

        // Set initial focus to the isInitiallyFocused element if nothing is currently focused
        let needsRecompute =
            match VdomContext.focusedKey vdomContext with
            | None ->
                match initiallyFocusedKey.Value with
                | Some initialKey when focusableKeys.Contains initialKey ->
                    VdomContext.setFocusedKey (Some initialKey) vdomContext
                    true // Need to recompute vdom with the new focus state
                | _ -> false
            | Some _ -> false

        // Recompute vdom if we just set initial focus
        let layoutResult =
            if needsRecompute then
                // Clear and recompute to get correct focus rendering
                keyToNode.Clear ()
                focusableKeys.Clear ()
                firstToFocusKey.Value <- None
                initiallyFocusedKey.Value <- None

                let vdom = compute userState

                TreeReconciliation.layout
                    keyToNode
                    focusableKeys
                    firstToFocusKey
                    initiallyFocusedKey
                    (RenderState.previousVdom renderState)
                    (VdomContext.terminalBounds vdomContext)
                    vdom
                    (RenderState.debugWriter renderState)
            else
                layoutResult

        let previousVdom = RenderState.previousVdom renderState

        // Phase 2: Render to buffer (only if something changed)
        match previousVdom with
        | Some prev when Object.referenceEquals prev layoutResult ->
            // Nothing changed, skip rendering
            ()
        | _ ->
            // Something changed, render to buffer
            renderToBuffer (RenderState.buffer renderState) previousVdom layoutResult

        RenderState.setPreviousVdom (Some layoutResult) renderState

        let cursorFlip = RenderState.isCursorVisible renderState
        let mutable haveManipulatedCursor = false
        let mutable haveStartedSyncUpdate = false

        for op in writeBuffer (RenderState.buffer renderState) do
            if not haveStartedSyncUpdate then
                RenderState.output renderState TerminalOp.BeginSynchronizedUpdate
                haveStartedSyncUpdate <- true

            if not haveManipulatedCursor && cursorFlip then
                RenderState.setCursorInvisible renderState
                haveManipulatedCursor <- true

            RenderState.output renderState op

        if haveManipulatedCursor && cursorFlip then
            RenderState.setCursorVisible renderState

        if haveStartedSyncUpdate then
            RenderState.output renderState TerminalOp.EndSynchronizedUpdate

    /// Flush all buffered output to the console in a single write.
    let flush<'event> (renderState : RenderState<'event>) = RenderState.flush renderState

    /// Render and flush to the terminal in one step.
    let oneStep<'state, 'event>
        (renderState : RenderState<'event>)
        (userState : 'state)
        (compute : 'state -> Vdom<DesiredBounds>)
        =
        oneStepNoFlush renderState userState compute
        flush renderState
