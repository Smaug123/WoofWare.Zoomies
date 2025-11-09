namespace WoofWare.Zoomies

open System

/// Constraints provided by parent during measurement
type internal MeasureConstraints =
    {
        /// Maximum available width.
        /// Invariant: n >= 0
        MaxWidth : int
        /// Maximum available height.
        /// Invariant: n >= 0
        MaxHeight : int
    }

/// Size requirements reported by a node
/// Invariants (must hold for all valid MeasuredSize values):
/// - 0 <= MinWidth <= PreferredWidth
/// - If MaxWidth = Some m, then MinWidth <= PreferredWidth <= m
/// - CRITICAL CONSTRAINT: If MaxWidth = Some m, then MinWidth <= m
///   A node must not report a MinWidth that exceeds the constraint it was given.
///   This prevents impossible demands that would break layout algorithms.
/// - For any width w >= 0, MinHeightForWidth(w) >= 0
/// - For any width w >= 0, MinHeightForWidth(w) <= PreferredHeightForWidth(w)
/// - MinHeightForWidth(0) must return a sensible value (not divide-by-zero)
type internal MeasuredSize =
    {
        /// Minimum width needed to render without data loss.
        /// Must respect any MaxWidth constraint from measurement.
        /// NOTE: This is a strong preference but may be violated by the arrange
        /// pass if insufficient space is available (see "Soft Constraints" in design doc).
        MinWidth : int
        /// Preferred width if space is available.
        PreferredWidth : int
        /// Maximum useful width (None = unbounded growth acceptable)
        /// This is a hint: arrangement may allocate beyond this, but the component
        /// gains no additional utility from the extra space.
        MaxWidth : int option
        /// Minimum height needed given some width
        /// MUST handle width=0 gracefully (e.g., return height for single-char-per-line layout)
        MinHeightForWidth : int -> int
        /// Preferred height given some width
        /// MUST handle width=0 gracefully
        PreferredHeightForWidth : int -> int
        /// Maximum useful height given some width (None = unbounded)
        /// MUST handle width=0 gracefully
        MaxHeightForWidth : int -> int option
    }

[<RequireQualifiedAccess>]
module internal Layout =
    /// A node annotated with its measurement result
    [<NoEquality ; NoComparison>]
    type MeasuredNode<'bounds> =
        {
            Vdom : KeylessVdom<'bounds>
            Measured : MeasuredSize
            /// Measured children (for container nodes)
            Children : MeasuredNode<'bounds> list
        }

    /// Result of the arrange phase
    [<NoEquality ; NoComparison>]
    type ArrangedNode =
        {
            /// The VDOM node being arranged
            Vdom : KeylessVdom<Rectangle>
            /// Final allocated rectangle for this node
            Bounds : Rectangle
            /// Arranged children (for container nodes)
            Children : ArrangedNode list
        }

    /// Helper: count lines after word-wrapping
    let private wordWrapCount (text : string) (width : int) : int =
        if width <= 0 then
            max 1 text.Length
        else
            let words = text.Split ([| ' ' ; '\t' ; '\n' |], StringSplitOptions.None)

            if words.Length = 0 then
                1
            else
                let mutable lineCount = 1
                let mutable currentLineWidth = 0

                for word in words do
                    let wordLen = word.Length

                    if currentLineWidth = 0 then
                        // First word on line
                        currentLineWidth <- wordLen
                    elif currentLineWidth + 1 + wordLen <= width then
                        // Word fits on current line (with space separator)
                        currentLineWidth <- currentLineWidth + 1 + wordLen
                    else
                        // Word doesn't fit, start new line
                        lineCount <- lineCount + 1
                        currentLineWidth <- wordLen

                lineCount

    /// Measure a text content node
    let private measureText (text : string) (constraints : MeasureConstraints) : MeasuredSize =
        let longestWord =
            if String.IsNullOrEmpty text then
                1
            else
                text.Split [| ' ' ; '\t' ; '\n' |] |> Seq.map String.length |> Seq.fold max 1

        let fullLineWidth = max 1 text.Length

        // Respect MaxWidth constraint when reporting MinWidth
        let constrainedMinWidth = min longestWord constraints.MaxWidth

        // Clamp preferred width to constraint
        let constrainedPreferredWidth = min fullLineWidth constraints.MaxWidth

        {
            MinWidth = constrainedMinWidth
            PreferredWidth = constrainedPreferredWidth
            MaxWidth = None // Can grow arbitrarily wide
            MinHeightForWidth =
                fun w ->
                    // CRITICAL: Handle w=0 case
                    let safeWidth = max 1 w
                    let wrappedLines = wordWrapCount text safeWidth
                    max 1 wrappedLines
            PreferredHeightForWidth =
                fun w ->
                    let safeWidth = max 1 w
                    let wrappedLines = wordWrapCount text safeWidth
                    max 1 wrappedLines
            MaxHeightForWidth =
                fun w ->
                    let safeWidth = max 1 w
                    Some (wordWrapCount text safeWidth)
        }

    /// Measure a checkbox node
    let private measureCheckbox : MeasuredSize =
        {
            MinWidth = 3 // "[ ]" or "[X]"
            PreferredWidth = 3
            MaxWidth = Some 3 // Fixed size component
            MinHeightForWidth = fun _ -> 1
            PreferredHeightForWidth = fun _ -> 1
            MaxHeightForWidth = fun _ -> Some 1
        }

    /// Measure a bordered container
    let rec private measureBordered
        (child : KeylessVdom<DesiredBounds>)
        (constraints : MeasureConstraints)
        : MeasuredNode<DesiredBounds>
        =
        // Reduce available space by border thickness (1 on each side = 2 total)
        let borderThickness = 2

        let childConstraints =
            {
                MaxWidth = max 0 (constraints.MaxWidth - borderThickness)
                MaxHeight = max 0 (constraints.MaxHeight - borderThickness)
            }

        // Measure child with reduced constraints
        let childMeasured = measureEither childConstraints child

        // Container's measurements wrap child's measurements
        let containerMeasured =
            {
                MinWidth = childMeasured.Measured.MinWidth + borderThickness
                PreferredWidth = childMeasured.Measured.PreferredWidth + borderThickness
                MaxWidth = childMeasured.Measured.MaxWidth |> Option.map (fun w -> w + borderThickness)
                MinHeightForWidth =
                    fun w ->
                        // CRITICAL: Handle narrow widths
                        let innerWidth = max 0 (w - borderThickness)
                        childMeasured.Measured.MinHeightForWidth innerWidth + borderThickness
                PreferredHeightForWidth =
                    fun w ->
                        let innerWidth = max 0 (w - borderThickness)
                        childMeasured.Measured.PreferredHeightForWidth innerWidth + borderThickness
                MaxHeightForWidth =
                    fun w ->
                        let innerWidth = max 0 (w - borderThickness)

                        childMeasured.Measured.MaxHeightForWidth innerWidth
                        |> Option.map (fun h -> h + borderThickness)
            }

        {
            Vdom = KeylessVdom.Unkeyed (UnkeyedVdom.Bordered child)
            Measured = containerMeasured
            Children = [ childMeasured ]
        }

    /// Measure a vertical split with fixed proportion
    and private measureVerticalSplitProportion
        (p : float)
        (child1 : KeylessVdom<DesiredBounds>)
        (child2 : KeylessVdom<DesiredBounds>)
        (constraints : MeasureConstraints)
        : MeasuredNode<DesiredBounds>
        =
        // Both children measure with same parent constraints
        let child1Measured = measureEither constraints child1
        let child2Measured = measureEither constraints child2

        let m1 = child1Measured.Measured
        let m2 = child2Measured.Measured

        // For fixed proportion, compute minimum total width:
        // p*total >= m1.MinWidth => total >= m1.MinWidth / p
        // (1-p)*total >= m2.MinWidth => total >= m2.MinWidth / (1-p)
        let minFromChild1 = int (ceil (float m1.MinWidth / p))
        let minFromChild2 = int (ceil (float m2.MinWidth / (1.0 - p)))

        {
            Vdom =
                KeylessVdom.Unkeyed (
                    UnkeyedVdom.PanelSplit (SplitDirection.Vertical, SplitBehaviour.Proportion p, child1, child2)
                )
            Measured =
                {
                    MinWidth = max minFromChild1 minFromChild2
                    // What total width lets each child hit its preference?
                    PreferredWidth =
                        let c1Pref = int (ceil (float m1.PreferredWidth / p))
                        let c2Pref = int (ceil (float m2.PreferredWidth / (1.0 - p)))
                        max c1Pref c2Pref
                    MaxWidth = None // Can grow to accommodate both
                    MinHeightForWidth =
                        fun w ->
                            let w1 = int (float w * p)
                            let w2 = w - w1
                            max (m1.MinHeightForWidth w1) (m2.MinHeightForWidth w2)
                    PreferredHeightForWidth =
                        fun w ->
                            let w1 = int (float w * p)
                            let w2 = w - w1
                            max (m1.PreferredHeightForWidth w1) (m2.PreferredHeightForWidth w2)
                    MaxHeightForWidth =
                        fun w ->
                            let w1 = int (float w * p)
                            let w2 = w - w1

                            match m1.MaxHeightForWidth w1, m2.MaxHeightForWidth w2 with
                            | Some h1, Some h2 -> Some (max h1 h2)
                            | _ -> None
                }
            Children = [ child1Measured ; child2Measured ]
        }

    /// Measure a horizontal split with fixed proportion
    and private measureHorizontalSplitProportion
        (p : float)
        (child1 : KeylessVdom<DesiredBounds>)
        (child2 : KeylessVdom<DesiredBounds>)
        (constraints : MeasureConstraints)
        : MeasuredNode<DesiredBounds>
        =
        // Both children measure with same parent constraints
        let child1Measured = measureEither constraints child1
        let child2Measured = measureEither constraints child2

        let m1 = child1Measured.Measured
        let m2 = child2Measured.Measured

        {
            Vdom =
                KeylessVdom.Unkeyed (
                    UnkeyedVdom.PanelSplit (SplitDirection.Horizontal, SplitBehaviour.Proportion p, child1, child2)
                )
            Measured =
                {
                    MinWidth = max m1.MinWidth m2.MinWidth
                    PreferredWidth = max m1.PreferredWidth m2.PreferredWidth
                    MaxWidth = None
                    MinHeightForWidth =
                        fun w ->
                            let h1 = m1.MinHeightForWidth w
                            let h2 = m2.MinHeightForWidth w
                            let totalH = int (ceil (float h1 / p))
                            let totalH2 = int (ceil (float h2 / (1.0 - p)))
                            max totalH totalH2
                    PreferredHeightForWidth =
                        fun w ->
                            let h1 = m1.PreferredHeightForWidth w
                            let h2 = m2.PreferredHeightForWidth w
                            let totalH = int (ceil (float h1 / p))
                            let totalH2 = int (ceil (float h2 / (1.0 - p)))
                            max totalH totalH2
                    MaxHeightForWidth =
                        fun w ->
                            match m1.MaxHeightForWidth w, m2.MaxHeightForWidth w with
                            | Some h1, Some h2 ->
                                let totalH = int (ceil (float h1 / p))
                                let totalH2 = int (ceil (float h2 / (1.0 - p)))
                                Some (max totalH totalH2)
                            | _ -> None
                }
            Children = [ child1Measured ; child2Measured ]
        }

    /// Measure a vertical split with auto (content-driven) sizing
    and private measureVerticalSplitAuto
        (child1 : KeylessVdom<DesiredBounds>)
        (child2 : KeylessVdom<DesiredBounds>)
        (constraints : MeasureConstraints)
        : MeasuredNode<DesiredBounds>
        =
        let child1Measured = measureEither constraints child1
        let child2Measured = measureEither constraints child2

        let m1 = child1Measured.Measured
        let m2 = child2Measured.Measured

        // Helper: compute width split - must match arrange logic for accurate height calculation
        let computeWidthSplit (totalWidth : int) : int * int =
            let totalPref = m1.PreferredWidth + m2.PreferredWidth
            let minSum = m1.MinWidth + m2.MinWidth

            if totalWidth < minSum then
                // Can't satisfy minimums - scale proportionally by minimum requirements
                let scale = float totalWidth / float minSum
                let w1 = int (float m1.MinWidth * scale)
                (w1, totalWidth - w1)
            elif totalWidth >= minSum && totalWidth <= totalPref then
                // Between minimums and preferences - distribute by preference ratio
                let p =
                    if totalPref = 0 then
                        0.5
                    else
                        float m1.PreferredWidth / float totalPref
                // Satisfy minimums, distribute remainder by ratio
                let remainder = totalWidth - minSum
                let w1 = m1.MinWidth + int (float remainder * p)
                (w1, totalWidth - w1)
            else // totalWidth > totalPref
                // More than preferences - distribute excess proportionally to preferences
                let p =
                    if totalPref = 0 then
                        0.5
                    else
                        float m1.PreferredWidth / float totalPref

                let extraSpace = totalWidth - totalPref
                let extraFor1 = int (float extraSpace * p)
                let w1 = m1.PreferredWidth + extraFor1
                (w1, totalWidth - w1)

        {
            Vdom =
                KeylessVdom.Unkeyed (
                    UnkeyedVdom.PanelSplit (SplitDirection.Vertical, SplitBehaviour.Auto, child1, child2)
                )
            Measured =
                {
                    MinWidth = m1.MinWidth + m2.MinWidth
                    PreferredWidth = m1.PreferredWidth + m2.PreferredWidth
                    MaxWidth = None
                    MinHeightForWidth =
                        fun w ->
                            let w1, w2 = computeWidthSplit w
                            max (m1.MinHeightForWidth w1) (m2.MinHeightForWidth w2)
                    PreferredHeightForWidth =
                        fun w ->
                            let w1, w2 = computeWidthSplit w
                            max (m1.PreferredHeightForWidth w1) (m2.PreferredHeightForWidth w2)
                    MaxHeightForWidth =
                        fun w ->
                            let w1, w2 = computeWidthSplit w

                            match m1.MaxHeightForWidth w1, m2.MaxHeightForWidth w2 with
                            | Some h1, Some h2 -> Some (max h1 h2)
                            | _ -> None
                }
            Children = [ child1Measured ; child2Measured ]
        }

    /// Measure a horizontal split with auto (content-driven) sizing
    and private measureHorizontalSplitAuto
        (child1 : KeylessVdom<DesiredBounds>)
        (child2 : KeylessVdom<DesiredBounds>)
        (constraints : MeasureConstraints)
        : MeasuredNode<DesiredBounds>
        =
        let child1Measured = measureEither constraints child1
        let child2Measured = measureEither constraints child2

        let m1 = child1Measured.Measured
        let m2 = child2Measured.Measured

        {
            Vdom =
                KeylessVdom.Unkeyed (
                    UnkeyedVdom.PanelSplit (SplitDirection.Horizontal, SplitBehaviour.Auto, child1, child2)
                )
            Measured =
                {
                    MinWidth = max m1.MinWidth m2.MinWidth
                    PreferredWidth = max m1.PreferredWidth m2.PreferredWidth
                    MaxWidth = None
                    MinHeightForWidth = fun w -> m1.MinHeightForWidth w + m2.MinHeightForWidth w
                    PreferredHeightForWidth = fun w -> m1.PreferredHeightForWidth w + m2.PreferredHeightForWidth w
                    MaxHeightForWidth =
                        fun w ->
                            match m1.MaxHeightForWidth w, m2.MaxHeightForWidth w with
                            | Some h1, Some h2 -> Some (h1 + h2)
                            | _ -> None
                }
            Children = [ child1Measured ; child2Measured ]
        }

    /// Measure a vertical split with absolute first child width
    and private measureVerticalSplitAbsolute
        (n : int)
        (child1 : KeylessVdom<DesiredBounds>)
        (child2 : KeylessVdom<DesiredBounds>)
        (constraints : MeasureConstraints)
        : MeasuredNode<DesiredBounds>
        =
        // If n < 0, it means "reserve abs(n) for child2, give child1 the rest"
        // Otherwise n >= 0 means "give child1 exactly n, child2 gets the rest"
        let isNegative = n < 0
        let absN = abs n

        // IMPORTANT: Constrain the child that gets the fixed allocation
        let child1Constraints, child2Constraints =
            if isNegative then
                // Child2 gets fixed allocation of absN
                constraints,
                {
                    MaxWidth = max 0 (min absN constraints.MaxWidth)
                    MaxHeight = constraints.MaxHeight
                }
            else
                // Child1 gets fixed allocation of n
                {
                    MaxWidth = max 0 (min n constraints.MaxWidth)
                    MaxHeight = constraints.MaxHeight
                },
                constraints

        let child1Measured = measureEither child1Constraints child1
        let child2Measured = measureEither child2Constraints child2

        let m1 = child1Measured.Measured
        let m2 = child2Measured.Measured

        // Compute container's min/preferred width
        let containerMinWidth, containerPreferredWidth =
            if isNegative then
                // Reserve absN for child2, child1 gets the rest
                m1.MinWidth + absN, m1.PreferredWidth + absN
            else
                // Reserve n for child1, child2 gets the rest
                n + m2.MinWidth, n + m2.PreferredWidth

        {
            Vdom =
                KeylessVdom.Unkeyed (
                    UnkeyedVdom.PanelSplit (SplitDirection.Vertical, SplitBehaviour.Absolute n, child1, child2)
                )
            Measured =
                {
                    MinWidth = containerMinWidth
                    PreferredWidth = containerPreferredWidth
                    MaxWidth = None
                    MinHeightForWidth =
                        fun w ->
                            let w1, w2 =
                                if isNegative then
                                    // Child2 gets min(absN, w), child1 gets remainder
                                    let w2 = min absN w
                                    let w1 = max 0 (w - w2)
                                    w1, w2
                                else
                                    // Child1 gets min(n, w), child2 gets remainder
                                    let w1 = min n w
                                    let w2 = max 0 (w - w1)
                                    w1, w2

                            max (m1.MinHeightForWidth w1) (m2.MinHeightForWidth w2)
                    PreferredHeightForWidth =
                        fun w ->
                            let w1, w2 =
                                if isNegative then
                                    let w2 = min absN w
                                    let w1 = max 0 (w - w2)
                                    w1, w2
                                else
                                    let w1 = min n w
                                    let w2 = max 0 (w - w1)
                                    w1, w2

                            max (m1.PreferredHeightForWidth w1) (m2.PreferredHeightForWidth w2)
                    MaxHeightForWidth =
                        fun w ->
                            let w1, w2 =
                                if isNegative then
                                    let w2 = min absN w
                                    let w1 = max 0 (w - w2)
                                    w1, w2
                                else
                                    let w1 = min n w
                                    let w2 = max 0 (w - w1)
                                    w1, w2

                            match m1.MaxHeightForWidth w1, m2.MaxHeightForWidth w2 with
                            | Some h1, Some h2 -> Some (max h1 h2)
                            | _ -> None
                }
            Children = [ child1Measured ; child2Measured ]
        }

    /// Measure a horizontal split with absolute first child height
    and private measureHorizontalSplitAbsolute
        (n : int)
        (child1 : KeylessVdom<DesiredBounds>)
        (child2 : KeylessVdom<DesiredBounds>)
        (constraints : MeasureConstraints)
        : MeasuredNode<DesiredBounds>
        =
        // If n < 0, it means "reserve abs(n) for child2, give child1 the rest"
        // Otherwise n >= 0 means "give child1 exactly n, child2 gets the rest"
        let isNegative = n < 0
        let absN = abs n

        // IMPORTANT: Constrain the child that gets the fixed allocation
        let child1Constraints, child2Constraints =
            if isNegative then
                // Child2 gets fixed allocation of absN
                constraints,
                {
                    MaxWidth = constraints.MaxWidth
                    MaxHeight = max 0 (min absN constraints.MaxHeight)
                }
            else
                // Child1 gets fixed allocation of n
                {
                    MaxWidth = constraints.MaxWidth
                    MaxHeight = max 0 (min n constraints.MaxHeight)
                },
                constraints

        let child1Measured = measureEither child1Constraints child1
        let child2Measured = measureEither child2Constraints child2

        let m1 = child1Measured.Measured
        let m2 = child2Measured.Measured

        {
            Vdom =
                KeylessVdom.Unkeyed (
                    UnkeyedVdom.PanelSplit (SplitDirection.Horizontal, SplitBehaviour.Absolute n, child1, child2)
                )
            Measured =
                {
                    MinWidth = max m1.MinWidth m2.MinWidth
                    PreferredWidth = max m1.PreferredWidth m2.PreferredWidth
                    MaxWidth = None
                    MinHeightForWidth =
                        fun w ->
                            if isNegative then
                                // Reserve absN for child2, child1 gets the rest
                                m1.MinHeightForWidth w + absN
                            else
                                // Reserve n for child1, child2 gets the rest
                                n + m2.MinHeightForWidth w
                    PreferredHeightForWidth =
                        fun w ->
                            if isNegative then
                                m1.PreferredHeightForWidth w + absN
                            else
                                n + m2.PreferredHeightForWidth w
                    MaxHeightForWidth =
                        fun w ->
                            if isNegative then
                                match m1.MaxHeightForWidth w with
                                | Some h1 -> Some (h1 + absN)
                                | None -> None
                            else
                                match m2.MaxHeightForWidth w with
                                | Some h2 -> Some (n + h2)
                                | None -> None
                }
            Children = [ child1Measured ; child2Measured ]
        }

    /// Measure any KeylessVdom node
    and private measureEither
        (constraints : MeasureConstraints)
        (vdom : KeylessVdom<DesiredBounds>)
        : MeasuredNode<DesiredBounds>
        =
        match vdom with
        | KeylessVdom.Keyed keyedVdom ->
            match keyedVdom with
            | KeyedVdom.WithKey (_, unkeyedVdom) ->
                let childMeasured = measureUnkeyed constraints unkeyedVdom

                {
                    Vdom = KeylessVdom.Keyed keyedVdom
                    Measured = childMeasured.Measured
                    Children = [ childMeasured ]
                }
        | KeylessVdom.Unkeyed unkeyedVdom -> measureUnkeyed constraints unkeyedVdom

    /// Measure an unkeyed VDOM node
    and private measureUnkeyed
        (constraints : MeasureConstraints)
        (vdom : UnkeyedVdom<DesiredBounds>)
        : MeasuredNode<DesiredBounds>
        =
        match vdom with
        | UnkeyedVdom.TextContent (text, _focused) ->
            {
                Vdom = KeylessVdom.Unkeyed vdom
                Measured = measureText text constraints
                Children = []
            }
        | UnkeyedVdom.Checkbox (_isChecked, _isFocused) ->
            {
                Vdom = KeylessVdom.Unkeyed vdom
                Measured = measureCheckbox
                Children = []
            }
        | UnkeyedVdom.Bordered child -> measureBordered child constraints
        | UnkeyedVdom.PanelSplit (direction, behaviour, child1, child2) ->
            match direction, behaviour with
            | SplitDirection.Vertical, SplitBehaviour.Proportion p ->
                measureVerticalSplitProportion p child1 child2 constraints
            | SplitDirection.Vertical, SplitBehaviour.Absolute n ->
                measureVerticalSplitAbsolute n child1 child2 constraints
            | SplitDirection.Vertical, SplitBehaviour.Auto -> measureVerticalSplitAuto child1 child2 constraints
            | SplitDirection.Horizontal, SplitBehaviour.Proportion p ->
                measureHorizontalSplitProportion p child1 child2 constraints
            | SplitDirection.Horizontal, SplitBehaviour.Absolute n ->
                measureHorizontalSplitAbsolute n child1 child2 constraints
            | SplitDirection.Horizontal, SplitBehaviour.Auto -> measureHorizontalSplitAuto child1 child2 constraints
        | UnkeyedVdom.Focusable (_, keyedVdom) ->
            // Focusable is transparent for measurement purposes
            let childMeasured = measureEither constraints (KeylessVdom.Keyed keyedVdom)

            {
                Vdom = KeylessVdom.Unkeyed vdom
                Measured = childMeasured.Measured
                Children = [ childMeasured ]
            }

    /// Arrange a measured tree into concrete bounds
    let rec internal arrange (measured : MeasuredNode<DesiredBounds>) (bounds : Rectangle) : ArrangedNode =
        match measured.Vdom with
        | KeylessVdom.Keyed (KeyedVdom.WithKey (key, unkeyedVdom)) ->
            match unkeyedVdom with
            | UnkeyedVdom.TextContent (text, focused) ->
                {
                    Vdom = KeylessVdom.Keyed (KeyedVdom.WithKey (key, UnkeyedVdom.TextContent (text, focused)))
                    Bounds = bounds
                    Children = []
                }
            | UnkeyedVdom.Checkbox (isChecked, isFocused) ->
                {
                    Vdom = KeylessVdom.Keyed (KeyedVdom.WithKey (key, UnkeyedVdom.Checkbox (isChecked, isFocused)))
                    Bounds = bounds
                    Children = []
                }
            | UnkeyedVdom.Bordered _ ->
                let borderThickness = 2

                let innerBounds =
                    {
                        TopLeftX = bounds.TopLeftX + 1
                        TopLeftY = bounds.TopLeftY + 1
                        Width = max 0 (bounds.Width - borderThickness)
                        Height = max 0 (bounds.Height - borderThickness)
                    }

                let childArranged = arrange measured.Children.[0] innerBounds
                let childVdom = childArranged.Vdom

                {
                    Vdom = KeylessVdom.Keyed (KeyedVdom.WithKey (key, UnkeyedVdom.Bordered childVdom))
                    Bounds = bounds
                    Children = [ childArranged ]
                }
            | UnkeyedVdom.PanelSplit (dir, behaviour, _, _) ->
                // Keyed wrappers store the wrapped node as their only child (see measureEither's Keyed case),
                // so we must unwrap to get the actual PanelSplit's MeasuredNode
                let childrenArranged = arrangePanelSplit measured.Children.[0] bounds dir behaviour
                let child1Vdom = childrenArranged.[0].Vdom
                let child2Vdom = childrenArranged.[1].Vdom

                {
                    Vdom =
                        KeylessVdom.Keyed (
                            KeyedVdom.WithKey (key, UnkeyedVdom.PanelSplit (dir, behaviour, child1Vdom, child2Vdom))
                        )
                    Bounds = bounds
                    Children = childrenArranged
                }
            | UnkeyedVdom.Focusable (isInitial, _) ->
                let childArranged = arrange measured.Children.[0] bounds
                let childVdom = childArranged.Vdom

                let focusableVdom =
                    match childVdom with
                    | KeylessVdom.Keyed keyedVdom ->
                        KeylessVdom.Keyed (KeyedVdom.WithKey (key, UnkeyedVdom.Focusable (isInitial, keyedVdom)))
                    | KeylessVdom.Unkeyed _ -> failwith "Focusable child must be keyed"

                {
                    Vdom = focusableVdom
                    Bounds = bounds
                    Children = [ childArranged ]
                }
        | KeylessVdom.Unkeyed unkeyedVdom ->
            match unkeyedVdom with
            | UnkeyedVdom.TextContent (text, focused) ->
                {
                    Vdom = KeylessVdom.Unkeyed (UnkeyedVdom.TextContent (text, focused))
                    Bounds = bounds
                    Children = []
                }
            | UnkeyedVdom.Checkbox (isChecked, isFocused) ->
                {
                    Vdom = KeylessVdom.Unkeyed (UnkeyedVdom.Checkbox (isChecked, isFocused))
                    Bounds = bounds
                    Children = []
                }
            | UnkeyedVdom.Bordered _ ->
                let borderThickness = 2

                let innerBounds =
                    {
                        TopLeftX = bounds.TopLeftX + 1
                        TopLeftY = bounds.TopLeftY + 1
                        Width = max 0 (bounds.Width - borderThickness)
                        Height = max 0 (bounds.Height - borderThickness)
                    }

                let childArranged = arrange measured.Children.[0] innerBounds
                let childVdom = childArranged.Vdom

                {
                    Vdom = KeylessVdom.Unkeyed (UnkeyedVdom.Bordered childVdom)
                    Bounds = bounds
                    Children = [ childArranged ]
                }
            | UnkeyedVdom.PanelSplit (dir, behaviour, _, _) ->
                // Unkeyed nodes have no wrapper, so 'measured' is directly the PanelSplit's MeasuredNode
                let childrenArranged = arrangePanelSplit measured bounds dir behaviour
                let child1Vdom = childrenArranged.[0].Vdom
                let child2Vdom = childrenArranged.[1].Vdom

                {
                    Vdom = KeylessVdom.Unkeyed (UnkeyedVdom.PanelSplit (dir, behaviour, child1Vdom, child2Vdom))
                    Bounds = bounds
                    Children = childrenArranged
                }
            | UnkeyedVdom.Focusable (isInitial, _) ->
                let childArranged = arrange measured.Children.[0] bounds
                let childVdom = childArranged.Vdom

                let focusableVdom =
                    match childVdom with
                    | KeylessVdom.Keyed keyedVdom -> KeylessVdom.Unkeyed (UnkeyedVdom.Focusable (isInitial, keyedVdom))
                    | KeylessVdom.Unkeyed _ -> failwith "Focusable child must be keyed"

                {
                    Vdom = focusableVdom
                    Bounds = bounds
                    Children = [ childArranged ]
                }

    /// Arrange a panel split container
    and private arrangePanelSplit
        (measured : MeasuredNode<DesiredBounds>)
        (bounds : Rectangle)
        (direction : SplitDirection)
        (behaviour : SplitBehaviour)
        : ArrangedNode list
        =
        let child1 = measured.Children.[0]
        let child2 = measured.Children.[1]
        let m1 = child1.Measured
        let m2 = child2.Measured

        match direction with
        | SplitDirection.Vertical ->
            let w1, w2 =
                match behaviour with
                | SplitBehaviour.Proportion p ->
                    // Calculate widths based on proportion
                    // CRITICAL: Always calculate w2 as remainder to avoid rounding gaps
                    // For Proportion splits, we ALWAYS honor the proportion exactly.
                    // Child minimums are soft constraints - if violated, children must
                    // render gracefully in degraded space. This ensures UI stability:
                    // as child content changes, the split ratio remains constant.
                    let w1 = int (float bounds.Width * p)
                    let w2 = bounds.Width - w1
                    (w1, w2)
                | SplitBehaviour.Absolute n ->
                    // Negative n means "give the second child n pixels, first gets the rest"
                    // Positive n means "give the first child n pixels, second gets the rest"
                    let w1 = if n < 0 then bounds.Width + n else min n bounds.Width
                    let w1 = max 0 w1 // Ensure non-negative
                    let w2 = bounds.Width - w1
                    (w1, w2)
                | SplitBehaviour.Auto ->
                    let totalPref = m1.PreferredWidth + m2.PreferredWidth
                    let minSum = m1.MinWidth + m2.MinWidth

                    if bounds.Width < minSum then
                        // Can't satisfy minimums - scale proportionally by minimum requirements
                        let scale = float bounds.Width / float minSum
                        let w1 = int (float m1.MinWidth * scale)
                        (w1, bounds.Width - w1)
                    elif bounds.Width >= minSum && bounds.Width <= totalPref then
                        // Between minimums and preferences - distribute by preference ratio
                        let p =
                            if totalPref = 0 then
                                0.5
                            else
                                float m1.PreferredWidth / float totalPref
                        // Satisfy minimums, distribute remainder by ratio
                        let remainder = bounds.Width - minSum
                        let w1 = m1.MinWidth + int (float remainder * p)
                        (w1, bounds.Width - w1)
                    else // bounds.Width > totalPref
                        // More than preferences - distribute excess proportionally to preferences
                        let p =
                            if totalPref = 0 then
                                0.5
                            else
                                float m1.PreferredWidth / float totalPref

                        let extraSpace = bounds.Width - totalPref
                        let extraFor1 = int (float extraSpace * p)
                        let w1 = m1.PreferredWidth + extraFor1
                        (w1, bounds.Width - w1)

            let bounds1 =
                {
                    TopLeftX = bounds.TopLeftX
                    TopLeftY = bounds.TopLeftY
                    Width = w1
                    Height = bounds.Height
                }

            let bounds2 =
                {
                    TopLeftX = bounds.TopLeftX + w1
                    TopLeftY = bounds.TopLeftY
                    Width = w2
                    Height = bounds.Height
                }

            let arranged1 = arrange child1 bounds1
            let arranged2 = arrange child2 bounds2
            [ arranged1 ; arranged2 ]

        | SplitDirection.Horizontal ->
            let h1, h2 =
                match behaviour with
                | SplitBehaviour.Proportion p ->
                    // Calculate heights based on proportion
                    // CRITICAL: Always calculate h2 as remainder to avoid rounding gaps
                    // For Proportion splits, we ALWAYS honor the proportion exactly.
                    // Child minimums are soft constraints - if violated, children must
                    // render gracefully in degraded space. This ensures UI stability:
                    // as child content changes, the split ratio remains constant.
                    let h1 = int (float bounds.Height * p)
                    let h2 = bounds.Height - h1
                    (h1, h2)
                | SplitBehaviour.Absolute n ->
                    // Negative n means "give the second child n pixels, first gets the rest"
                    // Positive n means "give the first child n pixels, second gets the rest"
                    let h1 = if n < 0 then bounds.Height + n else min n bounds.Height
                    let h1 = max 0 h1 // Ensure non-negative
                    let h2 = bounds.Height - h1
                    (h1, h2)
                | SplitBehaviour.Auto ->
                    let prefH1 = m1.PreferredHeightForWidth bounds.Width
                    let prefH2 = m2.PreferredHeightForWidth bounds.Width
                    let totalPref = prefH1 + prefH2
                    let minH1 = m1.MinHeightForWidth bounds.Width
                    let minH2 = m2.MinHeightForWidth bounds.Width
                    let minSum = minH1 + minH2

                    if bounds.Height < minSum then
                        // Can't satisfy minimums - scale proportionally by minimum requirements
                        let scale = float bounds.Height / float minSum
                        let h1 = int (float minH1 * scale)
                        (h1, bounds.Height - h1)
                    elif bounds.Height >= minSum && bounds.Height <= totalPref then
                        // Between minimums and preferences - distribute by preference ratio
                        let p =
                            if totalPref = 0 then
                                0.5
                            else
                                float prefH1 / float totalPref
                        // Satisfy minimums, distribute remainder by ratio
                        let remainder = bounds.Height - minSum
                        let h1 = minH1 + int (float remainder * p)
                        (h1, bounds.Height - h1)
                    else // bounds.Height > totalPref
                        // More than preferences - distribute excess proportionally to preferences
                        // BUT respect max height constraints
                        let maxH1 = m1.MaxHeightForWidth bounds.Width
                        let maxH2 = m2.MaxHeightForWidth bounds.Width

                        // Clamp preferences to maxes
                        let effectivePrefH1 =
                            match maxH1 with
                            | Some m -> min prefH1 m
                            | None -> prefH1

                        let effectivePrefH2 =
                            match maxH2 with
                            | Some m -> min prefH2 m
                            | None -> prefH2

                        let effectiveTotalPref = effectivePrefH1 + effectivePrefH2

                        if bounds.Height <= effectiveTotalPref then
                            // Even after clamping to maxes, we're at or below total
                            // Distribute as normal
                            let p =
                                if effectiveTotalPref = 0 then
                                    0.5
                                else
                                    float effectivePrefH1 / float effectiveTotalPref

                            let remainder = bounds.Height - (minH1 + minH2)
                            let h1 = minH1 + int (float remainder * p)
                            (h1, bounds.Height - h1)
                        else
                            // Have excess beyond maxes - give each their effective pref, distribute remainder
                            let extraSpace = bounds.Height - effectiveTotalPref

                            let p =
                                if effectiveTotalPref = 0 then
                                    0.5
                                else
                                    float effectivePrefH1 / float effectiveTotalPref

                            let extraFor1 = int (float extraSpace * p)
                            let h1 = effectivePrefH1 + extraFor1
                            // Clamp h1 to max if it exists (shouldn't exceed it, but just to be safe)
                            let h1 =
                                match maxH1 with
                                | Some m -> min h1 m
                                | None -> h1

                            (h1, bounds.Height - h1)

            let bounds1 =
                {
                    TopLeftX = bounds.TopLeftX
                    TopLeftY = bounds.TopLeftY
                    Width = bounds.Width
                    Height = h1
                }

            let bounds2 =
                {
                    TopLeftX = bounds.TopLeftX
                    TopLeftY = bounds.TopLeftY + h1
                    Width = bounds.Width
                    Height = h2
                }

            let arranged1 = arrange child1 bounds1
            let arranged2 = arrange child2 bounds2
            [ arranged1 ; arranged2 ]

    /// Top-level layout function: measure then arrange
    let internal layout (vdom : Vdom<DesiredBounds, Unkeyed>) (terminalBounds : Rectangle) : ArrangedNode =
        // Phase 1: Measure with terminal bounds as constraints
        let constraints =
            {
                MaxWidth = terminalBounds.Width
                MaxHeight = terminalBounds.Height
            }

        let measured =
            match vdom with
            | Vdom.Unkeyed (unkeyedVdom, _) -> measureUnkeyed constraints unkeyedVdom
            | Vdom.Keyed _ -> failwith "Top-level vdom must be unkeyed"

        // Phase 2: Arrange with terminal bounds
        arrange measured terminalBounds
