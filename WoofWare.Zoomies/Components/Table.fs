namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies
open TypeEquality

/// Sizing specification for a table column
type ColumnSpec =
    /// Column width determined by widest cell content
    | AutoColumn
    /// Column has fixed width (measured in cells)
    | FixedColumn of width : int
    /// Column gets proportional share of remaining space after auto/fixed columns
    /// Proportion must be > 0
    | ProportionColumn of proportion : float

/// Sizing specification for a table row
type RowSpec =
    /// Row height determined by tallest cell (given allocated column widths)
    | AutoRow
    /// Row has fixed height (measured in cells)
    | FixedRow of height : int
    /// Row gets proportional share of remaining space after auto/fixed rows
    /// Proportion must be > 0
    | ProportionRow of proportion : float

[<RequireQualifiedAccess>]
module Table =
    /// Normalize cells to have uniform column count by padding short rows with Vdom.empty
    ///
    /// The result has length equal to the input, but is now a rectangular array where every row is as long as the longest
    /// row. User-provided keys are preserved. Returns pairs of (actual vdom, keyless for measurement).
    let private normalizeCells (cells : Vdom<DesiredBounds, 'keyed>[][]) : (obj * KeylessVdom<DesiredBounds>)[,] =
        if cells.Length = 0 then
            Array2D.zeroCreate 0 0
        else
            let numRows = cells.Length
            let maxCols = cells |> Array.maxOf 0 _.Length

            let emptyVdom = Vdom.empty :> obj

            let emptyKeyless =
                match Vdom.empty with
                | Vdom.Unkeyed (u, _teq) -> KeylessVdom.Unkeyed u
                | Vdom.Keyed (k, _teq) -> KeylessVdom.Keyed k

            let result = Array2D.create numRows maxCols (emptyVdom, emptyKeyless)

            for rowIdx = 0 to cells.Length - 1 do
                let row = cells.[rowIdx]
                assert (row.Length <= maxCols)

                for colIdx = 0 to row.Length - 1 do
                    let cell = row.[colIdx]

                    let keylessVdom =
                        match cell with
                        | Vdom.Keyed (keyed, _teq) -> KeylessVdom.Keyed keyed
                        | Vdom.Unkeyed (unkeyed, _teq) -> KeylessVdom.Unkeyed unkeyed

                    Array2D.set result rowIdx colIdx (box cell, keylessVdom)

            // let's just make it very obvious what our result's dimensions are!
            assert (result.GetLength 0 = numRows)
            assert (result.GetLength 1 = maxCols)
            result

    /// Normalize specs to match expected count, using default for missing specs and truncating extras.
    ///
    /// The resulting array has length exactly `expectedCount`.
    let private normalizeSpecs<'spec> (specs : 'spec[]) (expectedCount : int) (defaultSpec : 'spec) : 'spec[] =
        let result =
            if Array.length specs < expectedCount then
                Array.append specs (Array.create (expectedCount - Array.length specs) defaultSpec)
            else
                Array.take expectedCount specs

        assert (result.Length = expectedCount)
        result

    /// Sanitize proportion values to ensure they are positive and real; clamp noncompliant values to epsilon
    let private sanitizeColumnSpec (spec : ColumnSpec) : ColumnSpec =
        match spec with
        | ProportionColumn p when p <= 0.0 || System.Double.IsNaN p || System.Double.IsInfinity p ->
            ProportionColumn 0.01
        | other -> other

    /// Sanitize proportion values to ensure they are positive and real; clamp noncompliant values to epsilon
    let private sanitizeRowSpec (spec : RowSpec) : RowSpec =
        match spec with
        | ProportionRow p when p <= 0.0 || System.Double.IsNaN p || System.Double.IsInfinity p -> ProportionRow 0.01
        | other -> other

    /// Allocate column widths from available width, respecting per-column minima.
    let private allocateColumnWidths
        (availableWidth : int)
        (columnSpecs : ColumnSpec[])
        (cellMeasurements : MeasuredSize[,])
        : int[]
        =
        assert (columnSpecs.Length = cellMeasurements.GetLength 1)

        if Array.isEmpty columnSpecs then
            [||]
        else

        let numCols = Array.length columnSpecs

        if availableWidth <= 0 then
            Array.zeroCreate numCols
        else

        let columnInfo =
            columnSpecs
            |> Array.mapi (fun colIdx spec ->
                let cellMeasForCol = Array.getColumn colIdx cellMeasurements

                let minWidth =
                    match spec with
                    | FixedColumn w -> w
                    | AutoColumn
                    | ProportionColumn _ -> cellMeasForCol |> Array.maxOf 0 _.MinWidth

                let preferredWidth =
                    match spec with
                    | FixedColumn w -> w
                    | AutoColumn
                    | ProportionColumn _ -> cellMeasForCol |> Array.maxOf 0 _.PreferredWidth

                (spec, colIdx, minWidth, preferredWidth)
            )

        assert (columnInfo.Length = numCols)

        let totalMin = columnInfo |> Array.sumBy (fun (_, _, minW, _) -> minW)

        if totalMin >= availableWidth then
            // Over-constrained: prioritize fixed columns, then shrink auto columns
            // Separate fixed from auto/proportion
            let fixedWidth =
                columnInfo
                |> Array.sumBy (fun (spec, _, minW, _) ->
                    match spec with
                    | FixedColumn w -> w
                    | _ -> 0
                )

            if fixedWidth >= availableWidth then
                // Even fixed columns alone exceed available width: must scale everything proportionally
                let totalMinF = float totalMin

                let scaled =
                    columnInfo
                    |> Array.map (fun (_, _, minW, _) -> int (float minW * float availableWidth / totalMinF))

                let mutable allocated = scaled |> Array.sum
                let mutable idx = 0

                while allocated < availableWidth && idx < numCols do
                    scaled[idx] <- scaled[idx] + 1
                    allocated <- allocated + 1
                    idx <- idx + 1

                assert (scaled.Length = numCols)
                scaled
            else
                // Fixed columns fit: give them exact width, shrink auto/proportion to fit remainder
                let remainingWidth = availableWidth - fixedWidth

                let autoAndProportionMinTotal =
                    columnInfo
                    |> Array.sumBy (fun (spec, _, minW, _) ->
                        match spec with
                        | FixedColumn _ -> 0
                        | _ -> minW
                    )

                let scaled =
                    columnInfo
                    |> Array.map (fun (spec, _, minW, _) ->
                        match spec with
                        | FixedColumn w -> w // Fixed columns keep exact width
                        | _ when autoAndProportionMinTotal = 0 -> 0
                        | _ ->
                            // Scale auto/proportion columns proportionally to fit in remaining space
                            int (float minW * float remainingWidth / float autoAndProportionMinTotal)
                    )

                let mutable allocated = scaled |> Array.sum
                let mutable idx = 0

                // Distribute rounding errors to non-fixed columns
                while allocated < availableWidth && idx < numCols do
                    match columnInfo[idx] with
                    | FixedColumn _, _, _, _ -> () // Don't adjust fixed columns
                    | _ ->
                        scaled[idx] <- scaled[idx] + 1
                        allocated <- allocated + 1

                    idx <- idx + 1

                assert (scaled.Length = numCols)
                scaled
        else
            let widths = columnInfo |> Array.map (fun (_, _, minW, _) -> minW)

            let mutable remaining = availableWidth - totalMin

            // Grow auto columns toward preferred
            let autoColumns =
                columnInfo
                |> Array.choose (fun (spec, idx, minW, prefW) ->
                    match spec with
                    | AutoColumn -> Some (idx, max 0 (prefW - minW))
                    | _ -> None
                )

            let autoGrowthTotal = autoColumns |> Array.sumBy snd

            if autoGrowthTotal > 0 && remaining > 0 then
                let autoAllocated = min remaining autoGrowthTotal

                for idx, capacity in autoColumns do
                    let extra = int (float autoAllocated * (float capacity / float autoGrowthTotal))

                    widths[idx] <- widths[idx] + extra

                let spent = widths |> Array.sum |> (fun s -> s - totalMin)
                let mutable toDistribute = autoAllocated - spent
                let mutable autoIdx = 0

                while toDistribute > 0 && autoIdx < autoColumns.Length do
                    let colIndex, _ = autoColumns[autoIdx]
                    widths[colIndex] <- widths[colIndex] + 1
                    toDistribute <- toDistribute - 1
                    autoIdx <- autoIdx + 1

                remaining <- remaining - autoAllocated

            // Give remaining space to proportion columns according to their weights
            let proportionColumns =
                columnInfo
                |> Array.choose (fun (spec, idx, _, _) ->
                    match spec with
                    | ProportionColumn p -> Some (idx, p)
                    | _ -> None
                )

            let proportionTotal = proportionColumns |> Array.sumBy (fun (_, p) -> p)

            if proportionTotal > 0.0 && remaining > 0 then
                let mutable leftover = remaining

                for idx, p in proportionColumns do
                    let extra = int (float remaining * (p / proportionTotal))
                    widths[idx] <- widths[idx] + extra
                    leftover <- leftover - extra

                let mutable propIdx = 0

                while leftover > 0 && propIdx < proportionColumns.Length do
                    let colIndex, _ = proportionColumns[propIdx]
                    widths[colIndex] <- widths[colIndex] + 1
                    leftover <- leftover - 1
                    propIdx <- propIdx + 1

            assert (widths.Length = numCols)
            widths

    /// Allocate row heights from available height (similar to columns, but heights depend on column widths)
    let private allocateRowHeights
        (availableHeight : int)
        (rowSpecs : RowSpec[])
        (cellMeasurements : MeasuredSize[,])
        (columnWidths : int[])
        : int[]
        =
        assert (rowSpecs.Length = cellMeasurements.GetLength 0)
        assert (columnWidths.Length = cellMeasurements.GetLength 1)

        if Array.isEmpty rowSpecs then
            [||]
        else if

            availableHeight <= 0
        then
            Array.zeroCreate rowSpecs.Length
        else

        let numRows = rowSpecs.Length

        let rowInfo =
            rowSpecs
            |> Array.mapi (fun rowIdx spec ->
                let cellsInRow = Array.getRow rowIdx cellMeasurements
                assert (cellsInRow.Length = columnWidths.Length)

                let minHeight =
                    match spec with
                    | FixedRow h -> h
                    | AutoRow
                    | ProportionRow _ ->
                        Array.max2Of 0 (fun cell colWidth -> cell.MinHeightForWidth colWidth) cellsInRow columnWidths

                let preferredHeight =
                    match spec with
                    | FixedRow h -> h
                    | AutoRow
                    | ProportionRow _ ->
                        Array.max2Of
                            0
                            (fun cell colWidth -> cell.PreferredHeightForWidth colWidth)
                            cellsInRow
                            columnWidths

                (spec, rowIdx, minHeight, preferredHeight)
            )

        let totalMin = rowInfo |> Array.sumBy (fun (_, _, minH, _) -> minH)

        if totalMin >= availableHeight then
            // Over-constrained: prioritize fixed rows, then shrink auto rows (with 1-line floor)
            let fixedHeight =
                rowInfo
                |> Array.sumBy (fun (spec, _, minH, _) ->
                    match spec with
                    | FixedRow h -> h
                    | _ -> 0
                )

            if fixedHeight >= availableHeight then
                // Even fixed rows alone exceed available height: must scale everything proportionally
                let totalMinF = float totalMin

                let scaled =
                    rowInfo
                    |> Array.map (fun (spec, _, minH, _) ->
                        match spec with
                        | AutoRow -> max 1 (int (float minH * float availableHeight / totalMinF)) // 1-line floor for auto
                        | _ -> int (float minH * float availableHeight / totalMinF)
                    )

                let mutable allocated = scaled |> Array.sum
                let mutable idx = 0

                while allocated < availableHeight && idx < rowSpecs.Length do
                    scaled[idx] <- scaled[idx] + 1
                    allocated <- allocated + 1
                    idx <- idx + 1

                assert (scaled.Length = numRows)
                scaled
            else
                // Fixed rows fit: give them exact height, shrink auto/proportion to fit remainder
                let remainingHeight = availableHeight - fixedHeight

                let autoAndProportionMinTotal =
                    rowInfo
                    |> Array.sumBy (fun (spec, _, minH, _) ->
                        match spec with
                        | FixedRow _ -> 0
                        | _ -> minH
                    )

                let scaled =
                    rowInfo
                    |> Array.map (fun (spec, _, minH, _) ->
                        match spec with
                        | FixedRow h -> h // Fixed rows keep exact height
                        | AutoRow when autoAndProportionMinTotal = 0 -> 1 // 1-line floor
                        | AutoRow ->
                            // Scale auto rows proportionally, with 1-line floor
                            max 1 (int (float minH * float remainingHeight / float autoAndProportionMinTotal))
                        | ProportionRow _ when autoAndProportionMinTotal = 0 -> 0
                        | ProportionRow _ ->
                            // Scale proportion rows proportionally to fit in remaining space
                            int (float minH * float remainingHeight / float autoAndProportionMinTotal)
                    )

                let mutable allocated = scaled |> Array.sum
                let mutable idx = 0

                // Distribute rounding errors to non-fixed rows
                while allocated < availableHeight && idx < rowSpecs.Length do
                    match rowInfo[idx] with
                    | FixedRow _, _, _, _ -> () // Don't adjust fixed rows
                    | _ ->
                        scaled[idx] <- scaled[idx] + 1
                        allocated <- allocated + 1

                    idx <- idx + 1

                assert (scaled.Length = numRows)
                scaled
        else
            let heights = rowInfo |> Array.map (fun (_, _, minH, _) -> minH)

            let mutable remaining = availableHeight - totalMin

            let autoRows =
                rowInfo
                |> Array.choose (fun (spec, idx, minH, prefH) ->
                    match spec with
                    | AutoRow -> Some (idx, max 0 (prefH - minH))
                    | _ -> None
                )

            let autoGrowthTotal = autoRows |> Array.sumBy snd

            if autoGrowthTotal > 0 && remaining > 0 then
                let autoAllocated = min remaining autoGrowthTotal

                for idx, capacity in autoRows do
                    let extra = int (float autoAllocated * (float capacity / float autoGrowthTotal))

                    heights[idx] <- heights[idx] + extra

                let spent = heights |> Array.sum |> (fun s -> s - totalMin)
                let mutable toDistribute = autoAllocated - spent
                let mutable autoIdx = 0

                while toDistribute > 0 && autoIdx < autoRows.Length do
                    let rowIndex, _ = autoRows[autoIdx]
                    heights[rowIndex] <- heights[rowIndex] + 1
                    toDistribute <- toDistribute - 1
                    autoIdx <- autoIdx + 1

                remaining <- remaining - autoAllocated

            let proportionRows =
                rowInfo
                |> Array.choose (fun (spec, idx, _, _) ->
                    match spec with
                    | ProportionRow p -> Some (idx, p)
                    | _ -> None
                )

            let proportionTotal = proportionRows |> Array.sumBy (fun (_, p) -> p)

            if proportionTotal > 0.0 && remaining > 0 then
                let mutable leftover = remaining

                for idx, p in proportionRows do
                    let extra = int (float remaining * (p / proportionTotal))
                    heights[idx] <- heights[idx] + extra
                    leftover <- leftover - extra

                let mutable propIdx = 0

                while leftover > 0 && propIdx < proportionRows.Length do
                    let rowIndex, _ = proportionRows[propIdx]
                    heights[rowIndex] <- heights[rowIndex] + 1
                    leftover <- leftover - 1
                    propIdx <- propIdx + 1

            assert (heights.Length = numRows)
            heights

    /// Creates a table with specified cells and sizing.
    /// Gracefully handles ragged rows (pads with Vdom.empty) and spec mismatches (defaults to Auto for missing or
    /// invalid specs).
    /// Accepts both keyed and unkeyed cells and preserves them as-is.
    /// If you want stable focus tracking across table re-renders, provide keyed cells with meaningful keys.
    ///
    /// The keyPrefix parameter namespaces internal intermediate split keys to prevent collisions when multiple tables
    /// are rendered in the same VDOM tree. If rendering multiple tables, ensure each has a unique keyPrefix.
    let make
        (keyPrefix : NodeKey)
        (cells : Vdom<DesiredBounds, 'keyed>[][])
        (columnSpecs : ColumnSpec[])
        (rowSpecs : RowSpec[])
        : Vdom<DesiredBounds, Unkeyed>
        =
        // Normalize inputs (graceful error handling) - strips any existing keys from cells
        let cells = normalizeCells cells

        let numCols = if cells.Length = 0 then 0 else cells.GetLength 1

        let numRows = cells.GetLength 0

        let columnSpecs =
            columnSpecs
            |> Array.map sanitizeColumnSpec
            |> fun specs -> normalizeSpecs specs numCols AutoColumn

        let rowSpecs =
            rowSpecs
            |> Array.map sanitizeRowSpec
            |> fun specs -> normalizeSpecs specs numRows AutoRow

        // Cache cell measurements so render can reuse them
        let mutable cachedCellMeasurements : MeasuredSize[,] option = None

        let measure (constraints : MeasureConstraints) : MeasuredSize =
            if numRows = 0 then
                // Empty table (no rows): zero-sized
                cachedCellMeasurements <- Some (Array2D.zeroCreate 0 0)

                {
                    MinWidth = 0
                    PreferredWidth = 0
                    MaxWidth = Some 0
                    MinHeightForWidth = fun _ -> 0
                    PreferredHeightForWidth = fun _ -> 0
                    MaxHeightForWidth = fun _ -> Some 0
                }
            else if numCols = 0 then
                // Table with rows but no columns: render will return empty, so measurement should be zero
                cachedCellMeasurements <- Some (Array2D.zeroCreate numRows 0)

                {
                    MinWidth = 0
                    PreferredWidth = 0
                    MaxWidth = Some 0
                    MinHeightForWidth = fun _ -> 0
                    PreferredHeightForWidth = fun _ -> 0
                    MaxHeightForWidth = fun _ -> Some 0
                }
            else
                // Measure all cells to determine column widths
                let cellMeasurements =
                    cells
                    |> Array2D.map (fun (_vdom, keyless) ->
                        // Measure the cell using its keyless representation
                        let measured = Layout.measureEither constraints keyless
                        measured.Measured
                    )

                // Cache for render phase
                cachedCellMeasurements <- Some cellMeasurements

                // Compute minimum column widths (respecting cell MinWidth for all column types)
                let columnMinWidths =
                    [|
                        for colIdx in 0 .. numCols - 1 do
                            match Array.item colIdx columnSpecs with
                            | FixedColumn w -> w // Fixed columns have exact size
                            | AutoColumn
                            | ProportionColumn _ ->
                                // Even proportion columns must report child minima
                                let column = Array.getColumn colIdx cellMeasurements
                                column |> Array.maxOf 0 _.MinWidth
                    |]

                assert (columnMinWidths.Length = numCols)

                // Compute preferred column widths
                let columnPreferredWidths =
                    [|
                        for colIdx in 0 .. numCols - 1 do
                            match Array.item colIdx columnSpecs with
                            | FixedColumn w -> w
                            | AutoColumn
                            | ProportionColumn _ ->
                                // Include proportion column preferences so table reports accurate preferred size
                                let column = Array.getColumn colIdx cellMeasurements
                                column |> Array.maxOf 0 _.PreferredWidth
                    |]

                assert (columnPreferredWidths.Length = numCols)

                // Table's aggregate minimum and preferred width (includes all columns, even proportion)
                let tableMinWidth = min (Array.sum columnMinWidths) constraints.MaxWidth
                let tablePreferredWidth = min (Array.sum columnPreferredWidths) constraints.MaxWidth

                {
                    MinWidth = tableMinWidth
                    PreferredWidth = tablePreferredWidth
                    MaxWidth = None
                    MinHeightForWidth =
                        fun allocatedWidth ->
                            // Allocate column widths to determine how tall each row needs to be
                            let allocatedColWidths =
                                allocateColumnWidths allocatedWidth columnSpecs cellMeasurements

                            let mutable sumOfRowHeights = 0

                            for rowIdx = 0 to cellMeasurements.GetLength 0 - 1 do
                                let rowCells = Array.getRow rowIdx cellMeasurements

                                sumOfRowHeights <-
                                    sumOfRowHeights
                                    + match Array.item rowIdx rowSpecs with
                                      | FixedRow h -> h
                                      | AutoRow
                                      | ProportionRow _ ->
                                          // Even proportion rows must report child minima
                                          Array.max2Of
                                              0
                                              (fun cellMeas colWidth -> cellMeas.MinHeightForWidth colWidth)
                                              rowCells
                                              allocatedColWidths

                            min sumOfRowHeights constraints.MaxHeight
                    PreferredHeightForWidth =
                        fun allocatedWidth ->
                            let allocatedColWidths =
                                allocateColumnWidths allocatedWidth columnSpecs cellMeasurements

                            let mutable sumOfRowHeights = 0

                            for rowIdx = 0 to cellMeasurements.GetLength 0 - 1 do
                                let rowCells = Array.getRow rowIdx cellMeasurements

                                sumOfRowHeights <-
                                    sumOfRowHeights
                                    + match Array.item rowIdx rowSpecs with
                                      | FixedRow h -> h
                                      | AutoRow
                                      | ProportionRow _ ->
                                          // Proportion rows report child preferred heights
                                          Array.max2Of
                                              0
                                              (fun cellMeas colWidth -> cellMeas.PreferredHeightForWidth colWidth)
                                              rowCells
                                              allocatedColWidths

                            min sumOfRowHeights constraints.MaxHeight
                    MaxHeightForWidth = fun _ -> None
                }

        let render (bounds : Rectangle) : Vdom<DesiredBounds, Unkeyed> =
            if numRows = 0 || numCols = 0 then
                Vdom.empty
            else
                // Retrieve cached measurements from the measure phase
                let cellMeasurements =
                    match cachedCellMeasurements with
                    | Some meas -> meas
                    | None ->
                        // Fallback: re-measure if cache is empty (shouldn't happen in normal flow)
                        let maxConstraints =
                            {
                                MaxWidth = bounds.Width
                                MaxHeight = bounds.Height
                            }

                        cells
                        |> Array2D.map (fun (_vdom, keyless) ->
                            let measured = Layout.measureEither maxConstraints keyless
                            measured.Measured
                        )

                // 1. Allocate column widths from available width
                let allocatedColumnWidths =
                    allocateColumnWidths bounds.Width columnSpecs cellMeasurements

                assert (allocatedColumnWidths.Length = columnSpecs.Length)
                assert (allocatedColumnWidths.Length = numCols)

                // 2. Allocate row heights from available height
                let allocatedRowHeights =
                    allocateRowHeights bounds.Height rowSpecs cellMeasurements allocatedColumnWidths

                assert (allocatedRowHeights.Length = rowSpecs.Length)
                assert (allocatedRowHeights.Length = numRows)

                // 3. Build nested PanelSplit structure
                // Use cells as-is, preserving any user-provided keys

                // Bind empty once to use as sentinel (Vdom.empty creates fresh value each call)
                let empty = Vdom.empty

                // Each row: cell0 | cell1 | cell2 | ...
                // Build with unique keys for intermediate split accumulators
                let buildRow
                    (rowIdx : int)
                    (rowCells : (obj * KeylessVdom<DesiredBounds>)[])
                    (widths : int[])
                    : Vdom<DesiredBounds, Unkeyed>
                    =
                    assert (rowCells.Length = widths.Length)

                    match rowCells with
                    | [||] -> empty
                    | [| (cellObj, _keyless) |] ->
                        // Single cell: reserve its allocated width so it doesn't absorb extra space
                        let width = if widths.Length = 0 then 0 else widths.[0]

                        // Use absolute split so the single cell keeps its allocated width even if there's
                        // extra space beyond the sum of column widths.
                        // We need to try both keyed and unkeyed overloads
                        try
                            let cell : Vdom<DesiredBounds, Keyed> = unbox cellObj
                            Vdom.panelSplitAbsolute (SplitDirection.Vertical, width, cell, empty)
                        with _ ->
                            let cell : Vdom<DesiredBounds, Unkeyed> = unbox cellObj
                            Vdom.panelSplitAbsolute (SplitDirection.Vertical, width, cell, empty)
                    | _ ->
                        // Build right-to-left using indexed fold: cell0 | (cell1 | (cell2 | ...))
                        // Each intermediate split gets a unique key based on its column span
                        let numCols = Array.length rowCells
                        assert (numCols = widths.Length)

                        let _, _, result =
                            Array.foldBack2
                                (fun
                                    (cellObj, _keyless)
                                    (width : int)
                                    (colIdx, isFirst, accum : Vdom<DesiredBounds, Unkeyed>) ->
                                    if isFirst then
                                        // Last cell in fold (rightmost cell in row), reserve its width explicitly
                                        let splitResult =
                                            try
                                                let cell : Vdom<DesiredBounds, Keyed> = unbox cellObj
                                                Vdom.panelSplitAbsolute (SplitDirection.Vertical, width, cell, empty)
                                            with _ ->
                                                let cell : Vdom<DesiredBounds, Unkeyed> = unbox cellObj
                                                Vdom.panelSplitAbsolute (SplitDirection.Vertical, width, cell, empty)

                                        (colIdx - 1, false, splitResult)
                                    else
                                        // Split: give 'cell' exactly 'width' chars, rest goes to accumulator
                                        // Key the accumulator with a unique namespaced key indicating "columns colIdx to end of row rowIdx"
                                        let accumKeyed =
                                            Vdom.withKey
                                                (NodeKey.makeTableCellKey
                                                    keyPrefix
                                                    rowIdx
                                                    None
                                                    (Some colIdx)
                                                    (Some (numCols - 1)))
                                                accum

                                        let splitResult =
                                            try
                                                let cell : Vdom<DesiredBounds, Keyed> = unbox cellObj

                                                Vdom.panelSplitAbsolute (
                                                    SplitDirection.Vertical,
                                                    width,
                                                    cell,
                                                    accumKeyed
                                                )
                                            with _ ->
                                                let cell : Vdom<DesiredBounds, Unkeyed> = unbox cellObj

                                                Vdom.panelSplitAbsolute (
                                                    SplitDirection.Vertical,
                                                    width,
                                                    cell,
                                                    accumKeyed
                                                )

                                        (colIdx - 1, false, splitResult)
                                )
                                rowCells
                                widths
                                (numCols - 1, true, empty) // Start with last column index, isFirst=true

                        result

                let rowVdoms =
                    Array.mapiRows (fun rowIdx row -> buildRow rowIdx row allocatedColumnWidths) cells

                assert (rowVdoms.Length = numRows)

                // Stack rows vertically: row0 / row1 / row2 / ...
                match rowVdoms with
                | [||] -> empty
                | [| single |] -> single
                | _ ->
                    let numRowVdoms = Array.length rowVdoms
                    assert (numRowVdoms = numRows)

                    let _, _, result =
                        Array.foldBack2
                            (fun (row : Vdom<DesiredBounds, Unkeyed>) height (rowIdx, isFirst, accum) ->
                                if isFirst then
                                    // Last row in fold (bottommost row), reserve its height explicitly
                                    (rowIdx - 1,
                                     false,
                                     Vdom.panelSplitAbsolute (SplitDirection.Horizontal, height, row, empty))
                                else
                                    // Key both the current row and the accumulator with unique namespaced keys
                                    let rowKeyed =
                                        Vdom.withKey (NodeKey.makeTableCellKey keyPrefix rowIdx None None None) row

                                    let accumKeyed =
                                        Vdom.withKey
                                            (NodeKey.makeTableCellKey
                                                keyPrefix
                                                rowIdx
                                                (Some (numRowVdoms - 1))
                                                None
                                                None)
                                            accum

                                    (rowIdx - 1,
                                     false,
                                     Vdom.panelSplitAbsolute (SplitDirection.Horizontal, height, rowKeyed, accumKeyed))
                            )
                            rowVdoms
                            allocatedRowHeights
                            (numRowVdoms - 1, true, empty)

                    result

        Vdom.flexibleContent measure render |> Vdom.withTag "table"

    /// Creates an auto-sized table (all columns and rows size to content).
    /// Gracefully handles ragged rows (pads with Vdom.empty).
    /// Accepts both keyed and unkeyed cells and preserves them as-is.
    ///
    /// The keyPrefix parameter namespaces internal intermediate split keys to prevent collisions when multiple tables
    /// are rendered in the same VDOM tree. If rendering multiple tables, ensure each has a unique keyPrefix.
    let makeAuto<'keyed>
        (keyPrefix : NodeKey)
        (cells : Vdom<DesiredBounds, 'keyed>[][])
        : Vdom<DesiredBounds, Unkeyed>
        =
        make keyPrefix cells [||] [||]
