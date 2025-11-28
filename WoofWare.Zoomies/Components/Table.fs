namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies
open TypeEquality

/// Sizing specification for a table column
type ColumnSpec =
    /// Column width determined by widest cell content
    | AutoColumn
    /// Column has fixed width in characters
    | FixedColumn of width : int
    /// Column gets proportional share of remaining space after auto/fixed columns
    /// Proportion must be > 0
    | ProportionColumn of proportion : float

/// Sizing specification for a table row
type RowSpec =
    /// Row height determined by tallest cell (given allocated column widths)
    | AutoRow
    /// Row has fixed height in lines
    | FixedRow of height : int
    /// Row gets proportional share of remaining space after auto/fixed rows
    /// Proportion must be > 0
    | ProportionRow of proportion : float

[<RequireQualifiedAccess>]
module Table =
    /// Extract the inner UnkeyedVdom from a Vdom<'bounds, 'keyed>, discarding any existing key.
    /// This allows us to accept both keyed and unkeyed cells and assign new keys based on position.
    let private toUnkeyedVdom (vdom : Vdom<DesiredBounds, 'keyed>) : UnkeyedVdom<DesiredBounds> =
        match vdom with
        | Vdom.Keyed (KeyedVdom.WithKey (_, inner), _) -> inner
        | Vdom.Unkeyed (inner, _) -> inner

    /// Normalize cells to have uniform column count by padding short rows with Vdom.empty
    let private normalizeCells (cells : Vdom<DesiredBounds, 'keyed> list list) : UnkeyedVdom<DesiredBounds> list list =
        if List.isEmpty cells then
            []
        else
            let maxCols = cells |> List.map List.length |> List.max

            cells
            |> List.map (fun row ->
                let cellsUnkeyed = row |> List.map toUnkeyedVdom
                let padding = List.replicate (maxCols - List.length row) UnkeyedVdom.Empty
                cellsUnkeyed @ padding
            )

    /// Normalize specs to match expected count, using default for missing specs and truncating extras
    let private normalizeSpecs<'spec> (specs : 'spec list) (expectedCount : int) (defaultSpec : 'spec) : 'spec list =
        if List.length specs < expectedCount then
            specs @ List.replicate (expectedCount - List.length specs) defaultSpec
        else
            List.take expectedCount specs

    /// Sanitize proportion values to ensure they are positive
    let private sanitizeColumnSpec (spec : ColumnSpec) : ColumnSpec =
        match spec with
        | ProportionColumn p when p <= 0.0 || System.Double.IsNaN p || System.Double.IsInfinity p -> AutoColumn
        | other -> other

    let private sanitizeRowSpec (spec : RowSpec) : RowSpec =
        match spec with
        | ProportionRow p when p <= 0.0 || System.Double.IsNaN p || System.Double.IsInfinity p -> AutoRow
        | other -> other

    /// Allocate column widths from available width, respecting per-column minima
    let private allocateColumnWidths
        (availableWidth : int)
        (columnSpecs : ColumnSpec list)
        (cellMeasurements : MeasuredSize list list)
        : int list
        =
        if List.isEmpty columnSpecs then
            []
        else
            let numCols = List.length columnSpecs

            let columnInfo =
                columnSpecs
                |> List.mapi (fun colIdx spec ->
                    let cellMeasForCol = cellMeasurements |> List.map (fun row -> List.item colIdx row)

                    let minWidth =
                        match spec with
                        | FixedColumn w -> w
                        | AutoColumn
                        | ProportionColumn _ -> cellMeasForCol |> List.map (fun m -> m.MinWidth) |> List.max

                    let preferredWidth =
                        match spec with
                        | FixedColumn w -> w
                        | AutoColumn
                        | ProportionColumn _ -> cellMeasForCol |> List.map (fun m -> m.PreferredWidth) |> List.max

                    (spec, colIdx, minWidth, preferredWidth)
                )

            let totalMin = columnInfo |> List.sumBy (fun (_, _, minW, _) -> minW)

            if availableWidth <= 0 then
                List.replicate numCols 0
            elif totalMin >= availableWidth then
                // Not enough room for all minima: scale them proportionally
                let totalMinF = float totalMin

                let scaled =
                    columnInfo
                    |> List.map (fun (_, _, minW, _) -> int (float minW * float availableWidth / totalMinF))
                    |> List.toArray

                let mutable allocated = scaled |> Array.sum
                let mutable idx = 0

                while allocated < availableWidth && idx < numCols do
                    scaled[idx] <- scaled[idx] + 1
                    allocated <- allocated + 1
                    idx <- idx + 1

                scaled |> Array.toList
            else
                let widths = columnInfo |> List.map (fun (_, _, minW, _) -> minW) |> List.toArray

                let mutable remaining = availableWidth - totalMin

                // Grow auto columns toward preferred
                let autoColumns =
                    columnInfo
                    |> List.choose (fun (spec, idx, minW, prefW) ->
                        match spec with
                        | AutoColumn -> Some (idx, max 0 (prefW - minW))
                        | _ -> None
                    )

                let autoGrowthTotal = autoColumns |> List.sumBy snd

                if autoGrowthTotal > 0 && remaining > 0 then
                    let autoAllocated = min remaining autoGrowthTotal

                    for (idx, capacity) in autoColumns do
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
                    |> List.choose (fun (spec, idx, _, _) ->
                        match spec with
                        | ProportionColumn p -> Some (idx, p)
                        | _ -> None
                    )

                let proportionTotal = proportionColumns |> List.sumBy (fun (_, p) -> p)

                if proportionTotal > 0.0 && remaining > 0 then
                    let mutable leftover = remaining

                    for (idx, p) in proportionColumns do
                        let extra = int (float remaining * (p / proportionTotal))
                        widths[idx] <- widths[idx] + extra
                        leftover <- leftover - extra

                    let mutable propIdx = 0

                    while leftover > 0 && propIdx < proportionColumns.Length do
                        let colIndex, _ = proportionColumns[propIdx]
                        widths[colIndex] <- widths[colIndex] + 1
                        leftover <- leftover - 1
                        propIdx <- propIdx + 1

                widths |> Array.toList

    /// Allocate row heights from available height (similar to columns, but heights depend on column widths)
    let private allocateRowHeights
        (availableHeight : int)
        (rowSpecs : RowSpec list)
        (cellMeasurements : MeasuredSize list list)
        (columnWidths : int list)
        : int list
        =
        if List.isEmpty rowSpecs then
            []
        else
            let rowInfo =
                rowSpecs
                |> List.mapi (fun rowIdx spec ->
                    let cellsInRow = List.item rowIdx cellMeasurements

                    let minHeight =
                        match spec with
                        | FixedRow h -> h
                        | AutoRow
                        | ProportionRow _ ->
                            List.zip cellsInRow columnWidths
                            |> List.map (fun (cell, colWidth) -> cell.MinHeightForWidth colWidth)
                            |> List.max

                    let preferredHeight =
                        match spec with
                        | FixedRow h -> h
                        | AutoRow
                        | ProportionRow _ ->
                            List.zip cellsInRow columnWidths
                            |> List.map (fun (cell, colWidth) -> cell.PreferredHeightForWidth colWidth)
                            |> List.max

                    (spec, rowIdx, minHeight, preferredHeight)
                )

            let totalMin = rowInfo |> List.sumBy (fun (_, _, minH, _) -> minH)

            if availableHeight <= 0 then
                List.replicate rowSpecs.Length 0
            elif totalMin >= availableHeight then
                let totalMinF = float totalMin

                let scaled =
                    rowInfo
                    |> List.map (fun (_, _, minH, _) -> int (float minH * float availableHeight / totalMinF))
                    |> List.toArray

                let mutable allocated = scaled |> Array.sum
                let mutable idx = 0

                while allocated < availableHeight && idx < rowSpecs.Length do
                    scaled[idx] <- scaled[idx] + 1
                    allocated <- allocated + 1
                    idx <- idx + 1

                scaled |> Array.toList
            else
                let heights = rowInfo |> List.map (fun (_, _, minH, _) -> minH) |> List.toArray

                let mutable remaining = availableHeight - totalMin

                let autoRows =
                    rowInfo
                    |> List.choose (fun (spec, idx, minH, prefH) ->
                        match spec with
                        | AutoRow -> Some (idx, max 0 (prefH - minH))
                        | _ -> None
                    )

                let autoGrowthTotal = autoRows |> List.sumBy snd

                if autoGrowthTotal > 0 && remaining > 0 then
                    let autoAllocated = min remaining autoGrowthTotal

                    for (idx, capacity) in autoRows do
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
                    |> List.choose (fun (spec, idx, _, _) ->
                        match spec with
                        | ProportionRow p -> Some (idx, p)
                        | _ -> None
                    )

                let proportionTotal = proportionRows |> List.sumBy (fun (_, p) -> p)

                if proportionTotal > 0.0 && remaining > 0 then
                    let mutable leftover = remaining

                    for (idx, p) in proportionRows do
                        let extra = int (float remaining * (p / proportionTotal))
                        heights[idx] <- heights[idx] + extra
                        leftover <- leftover - extra

                    let mutable propIdx = 0

                    while leftover > 0 && propIdx < proportionRows.Length do
                        let rowIndex, _ = proportionRows[propIdx]
                        heights[rowIndex] <- heights[rowIndex] + 1
                        leftover <- leftover - 1
                        propIdx <- propIdx + 1

                heights |> Array.toList

    /// Creates a table with specified cells and sizing.
    /// Gracefully handles ragged rows (pads with Vdom.empty) and spec mismatches (defaults to Auto).
    /// Accepts both keyed and unkeyed cells - the table assigns each cell a unique key based on (row, col) position
    /// for stable focus tracking. Original keys are discarded and replaced with position-based keys.
    let make
        (cells : Vdom<DesiredBounds, 'keyed> list list)
        (columnSpecs : ColumnSpec list)
        (rowSpecs : RowSpec list)
        : Vdom<DesiredBounds, Unkeyed>
        =
        // Normalize inputs (graceful error handling) - strips any existing keys from cells
        let cells : UnkeyedVdom<DesiredBounds> list list = normalizeCells cells

        let numCols =
            if List.isEmpty cells then
                0
            else
                cells |> List.map List.length |> List.max

        let numRows = List.length cells

        let columnSpecs =
            columnSpecs
            |> List.map sanitizeColumnSpec
            |> fun specs -> normalizeSpecs specs numCols AutoColumn

        let rowSpecs =
            rowSpecs
            |> List.map sanitizeRowSpec
            |> fun specs -> normalizeSpecs specs numRows AutoRow

        // Cache cell measurements so render can reuse them
        let mutable cachedCellMeasurements : MeasuredSize list list option = None

        let measure (constraints : MeasureConstraints) : MeasuredSize =
            if List.isEmpty cells then
                // Empty table: zero-sized
                cachedCellMeasurements <- Some []

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
                let cellMeasurements : MeasuredSize list list =
                    cells
                    |> List.map (fun row ->
                        row
                        |> List.map (fun cell ->
                            // Measure the unkeyed cell directly
                            let keylessVdom = KeylessVdom.Unkeyed cell
                            let measured = Layout.measureEither constraints keylessVdom
                            measured.Measured
                        )
                    )

                // Cache for render phase
                cachedCellMeasurements <- Some cellMeasurements

                // Compute minimum column widths (respecting cell MinWidth for all column types)
                let columnMinWidths =
                    [
                        for colIdx in 0 .. numCols - 1 do
                            match List.item colIdx columnSpecs with
                            | FixedColumn w -> w // Fixed columns have exact size
                            | AutoColumn
                            | ProportionColumn _ ->
                                // Even proportion columns must report child minima
                                cellMeasurements
                                |> List.map (fun row -> (List.item colIdx row).MinWidth)
                                |> List.max
                    ]

                // Compute preferred column widths
                let columnPreferredWidths =
                    [
                        for colIdx in 0 .. numCols - 1 do
                            match List.item colIdx columnSpecs with
                            | FixedColumn w -> w
                            | AutoColumn
                            | ProportionColumn _ ->
                                // Include proportion column preferences so table reports accurate preferred size
                                cellMeasurements
                                |> List.map (fun row -> (List.item colIdx row).PreferredWidth)
                                |> List.max
                    ]

                // Table's aggregate minimum and preferred width (includes all columns, even proportion)
                let tableMinWidth = min (List.sum columnMinWidths) constraints.MaxWidth
                let tablePreferredWidth = min (List.sum columnPreferredWidths) constraints.MaxWidth

                {
                    MinWidth = tableMinWidth
                    PreferredWidth = tablePreferredWidth
                    MaxWidth = None
                    MinHeightForWidth =
                        fun allocatedWidth ->
                            // Allocate column widths to determine how tall each row needs to be
                            let allocatedColWidths =
                                allocateColumnWidths allocatedWidth columnSpecs cellMeasurements

                            let rowHeights =
                                cellMeasurements
                                |> List.mapi (fun rowIdx rowCells ->
                                    match List.item rowIdx rowSpecs with
                                    | FixedRow h -> h
                                    | AutoRow
                                    | ProportionRow _ ->
                                        // Even proportion rows must report child minima
                                        List.zip rowCells allocatedColWidths
                                        |> List.map (fun (cellMeas, colWidth) -> cellMeas.MinHeightForWidth colWidth // Use MIN height
                                        )
                                        |> List.max
                                )

                            min (List.sum rowHeights) constraints.MaxHeight
                    PreferredHeightForWidth =
                        fun allocatedWidth ->
                            let allocatedColWidths =
                                allocateColumnWidths allocatedWidth columnSpecs cellMeasurements

                            let rowHeights =
                                cellMeasurements
                                |> List.mapi (fun rowIdx rowCells ->
                                    match List.item rowIdx rowSpecs with
                                    | FixedRow h -> h
                                    | AutoRow
                                    | ProportionRow _ ->
                                        // Proportion rows report child preferred heights
                                        List.zip rowCells allocatedColWidths
                                        |> List.map (fun (cellMeas, colWidth) ->
                                            cellMeas.PreferredHeightForWidth colWidth
                                        )
                                        |> List.max
                                )

                            min (List.sum rowHeights) constraints.MaxHeight
                    MaxHeightForWidth = fun _ -> None
                }

        let render (bounds : Rectangle) : Vdom<DesiredBounds, Unkeyed> =
            if List.isEmpty cells then
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
                                MaxWidth = System.Int32.MaxValue
                                MaxHeight = System.Int32.MaxValue
                            }

                        cells
                        |> List.map (fun row ->
                            row
                            |> List.map (fun cell ->
                                let keylessVdom = KeylessVdom.Unkeyed cell
                                let measured = Layout.measureEither maxConstraints keylessVdom
                                measured.Measured
                            )
                        )
                        : MeasuredSize list list

                // 1. Allocate column widths from available width
                let allocatedColumnWidths =
                    allocateColumnWidths bounds.Width columnSpecs cellMeasurements

                // 2. Allocate row heights from available height
                let allocatedRowHeights =
                    allocateRowHeights bounds.Height rowSpecs cellMeasurements allocatedColumnWidths

                // 3. Build nested PanelSplit structure
                // First, assign unique keys to all cells based on (row, col) position
                // This ensures all cells are uniformly Keyed for panelSplitAbsolute
                let keyedCells =
                    cells
                    |> List.mapi (fun rowIdx row ->
                        row
                        |> List.mapi (fun colIdx cell ->
                            // Wrap the UnkeyedVdom with a position-based key
                            Vdom.withKey (NodeKey.make $"cell_{rowIdx}_{colIdx}") (Vdom.Unkeyed (cell, Teq.refl))
                        )
                    )

                // Bind empty once to use as sentinel (Vdom.empty creates fresh value each call)
                let empty = Vdom.empty

                // Each row: cell0 | cell1 | cell2 | ...
                // Build with unique keys for intermediate splits based on column span
                let buildRow
                    (rowIdx : int)
                    (rowCells : Vdom<DesiredBounds, Keyed> list)
                    (widths : int list)
                    : Vdom<DesiredBounds, Unkeyed>
                    =
                    match rowCells with
                    | [] -> empty
                    | [ single ] ->
                        // Single cell: reserve its allocated width so it doesn't absorb extra space
                        let width =
                            match widths with
                            | width :: _ -> width
                            | [] -> 0

                        // Use absolute split so the single cell keeps its allocated width even if there's
                        // extra space beyond the sum of column widths.
                        Vdom.panelSplitAbsolute (SplitDirection.Vertical, width, single, empty)
                    | _ ->
                        // Build right-to-left using indexed fold: cell0 | (cell1 | (cell2 | ...))
                        // Each intermediate split gets a unique key based on its column span
                        let numCols = List.length rowCells

                        let _, _, result =
                            List.foldBack2
                                (fun
                                    (cell : Vdom<DesiredBounds, Keyed>)
                                    (width : int)
                                    (colIdx, isFirst, accum : Vdom<DesiredBounds, Unkeyed>) ->
                                    if isFirst then
                                        // Last cell in fold (rightmost cell in row), reserve its width explicitly
                                        (colIdx - 1,
                                         false,
                                         Vdom.panelSplitAbsolute (SplitDirection.Vertical, width, cell, empty))
                                    else
                                        // Split: give 'cell' exactly 'width' chars, rest goes to accumulator
                                        // Key the accumulator with a unique key indicating "columns colIdx to end of row rowIdx"
                                        let accumKeyed =
                                            Vdom.withKey
                                                (NodeKey.make $"row{rowIdx}_cols{colIdx}to{numCols - 1}")
                                                accum

                                        (colIdx - 1,
                                         false,
                                         Vdom.panelSplitAbsolute (SplitDirection.Vertical, width, cell, accumKeyed))
                                )
                                rowCells
                                widths
                                (numCols - 1, true, empty) // Start with last column index, isFirst=true

                        result

                let rowVdoms =
                    List.mapi (fun rowIdx row -> buildRow rowIdx row allocatedColumnWidths) keyedCells

                // Stack rows vertically: row0 / row1 / row2 / ...
                match rowVdoms with
                | [] -> empty
                | [ single ] -> single
                | _ ->
                    let numRows = List.length rowVdoms

                    let _, _, result =
                        List.foldBack2
                            (fun row height (rowIdx, isFirst, accum) ->
                                if isFirst then
                                    (rowIdx - 1, false, row)
                                else
                                    // Key both the current row and the accumulator with unique keys
                                    let rowKeyed = Vdom.withKey (NodeKey.make $"row{rowIdx}") row

                                    let accumKeyed = Vdom.withKey (NodeKey.make $"rows{rowIdx}to{numRows - 1}") accum

                                    (rowIdx - 1,
                                     false,
                                     Vdom.panelSplitAbsolute (SplitDirection.Horizontal, height, rowKeyed, accumKeyed))
                            )
                            rowVdoms
                            allocatedRowHeights
                            (numRows - 1, true, empty)

                    result

        Vdom.flexibleContent measure render |> Vdom.withTag "table"

    /// Creates an auto-sized table (all columns and rows size to content).
    /// Gracefully handles ragged rows (pads with Vdom.empty).
    /// Accepts both keyed and unkeyed cells - the table assigns position-based keys internally.
    let makeAuto (cells : Vdom<DesiredBounds, 'keyed> list list) : Vdom<DesiredBounds, Unkeyed> = make cells [] []
