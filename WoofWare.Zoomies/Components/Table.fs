namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

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
    /// Normalize cells to have uniform column count by padding short rows with Vdom.empty
    let private normalizeCells
        (cells : Vdom<DesiredBounds, Unkeyed> list list)
        : Vdom<DesiredBounds, Unkeyed> list list
        =
        if List.isEmpty cells then
            []
        else
            let maxCols = cells |> List.map List.length |> List.max

            cells
            |> List.map (fun row ->
                let padding = List.replicate (maxCols - List.length row) Vdom.empty
                row @ padding
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

    /// Allocate column widths from available width
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

            // Compute preferred width for each column
            let columnPreferredWidths =
                columnSpecs
                |> List.mapi (fun colIdx spec ->
                    match spec with
                    | FixedColumn w -> (spec, w, true) // (spec, width, isFixed)
                    | AutoColumn ->
                        let preferredWidth =
                            cellMeasurements
                            |> List.map (fun row -> (List.item colIdx row).PreferredWidth)
                            |> List.max

                        (spec, preferredWidth, false)
                    | ProportionColumn _ -> (spec, 0, false) // Defer
                )

            // Separate into fixed, auto, and proportion
            let fixedCols =
                columnPreferredWidths
                |> List.filter (fun (spec, _, _) ->
                    match spec with
                    | FixedColumn _ -> true
                    | _ -> false
                )

            let autoCols =
                columnPreferredWidths
                |> List.filter (fun (spec, _, _) ->
                    match spec with
                    | AutoColumn -> true
                    | _ -> false
                )

            let proportionCols =
                columnPreferredWidths
                |> List.filter (fun (spec, _, _) ->
                    match spec with
                    | ProportionColumn _ -> true
                    | _ -> false
                )

            let fixedTotal = fixedCols |> List.sumBy (fun (_, w, _) -> w)
            let autoTotal = autoCols |> List.sumBy (fun (_, w, _) -> w)

            // Check if we're over-constrained
            if fixedTotal + autoTotal > availableWidth then
                // Shrink-to-fit: scale down auto columns (fixed columns stay fixed)
                let spaceForAuto = max 0 (availableWidth - fixedTotal)

                let autoScaleFactor =
                    if autoTotal > 0 then
                        float spaceForAuto / float autoTotal
                    else
                        0.0

                columnPreferredWidths
                |> List.map (fun (spec, width, _) ->
                    match spec with
                    | FixedColumn _ -> width
                    | AutoColumn -> int (float width * autoScaleFactor)
                    | ProportionColumn _ -> 0 // No space left for proportions
                )
            else
                // We have space for fixed + auto + proportions
                let remainingWidth = availableWidth - fixedTotal - autoTotal

                let proportionTotal =
                    proportionCols
                    |> List.sumBy (fun (spec, _, _) ->
                        match spec with
                        | ProportionColumn p -> p
                        | _ -> 0.0
                    )

                columnPreferredWidths
                |> List.map (fun (spec, width, _) ->
                    match spec with
                    | FixedColumn _ -> width
                    | AutoColumn -> width
                    | ProportionColumn p ->
                        if proportionTotal > 0.0 then
                            int (float remainingWidth * p / proportionTotal)
                        else
                            remainingWidth / List.length proportionCols // Divide equally
                )

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
            // For auto rows, compute how tall each cell needs to be given the allocated column widths
            let rowPreferredHeights =
                rowSpecs
                |> List.mapi (fun rowIdx spec ->
                    match spec with
                    | FixedRow h -> (spec, h, true)
                    | AutoRow ->
                        // Compute max preferred height across all cells in this row
                        let preferredHeight =
                            List.zip (List.item rowIdx cellMeasurements) columnWidths
                            |> List.map (fun (cellMeas, colWidth) -> cellMeas.PreferredHeightForWidth colWidth)
                            |> List.max

                        (spec, preferredHeight, false)
                    | ProportionRow _ -> (spec, 0, false)
                )

            let fixedRows =
                rowPreferredHeights
                |> List.filter (fun (spec, _, _) ->
                    match spec with
                    | FixedRow _ -> true
                    | _ -> false
                )

            let autoRows =
                rowPreferredHeights
                |> List.filter (fun (spec, _, _) ->
                    match spec with
                    | AutoRow -> true
                    | _ -> false
                )

            let proportionRows =
                rowPreferredHeights
                |> List.filter (fun (spec, _, _) ->
                    match spec with
                    | ProportionRow _ -> true
                    | _ -> false
                )

            let fixedTotal = fixedRows |> List.sumBy (fun (_, h, _) -> h)
            let autoTotal = autoRows |> List.sumBy (fun (_, h, _) -> h)

            // Check if we're over-constrained
            if fixedTotal + autoTotal > availableHeight then
                // Shrink-to-fit: scale down auto rows
                let spaceForAuto = max 0 (availableHeight - fixedTotal)

                let autoScaleFactor =
                    if autoTotal > 0 then
                        float spaceForAuto / float autoTotal
                    else
                        0.0

                rowPreferredHeights
                |> List.map (fun (spec, height, _) ->
                    match spec with
                    | FixedRow _ -> height
                    | AutoRow -> max 1 (int (float height * autoScaleFactor)) // At least 1 line
                    | ProportionRow _ -> 0
                )
            else
                // We have space for all
                let remainingHeight = availableHeight - fixedTotal - autoTotal

                let proportionTotal =
                    proportionRows
                    |> List.sumBy (fun (spec, _, _) ->
                        match spec with
                        | ProportionRow p -> p
                        | _ -> 0.0
                    )

                rowPreferredHeights
                |> List.map (fun (spec, height, _) ->
                    match spec with
                    | FixedRow _ -> height
                    | AutoRow -> height
                    | ProportionRow p ->
                        if proportionTotal > 0.0 then
                            int (float remainingHeight * p / proportionTotal)
                        else
                            remainingHeight / List.length proportionRows
                )

    /// Creates a table with specified cells and sizing.
    /// Gracefully handles ragged rows (pads with Vdom.empty) and spec mismatches (defaults to Auto).
    /// Accepts both keyed and unkeyed cells - the table assigns each cell a unique key based on (row, col) position
    /// for stable focus tracking. Original keys are preserved within each cell.
    let make
        (cells : Vdom<DesiredBounds, Unkeyed> list list)
        (columnSpecs : ColumnSpec list)
        (rowSpecs : RowSpec list)
        : Vdom<DesiredBounds, Unkeyed>
        =
        // Normalize inputs (graceful error handling)
        let cells = normalizeCells cells

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
                let cellMeasurements =
                    cells
                    |> List.map (fun row -> row |> List.map (fun cell -> Layout.measure cell constraints))

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
                        |> List.map (fun row -> row |> List.map (fun cell -> Layout.measure cell maxConstraints))

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
                        |> List.mapi (fun colIdx cell -> Vdom.withKey (NodeKey.make $"cell_{rowIdx}_{colIdx}") cell)
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
                        // Single cell: split with empty to convert Keyed to Unkeyed
                        Vdom.panelSplitAbsolute (SplitDirection.Vertical, 0, empty, single)
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
                                        // Last cell in fold (first cell in row), split with empty to get Unkeyed
                                        (colIdx - 1,
                                         false,
                                         Vdom.panelSplitAbsolute (SplitDirection.Vertical, 0, empty, cell))
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
    /// Accepts unkeyed cells - the table assigns position-based keys internally.
    let makeAuto (cells : Vdom<DesiredBounds, Unkeyed> list list) : Vdom<DesiredBounds, Unkeyed> = make cells [] []
