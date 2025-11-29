# Table Layout Component - Implementation Plan

## Document Status

**Updated**: This document has been revised to address review feedback. Key changes:

### First Review (Initial Fixes)

1. **Fixed critical FlexibleContent limitation**: Clarified that Approach 2 requires exposing a `Vdom.measure` helper, as `FlexibleContent` doesn't currently provide child measurements to the measure callback (see "The Measurement Problem" section).

2. **Resolved internal inconsistency**: The document now consistently recommends Option A (expose `Vdom.measure` helper) throughout.

3. **Demoted new-primitive approaches**: Approaches 1 and 3 are now clearly marked as "LAST RESORT ONLY" options that conflict with the framework's small-primitive philosophy.

4. **Fixed empty-row safety**: Replaced `List.reduce` with `List.foldBack2` to handle empty rows gracefully.

5. **Answered open questions**: Added explicit answers about using `Vdom.measure` helper and demoting new-primitive options.

### Second Review (Data Flow & Constraints)

6. **Fixed data flow in render**: Added mutable cache `cachedCellMeasurements` to share measurements from measure phase to render phase (lines 385-481). Removed all placeholder values.

7. **Fixed size constraints**: `MinWidth` now derives from actual cell minima (sum of column mins) rather than hard-coded `numCols * 1`. `MinHeightForWidth` uses cells' `MinHeightForWidth` rather than preferred heights (lines 412-461).

8. **Fixed type signatures**: Updated `allocateColumnWidths` and `allocateRowHeights` to take `MeasuredSize list list` instead of `int list list`, using actual cell measurements (lines 551-642).

9. **Removed all placeholders**: Render phase now uses cached measurements to compute real column widths and row heights, honoring child constraints and providing low-level control.

### Third Review (Keyedness, Proportion Constraints, Empty Sentinel)

10. **Fixed keyedness handling**: Table now accepts both keyed and unkeyed cells (polymorphic `'keyed`) and preserves them as-is. User-provided keys are retained; unkeyed cells remain unkeyed. The table does not force keys on cells.

11. **Fixed proportion column/row constraints**: Proportion columns and rows now report child minima and preferred sizes rather than 0/1, preventing parent Auto splits from collapsing tables with proportion-only specs (lines 423-449, 462-485).

12. **Fixed empty sentinel**: Replaced `Object.referenceEquals accum Vdom.empty` (which never matched because `Vdom.empty` creates fresh values) with an explicit `isFirst` flag in the fold accumulator (lines 530-573).

13. **Clarified Vdom.measure prerequisite**: The document now consistently notes that `Vdom.measure` is a required framework addition for Approach 2 (see "The Measurement Problem" section and Phase 1 of Implementation Plan).

### Fourth Review (Key Uniqueness)

14. **Fixed key uniqueness**: Intermediate split nodes now use `NodeKey.makeTableCellKey` with a `keyPrefix` parameter to ensure keys are unique within the tree and namespaced to prevent collisions when multiple tables are rendered (lines 520-594):
    - Intermediate column splits: Keys representing the span of columns in the accumulator
    - Intermediate row splits: Keys representing the span of rows in the accumulator
    - Cells are NOT keyed by the table; user-provided keys are preserved as-is

## Overview

This document outlines a design for implementing a table layout component in WoofWare.Zoomies. A table requires synchronized sizing across multiple axes: all cells in a row must have the same height, and all cells in a column must have the same width. This is fundamentally different from the current `PanelSplit` primitive, which only handles binary splits and doesn't support cross-axis alignment.

## Current System Limitations

### PanelSplit Constraints

The current layout system is built around `PanelSplit`, which:
- Only handles exactly 2 children
- Distributes space along a single axis (Vertical or Horizontal)
- Has no mechanism for cross-axis synchronization
- Treats each split independently

Building an N-column table with `PanelSplit` requires deeply nested structures:
```fsharp
// 4-column row:
PanelSplit(col1, PanelSplit(col2, PanelSplit(col3, col4)))
```

### The Cross-Axis Synchronization Problem

The fundamental challenge: **measuring cells in isolation doesn't give you table-wide constraints**.

In a 3×2 table:
```
┌─────┬──────────┬──────┐
│ A1  │ B1       │ C1   │
├─────┼──────────┼──────┤
│ A2  │ B2       │ C2   │
└─────┴──────────┴──────┘
```

Requirements:
- Column A width = max(A1.width, A2.width)
- Column B width = max(B1.width, B2.width)
- Column C width = max(C1.width, C2.width)
- Row 1 height = max(A1.height, B1.height, C1.height)
- Row 2 height = max(A2.height, B2.height, C2.height)

The current `Auto` split behavior can't express this because:
1. Each split only sees its two children
2. There's no way to propagate constraints across splits
3. Height-for-width calculations are local to each node

## Design Approaches

### Approach 1: New Vdom Primitive - `Table` (LAST RESORT ONLY)

**⚠️ This approach conflicts with the framework's small-primitive philosophy and should only be considered if the FlexibleContent-based approach (Approach 2) proves to have unacceptable performance after implementation and measurement.**

Add a first-class table primitive to the core VDOM:

```fsharp
type UnkeyedVdom<'bounds> =
    | ...
    | Table of
        rows : KeylessVdom<'bounds> list list * // row-major: list of rows, each row is list of cells
        sizing : TableSizing

type TableSizing =
    | Auto  // Size columns/rows based on content
    | Fixed of columnWidths : int list * rowHeights : int list
    | Mixed of columnSpecs : ColumnSpec list * rowSpecs : RowSpec list

type ColumnSpec =
    | AutoColumn
    | FixedColumn of width : int
    | ProportionColumn of proportion : float  // Share of available space

type RowSpec =
    | AutoRow
    | FixedRow of height : int
    | ProportionRow of proportion : float
```

**Measure Phase:**
1. Collect all cells' measurements
2. For each column, compute: `colWidth[i] = max(cells[*][i].PreferredWidth)`
3. For each row, given allocated column widths, compute: `rowHeight[j] = max(cells[j][*].PreferredHeightForWidth(colWidth[i]))`
4. Table's measurements aggregate these

**Arrange Phase:**
1. Allocate space to columns (respecting Fixed/Proportion/Auto specs)
2. For each row, allocate height based on content measured at allocated column widths
3. Position each cell at its calculated grid location

**Advantages:**
- Clean API matching table semantics
- Efficient: single pass through all cells
- Framework can optimize rendering (e.g., only redraw changed cells)
- Keying can be per-cell for fine-grained focus tracking

**Disadvantages:**
- **Violates the small-primitive philosophy**: The architecture docs state panel-split is the sole layout primitive
- Adds complexity to core VDOM type
- Another layout algorithm to maintain in the framework core
- Prevents demonstrating that existing primitives are sufficient

**Implementation Estimate:**
- New measurement logic in `Layout.fs` (~200 lines)
- New arrangement logic in `Layout.fs` (~150 lines)
- Vdom API surface (~100 lines)
- Tests (~300 lines)
- Total: ~750 lines

### Approach 2: FlexibleContent-Based Table Component

Build a table as a reusable component using `FlexibleContent`:

```fsharp
module Table =
    type Cell<'keyed> = Vdom<DesiredBounds, 'keyed>

    type Options =
        {
            ColumnWidths : ColumnSpec list option  // None = all auto
            RowHeights : RowSpec list option       // None = all auto
            ShowBorders : bool
        }

    let make (options : Options) (cells : Cell<Keyed> list list) : Vdom<DesiredBounds, Unkeyed>
```

**Measure Function:**
1. Measure each cell to get all `MeasuredSize`s
2. Compute column widths:
   - Auto columns: take max `PreferredWidth` of cells in that column
   - Fixed columns: use specified width
   - Proportion columns: defer to arrange phase
3. Compute row heights (knowing column widths):
   - Auto rows: for each cell in row, get `PreferredHeightForWidth(allocatedColWidth)`, take max
   - Fixed rows: use specified height
   - Proportion rows: defer to arrange phase
4. Return table's aggregate measurements

**Render Function:**
1. Re-measure all cells (can cache from measure phase)
2. Allocate column widths from available `Rectangle.Width`
3. Allocate row heights from available `Rectangle.Height`
4. For each cell at (row, col):
   - Calculate its bounds: `Rectangle { TopLeftX = x_offset, TopLeftY = y_offset, Width = colWidth[col], Height = rowHeight[row] }`
   - Recursively arrange the cell's VDOM with those bounds
5. Stitch cells together using nested `PanelSplit`s

**Implementation Strategy:**

The render function would build nested splits like:
```fsharp
let renderRow (cellVdoms : Vdom list) (widths : int list) : Vdom =
    // Build: cell0 | cell1 | cell2 | ...
    List.fold2 (fun accum cell width ->
        Vdom.panelSplitAbsolute(Vertical, width, cell, accum)
    ) Vdom.empty cellVdoms widths

let renderTable (rows : Vdom list list) (widths : int list) (heights : int list) : Vdom =
    let rowVdoms = List.map2 (fun cells height -> renderRow cells widths) rows heights
    // Stack rows vertically
    List.fold2 (fun accum row height ->
        Vdom.panelSplitAbsolute(Horizontal, height, row, accum)
    ) Vdom.empty rowVdoms heights
```

**Advantages:**
- No changes to core VDOM
- Pure library code in `Components/` directory
- Demonstrates power of `FlexibleContent`
- Easy to experiment and iterate

**Disadvantages:**
- Nested `PanelSplit`s may be less efficient than a dedicated primitive
- More complex implementation (manage measurement cache, coordinate phases)
- Potentially fragile (if measure/render get out of sync)
- Deep nesting could impact performance for large tables

**Implementation Estimate:**
- Component implementation (~400 lines)
- Helper functions for measurement/arrangement (~200 lines)
- API and options (~100 lines)
- Tests (~300 lines)
- Total: ~1000 lines

### Approach 3: Hybrid - New Layout Algorithm, No New Primitive (LAST RESORT ONLY)

**⚠️ This approach still adds a new primitive (LayoutGroup) to the core VDOM, conflicting with the small-primitive philosophy. Like Approach 1, this should only be considered if Approach 2 proves inadequate after implementation and measurement.**

Extend the layout system to support "layout groups" without adding a new VDOM node type.

Add a new VDOM node:
```fsharp
type UnkeyedVdom<'bounds> =
    | ...
    | LayoutGroup of
        layouter : ILayouter *
        children : KeylessVdom<'bounds> list

type ILayouter =
    abstract Measure : MeasuredSize list -> MeasureConstraints -> MeasuredSize
    abstract Arrange : MeasuredNode list -> Rectangle -> (KeylessVdom<Rectangle> * Rectangle) list
```

A table would be one implementation of `ILayouter`:

```fsharp
type TableLayouter(numRows : int, numCols : int, options : TableOptions) =
    interface ILayouter with
        member _.Measure(childMeasurements, constraints) =
            // Extract cells as [numRows][numCols] grid from flat list
            // Compute column widths and row heights
            // Return table's aggregate measurement

        member _.Arrange(measuredNodes, bounds) =
            // Allocate columns and rows
            // Return list of (arranged child VDOM, child bounds) pairs
```

**Advantages:**
- Extensible system for custom layouts beyond tables (grids, flexbox-like, etc.)
- Keeps core VDOM type smaller than Approach 1
- Table logic is encapsulated

**Disadvantages:**
- **Still adds a new primitive (LayoutGroup) to core**: Conflicts with panel-split-only architecture
- Interface-based design less idiomatic in F#
- More abstraction layers
- Harder to pattern-match and analyze VDOM structure
- Adds complexity to core layout system

**Implementation Estimate:**
- Layout group infrastructure (~300 lines)
- Table layouter (~350 lines)
- API surface (~100 lines)
- Tests (~350 lines)
- Total: ~1100 lines

## Recommendation: Approach 2 (FlexibleContent-Based Component)

**Choose Approach 2** - build table as a FlexibleContent-based component in `Components/Table.fs`.

### Rationale

1. **Preserves framework philosophy**: Follows the stated goal of "small set of powerful primitives" with "higher-level ergonomic libraries" built on top
2. **Demonstrates FlexibleContent power**: Shows that the existing primitives are sufficient for complex layouts
3. **Easier iteration**: Can experiment and refine without touching core layout code
4. **Consistent with existing patterns**: `Button` and `ProgressBar` live in `Components/`; table should follow the same pattern
5. **Path to optimization**: Can measure performance first, then add a primitive if needed (but likely won't be)

### Philosophy Alignment

The CLAUDE.md states:
> "The intended philosophy of the project is that from a small set of powerful and coherent primitives, it should be possible to build a number of higher-level ergonomic libraries."

FlexibleContent exists precisely for this use case - building complex, content-aware layouts without expanding the core VDOM. Table is a perfect example of a higher-level ergonomic library built on the existing primitives.

This follows precedent: `Button` is an interactive component with state management yet lives in `Components/`. Table is simpler (pure layout, no interaction) and should follow the same pattern.

## Detailed Design: Table Component

### Component Types

```fsharp
// WoofWare.Zoomies/Components/Table.fs
namespace WoofWare.Zoomies.Components

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
```

### API Surface

```fsharp
[<RequireQualifiedAccess>]
module Table =
    /// Creates a table with specified cells and sizing.
    /// Gracefully handles ragged rows (pads with Vdom.empty) and spec mismatches (defaults to Auto).
    /// Accepts both keyed and unkeyed cells and preserves them as-is.
    /// If you want stable focus tracking across table re-renders, provide keyed cells with meaningful keys.
    let make
        (cells : Vdom<DesiredBounds, 'keyed> list list)
        (columnSpecs : ColumnSpec list)
        (rowSpecs : RowSpec list)
        : Vdom<DesiredBounds, Unkeyed>

    /// Creates an auto-sized table (all columns and rows size to content).
    /// Gracefully handles ragged rows (pads with Vdom.empty).
    /// Accepts both keyed and unkeyed cells and preserves them as-is.
    let makeAuto
        (cells : Vdom<DesiredBounds, 'keyed> list list)
        : Vdom<DesiredBounds, Unkeyed>
```

As a performance optimization, we may choose to use jagged arrays (`[][]`) instead, and throughout the implementation.

### The Measurement Problem

**CRITICAL PREREQUISITE**: Approach 2 requires adding a `Vdom.measure` helper to the framework core.

The component needs to measure child cells to compute column widths and row heights. However, **the current `FlexibleContent` implementation does not provide child measurements to the measure callback**. Looking at `Layout.fs:647-679`:

```fsharp
| UnkeyedVdom.FlexibleContent (measure, _) ->
    let measured = measure constraints
    // ... validation ...
    {
        Vdom = KeylessVdom.Unkeyed vdom
        Measured = clampedMeasured
        Children = [] // No children yet - we don't know what they are until arrange
    }
```

The `Children` are empty during the measure phase. The `render` callback is only invoked during `arrange`, which happens after measurement is complete. This means **the table component cannot obtain real child measurements to compute column widths and row heights**.

To make Approach 2 work, we need one of these solutions:

**Option A: Expose a public measurement helper**

Add to `Vdom.fs`:
```fsharp
type Vdom =
    /// Get the measured size of a VDOM node given constraints.
    /// Useful for components that need to inspect child measurements.
    static member measure
        (vdom : Vdom<DesiredBounds, 'keyed>)
        (constraints : MeasureConstraints)
        : MeasuredSize
```

This exposes the internal `Layout.measureEither` function as a public API. The table's measure callback can then measure each cell individually.

**Option B: Extend FlexibleContent to feed render output back into measure**

Modify `FlexibleContent` so the measure callback can optionally return a VDOM structure, which the framework measures and provides back to the component. This is more complex and requires changes to the FlexibleContent API.

**Option C: Build the complete structure in measure and rely on framework measurement**

The measure callback builds and returns the complete nested split structure. The framework measures it recursively. The render callback rebuilds the same structure. This works but is inefficient (builds the structure twice) and doesn't give the component direct access to measurements for computing column widths.

**Recommendation**: **Option A** is the cleanest solution. It's a small API addition (~20 lines) that doesn't change FlexibleContent's semantics, and it enables table (and future components) to make informed layout decisions based on child measurements.

**This prerequisite keeps the table as a library component**: Unlike Approaches 1 and 3 which add new VDOM primitives, Option A just exposes an existing internal function as a public API, maintaining the table as composable library code similar to Components/Button.

### Measurement Algorithm (FlexibleContent measure callback)

**Assuming Option A (Vdom.measure helper) is implemented**, the measure callback can measure each cell individually and cache the results for the render phase:

```fsharp
let make (cells : Vdom<DesiredBounds, 'keyed> list list) columnSpecs rowSpecs =
    // Normalize inputs (graceful error handling)
    let cells = normalizeCells cells  // Pad ragged rows with Vdom.empty
    let numCols = if List.isEmpty cells then 0 else cells |> List.map List.length |> List.max
    let numRows = List.length cells
    let columnSpecs = normalizeSpecs columnSpecs numCols AutoColumn
    let rowSpecs = normalizeSpecs rowSpecs numRows AutoRow

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
                cells |> List.map (fun row ->
                    row |> List.map (fun cell ->
                        Vdom.measure cell constraints  // Uses the new Vdom.measure helper (Option A)
                    )
                )

            // Cache for render phase
            cachedCellMeasurements <- Some cellMeasurements

            // Compute minimum column widths (respecting cell MinWidth for all column types)
            let columnMinWidths =
                [ for colIdx in 0 .. numCols - 1 do
                    match columnSpecs.[colIdx] with
                    | FixedColumn w -> w  // Fixed columns have exact size
                    | AutoColumn | ProportionColumn _ ->
                        // Even proportion columns must report child minima
                        cellMeasurements
                        |> List.map (fun row -> row.[colIdx].MinWidth)
                        |> List.max
                ]

            // Compute preferred column widths
            let columnPreferredWidths =
                [ for colIdx in 0 .. numCols - 1 do
                    match columnSpecs.[colIdx] with
                    | FixedColumn w -> w
                    | AutoColumn | ProportionColumn _ ->
                        // Include proportion column preferences so table reports accurate preferred size
                        cellMeasurements
                        |> List.map (fun row -> row.[colIdx].PreferredWidth)
                        |> List.max
                ]

            // Table's aggregate minimum and preferred width (includes all columns, even proportion)
            let tableMinWidth = min (List.sum columnMinWidths) constraints.MaxWidth
            let tablePreferredWidth = min (List.sum columnPreferredWidths) constraints.MaxWidth

            {
                MinWidth = tableMinWidth
                PreferredWidth = tablePreferredWidth
                MaxWidth = None
                MinHeightForWidth = fun allocatedWidth ->
                    // Allocate column widths to determine how tall each row needs to be
                    let allocatedColWidths = allocateColumnWidths allocatedWidth columnSpecs cellMeasurements
                    let rowHeights =
                        cellMeasurements |> List.mapi (fun rowIdx rowCells ->
                            match rowSpecs.[rowIdx] with
                            | FixedRow h -> h
                            | AutoRow | ProportionRow _ ->
                                // Even proportion rows must report child minima
                                List.zip rowCells allocatedColWidths
                                |> List.map (fun (cellMeas, colWidth) ->
                                    cellMeas.MinHeightForWidth colWidth  // Use MIN height
                                )
                                |> List.max
                        )
                    min (List.sum rowHeights) constraints.MaxHeight
                PreferredHeightForWidth = fun allocatedWidth ->
                    let allocatedColWidths = allocateColumnWidths allocatedWidth columnSpecs cellMeasurements
                    let rowHeights =
                        cellMeasurements |> List.mapi (fun rowIdx rowCells ->
                            match rowSpecs.[rowIdx] with
                            | FixedRow h -> h
                            | AutoRow | ProportionRow _ ->
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
```

**Note**: This approach requires implementing Option A (`Vdom.measure` helper). The measurements are cached in `cachedCellMeasurements` so the render phase can reuse them without re-measuring.

### Arrangement Algorithm (FlexibleContent render callback)

The render callback builds the nested split structure with calculated column widths and row heights:

```fsharp
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
                    let maxConstraints = { MaxWidth = Int32.MaxValue; MaxHeight = Int32.MaxValue }
                    cells |> List.map (fun row ->
                        row |> List.map (fun cell -> Vdom.measure cell maxConstraints)
                    )

            // 1. Allocate column widths from available width
            let allocatedColumnWidths =
                allocateColumnWidths bounds.Width columnSpecs cellMeasurements

            // 2. Allocate row heights from available height
            let allocatedRowHeights =
                allocateRowHeights bounds.Height rowSpecs cellMeasurements allocatedColumnWidths

            // 3. Build nested PanelSplit structure
            // Use cells as-is, preserving any user-provided keys

            // Bind empty once to use as sentinel (Vdom.empty creates fresh value each call)
            let empty = Vdom.empty

            // Each row: cell0 | cell1 | cell2 | ...
            // Build with unique keys for intermediate split accumulators
            let buildRow (rowIdx : int) (rowCells : Vdom<DesiredBounds, 'keyed> list) (widths : int list) : Vdom<DesiredBounds, Unkeyed> =
                match rowCells with
                | [] -> empty
                | [ single ] ->
                    // Single cell: reserve its allocated width so it doesn't absorb extra space
                    let width = if widths.Length = 0 then 0 else widths.[0]
                    Vdom.panelSplitAbsolute (SplitDirection.Vertical, width, single, empty)
                | _ ->
                    // Build right-to-left using indexed fold: cell0 | (cell1 | (cell2 | ...))
                    // Each intermediate accumulator gets a unique key
                    let numCols = List.length rowCells
                    let _, _, result =
                        List.foldBack2
                            (fun cell width (colIdx, isFirst, accum) ->
                                if isFirst then
                                    // Last cell in fold (rightmost cell in row), reserve its width explicitly
                                    (colIdx - 1, false, Vdom.panelSplitAbsolute (SplitDirection.Vertical, width, cell, empty))
                                else
                                    // Split: give 'cell' exactly 'width' chars, rest goes to accumulator
                                    // Key the accumulator to prevent collisions in the VDOM tree
                                    let accumKeyed = Vdom.withKey (NodeKey.make $"row{rowIdx}_cols{colIdx}to{numCols - 1}") accum
                                    (colIdx - 1, false, Vdom.panelSplitAbsolute(SplitDirection.Vertical, width, cell, accumKeyed))
                            )
                            rowCells
                            widths
                            (numCols - 1, true, empty)  // Start with last column index, isFirst=true
                    result

            let rowVdoms = List.mapi (fun rowIdx row ->
                buildRow rowIdx row allocatedColumnWidths
            ) cells

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
                                (rowIdx - 1, false, Vdom.panelSplitAbsolute(SplitDirection.Horizontal, height, rowKeyed, accumKeyed))
                        )
                        rowVdoms
                        allocatedRowHeights
                        (numRows - 1, true, empty)
                result

    Vdom.flexibleContent measure render
    |> Vdom.withTag "table"

// Helper: allocate column widths with shrink-to-fit
let private allocateColumnWidths
    (availableWidth : int)
    (columnSpecs : ColumnSpec list)
    (cellMeasurements : MeasuredSize list list)  // Row-major: measurements[rowIdx][colIdx]
    : int list =

    if List.isEmpty columnSpecs then
        []
    else
        // Compute preferred width for each column
        let columnPreferredWidths =
            columnSpecs |> List.mapi (fun colIdx spec ->
                match spec with
                | FixedColumn w -> (spec, w, true)  // (spec, width, isFixed)
                | AutoColumn ->
                    let preferredWidth =
                        cellMeasurements
                        |> List.map (fun row -> row.[colIdx].PreferredWidth)
                        |> List.max
                    (spec, preferredWidth, false)
                | ProportionColumn p -> (spec, 0, false)  // Defer
            )

        // Separate into fixed, auto, and proportion
        let fixedCols = columnPreferredWidths |> List.filter (fun (spec, _, _) -> match spec with FixedColumn _ -> true | _ -> false)
        let autoCols = columnPreferredWidths |> List.filter (fun (spec, _, _) -> match spec with AutoColumn -> true | _ -> false)
        let proportionCols = columnPreferredWidths |> List.filter (fun (spec, _, _) -> match spec with ProportionColumn _ -> true | _ -> false)

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

            columnPreferredWidths |> List.map (fun (spec, width, _) ->
                match spec with
                | FixedColumn _ -> width
                | AutoColumn -> int (float width * autoScaleFactor)
                | ProportionColumn _ -> 0  // No space left for proportions
            )
        else
            // We have space for fixed + auto + proportions
            let remainingWidth = availableWidth - fixedTotal - autoTotal
            let proportionTotal =
                proportionCols
                |> List.sumBy (fun (ProportionColumn p, _, _) -> p | _ -> 0.0)

            columnPreferredWidths |> List.map (fun (spec, width, _) ->
                match spec with
                | FixedColumn _ -> width
                | AutoColumn -> width
                | ProportionColumn p ->
                    if proportionTotal > 0.0 then
                        int (float remainingWidth * p / proportionTotal)
                    else
                        remainingWidth / List.length proportionCols  // Divide equally
            )

// Helper: allocate row heights (similar to columns, but heights depend on column widths)
let private allocateRowHeights
    (availableHeight : int)
    (rowSpecs : RowSpec list)
    (cellMeasurements : MeasuredSize list list)  // Row-major: measurements[rowIdx][colIdx]
    (columnWidths : int list)  // Already allocated column widths
    : int list =

    if List.isEmpty rowSpecs then
        []
    else
        // For auto rows, compute how tall each cell needs to be given the allocated column widths
        let rowPreferredHeights =
            rowSpecs |> List.mapi (fun rowIdx spec ->
                match spec with
                | FixedRow h -> (spec, h, true)
                | AutoRow ->
                    // Compute max preferred height across all cells in this row
                    let preferredHeight =
                        List.zip cellMeasurements.[rowIdx] columnWidths
                        |> List.map (fun (cellMeas, colWidth) ->
                            cellMeas.PreferredHeightForWidth colWidth
                        )
                        |> List.max
                    (spec, preferredHeight, false)
                | ProportionRow p -> (spec, 0, false)
            )

        let fixedRows = rowPreferredHeights |> List.filter (fun (spec, _, _) -> match spec with FixedRow _ -> true | _ -> false)
        let autoRows = rowPreferredHeights |> List.filter (fun (spec, _, _) -> match spec with AutoRow -> true | _ -> false)
        let proportionRows = rowPreferredHeights |> List.filter (fun (spec, _, _) -> match spec with ProportionRow _ -> true | _ -> false)

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

            rowPreferredHeights |> List.map (fun (spec, height, _) ->
                match spec with
                | FixedRow _ -> height
                | AutoRow -> max 1 (int (float height * autoScaleFactor))  // At least 1 line
                | ProportionRow _ -> 0
            )
        else
            // We have space for all
            let remainingHeight = availableHeight - fixedTotal - autoTotal
            let proportionTotal =
                proportionRows
                |> List.sumBy (fun (ProportionRow p, _, _) -> p | _ -> 0.0)

            rowPreferredHeights |> List.map (fun (spec, height, _) ->
                match spec with
                | FixedRow _ -> height
                | AutoRow -> height
                | ProportionRow p ->
                    if proportionTotal > 0.0 then
                        int (float remainingHeight * p / proportionTotal)
                    else
                        remainingHeight / List.length proportionRows
            )
```

### Edge Cases and Graceful Handling

The component never throws exceptions for bad input. Instead it applies sensible defaults:

1. **Empty table**: `cells = []` → Render `Vdom.empty` (zero-sized element)

2. **Ragged rows**: Different row lengths → Pad short rows with `Vdom.empty` to match the longest row
   ```fsharp
   let normalizeCells (cells : Vdom<'b, 'k> list list) =
       let maxCols = if List.isEmpty cells then 0 else cells |> List.map List.length |> List.max
       cells |> List.map (fun row ->
           let padding = List.replicate (maxCols - List.length row) Vdom.empty
           row @ padding
       )
   ```

3. **Mismatched specs**: `columnSpecs.Length ≠ numCols` → Default missing specs to `Auto`, ignore extra specs
   ```fsharp
   let normalizeSpecs (specs : 'spec list) (expectedCount : int) (defaultSpec : 'spec) =
       if List.length specs < expectedCount then
           specs @ List.replicate (expectedCount - List.length specs) defaultSpec
       else
           List.take expectedCount specs
   ```

4. **Insufficient space**: Table's minimum > allocated bounds → Cells receive less than their minimum size and must clip gracefully (framework requirement)

5. **Negative proportions**: Treat as epsilon (small positive value like 0.01)
   ```fsharp
   match spec with
   | ProportionColumn p when p <= 0.0 -> ProportionColumn 0.01
   | ProportionRow p when p <= 0.0 -> ProportionRow 0.01
   | other -> other
   ```

6. **Zero proportion total**: If all proportion columns/rows sum to zero, divide space equally among them

7. **NaN or Infinity proportions**: Treat as `AutoColumn` / `AutoRow`

### Focus Management

Focus behaves identically to any other layout component:

- The table structure itself is not focusable
- Individual cells can be marked with `withFocusTracking` if they should be focusable
- Focus cycles through focusable cells in tree-traversal order (the order they appear in the nested split structure)
- If you want stable focus across re-renders (e.g., focus follows a particular data row even if it moves position), provide keyed cells with stable keys based on data identity
- **No special semantics**: The component doesn't impose row-major or any other ordering; focus follows the VDOM tree structure as built by the render function

### Rendering Optimization

- Cache measurement results to avoid re-measuring on every frame
- Early cutoff: if table structure and allocated bounds unchanged, reuse previous arrangement
- User-provided keying allows fine-grained change detection
- Only redraw cells that changed

## Implementation Plan

### Phase 1: Measurement Helper (REQUIRED PREREQUISITE) (0.5 days)
**This phase is required** for Approach 2 to work.

- Add `Vdom.measure` to `Vdom.fs` (~20 lines)
- Expose internal `Layout.measureEither` through public API
- Add unit tests for the helper
- This enables table and future components to measure child nodes

### Phase 2: Component Types and Normalization (0.5 days)
- Create `WoofWare.Zoomies/Components/Table.fs`
- Define `ColumnSpec`, `RowSpec` types
- Implement normalization helpers (`normalizeCells`, `normalizeSpecs`, sanitize proportions)
- Add unit tests for normalization logic

### Phase 3: Measurement Logic (1 day)
- Implement `measure` callback for `FlexibleContent`
- Compute column widths from cell measurements
- Compute row heights given column widths
- Ensure proper clamping to `MeasureConstraints`
- Add unit tests for measurement logic

### Phase 4: Arrangement Logic (1 day)
- Implement allocation helpers (`allocateColumnWidths`, `allocateRowHeights`)
- Implement `render` callback that builds nested `PanelSplit`s
- Handle all spec types (Auto, Fixed, Proportion)
- Add unit tests for arrangement logic

### Phase 5: Integration and Testing (1 day)
- Wire up `Vdom.flexibleContent measure render`
- Test with real terminal output
- Test edge cases (empty table, ragged rows, mismatched specs)
- Test focus behavior (cells with `withFocusTracking`)

### Phase 6: Performance and Polish (1 day)
- Add measurement caching
- Performance testing with large tables (10×10, 50×20, 100×5)
- Optimize if needed (but likely won't be necessary)
- Add comprehensive property-based tests with FsCheck

### Phase 7: Documentation and Examples (0.5 days)
- Add example in `WoofWare.Zoomies.App/` demonstrating table usage
- API documentation comments
- Update architecture docs if needed

**Total: ~5.5 days**

## Future Enhancements

Once the basic table is working, consider:

1. **Borders**: Optional grid lines between cells
   ```fsharp
   type TableOptions = {
       ShowBorders : BorderStyle
   }
   type BorderStyle = NoBorders | RowBorders | ColumnBorders | AllBorders
   ```

2. **Spanning**: Cells that span multiple columns/rows
   ```fsharp
   type Cell =
       | SingleCell of Vdom
       | SpannedCell of Vdom * colspan : int * rowspan : int
   ```

3. **Header rows**: Special styling for header cells

4. **Scrollable tables**: Virtual scrolling for large data sets

5. **Column resizing**: Interactive width adjustment

6. **Sorting**: Click column header to sort

These would be built as either:
- Options added to the `Table.make` function (e.g., `showBorders : bool`)
- Higher-level wrapper components (e.g., `ScrollableTable`, `SortableTable`)
- Separate components that use the basic table (e.g., `DataGrid`)

## Answers to Open Questions

### 1. How should the component obtain child measurements?

**Answer**: **Extend the framework with a `Vdom.measure` helper (Option A from "The Measurement Problem" section)**.

This is a small, clean API addition that:
- Exposes the internal `Layout.measureEither` function as a public API
- Doesn't change FlexibleContent's semantics
- Enables table and future components to make informed layout decisions
- Aligns with the "drop down to lowest-level control" philosophy—components can access the same measurement primitives the framework uses

The alternative (Option B: modify FlexibleContent) would require more invasive changes to the core API.

Option C (build structure twice) is inefficient and doesn't give the component direct access to measurements for computing column widths, so it's not viable for a table implementation that needs to aggregate measurements across cells.

### 2. Should the doc demote new-primitive options?

**Answer**: **Yes, Approaches 1 and 3 should be marked as last-resort options** (already updated in this document).

They conflict with the stated architecture philosophy:
- `docs/architecture/vdom-layout.md:5-11` states panel-split is the sole layout primitive
- `CLAUDE.md` emphasizes "small set of powerful primitives"
- Adding a `Table` or `LayoutGroup` primitive to the core prevents demonstrating that existing primitives are sufficient

These approaches should only be considered if:
1. The FlexibleContent-based table (Approach 2) is fully implemented
2. Real-world performance testing reveals unacceptable performance
3. Optimization attempts (caching, virtual scrolling) are insufficient

The framework philosophy is to build ergonomic libraries on top of minimal primitives, not to expand the primitive set preemptively.

### 3. Other Questions

- **Performance for large tables**: Benchmark after implementing Approach 2. If needed, add virtual scrolling before considering a new primitive.
- **Alignment within cells**: Let cell content handle alignment via `Vdom.styledText`. Keep table focused on layout.
- **Minimum column/row sizes**: Let cells report true minimums. Framework clips gracefully if insufficient space.
- **API ergonomics**: The `list list` structure is simple and composable. Builders can be added as helpers later if needed.

## Conclusion

Building table as a FlexibleContent-based component is the right choice for WoofWare.Zoomies. It:

- **Preserves the framework philosophy**: Small core, rich component library
- **Demonstrates that primitives are sufficient**: FlexibleContent + PanelSplit can express complex layouts (with the small prerequisite of exposing a measurement helper)
- **Allows iteration without core changes**: Only requires a single, minimal API addition (`Vdom.measure`)
- **Follows existing patterns**: Consistent with Button, ProgressBar living in Components/
- **Provides a path to optimization**: If performance becomes an issue, we can measure first, then decide if a primitive is justified

The implementation requires one prerequisite (exposing `Vdom.measure`) but is otherwise straightforward, leveraging the existing two-phase layout system. This approach opens the door to rich data display components built on top of the basic table, while keeping the framework core minimal.
