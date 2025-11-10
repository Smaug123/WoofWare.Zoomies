# Two-Pass Layout System Design for WoofWare.Zoomies

*This document was produced by repeatedly having Sonnet 4.5 generate a doc and then feeding it to Gemini 2.5 Pro for review until both were approximately happy.*

## Overview

Replace the current single-pass top-down layout with a two-pass measure/arrange system that allows components to declare their size preferences declaratively, with automatic conflict resolution.

## Core Types

```fsharp
/// Constraints provided by parent during measurement
type MeasureConstraints = {
    /// Maximum available width.
    /// Invariant: n >= 0
    MaxWidth: int
    /// Maximum available height.
    /// Invariant: n >= 0
    MaxHeight: int
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
type MeasuredSize = {
    /// Minimum width needed to render without data loss.
    /// Must respect any MaxWidth constraint from measurement.
    /// NOTE: This is a strong preference but may be violated by the arrange
    /// pass if insufficient space is available (see "Soft Constraints" below).
    MinWidth: int
    /// Preferred width if space is available.
    PreferredWidth: int
    /// Maximum useful width (None = unbounded growth acceptable)
    /// This is a hint: arrangement may allocate beyond this, but the component
    /// gains no additional utility from the extra space.
    MaxWidth: int option
    /// Minimum height needed given some width
    /// MUST handle width=0 gracefully (e.g., return height for single-char-per-line layout)
    MinHeightForWidth: int -> int
    /// Preferred height given some width
    /// MUST handle width=0 gracefully
    PreferredHeightForWidth: int -> int
    /// Maximum useful height given some width (None = unbounded)
    /// MUST handle width=0 gracefully
    MaxHeightForWidth: int -> int option
}

/// A node annotated with its measurement result
type MeasuredNode<'bounds> = {
    Vdom: Vdom<'bounds, 'key>
    Measured: MeasuredSize
    /// Measured children (for container nodes)
    Children: MeasuredNode<'bounds> list
}

/// Result of the arrange phase
type ArrangedNode = {
    /// The VDOM node being arranged
    Vdom: Vdom<Rectangle, 'key>
    /// Final allocated rectangle for this node
    Bounds: Rectangle
    /// Arranged children (for container nodes)
    Children: ArrangedNode list
}

type Rectangle = {
    TopLeftX: int
    TopLeftY: int
    Width: int
    Height: int
}
```

## Data Flow Between Passes

The two-pass system works as follows:

1. **Measure Pass** (bottom-up): `measure` recursively traverses the VDOM tree, producing a `MeasuredNode` tree where each node is annotated with its `MeasuredSize`. Child measurements are stored in the parent's `Children` field.

2. **Arrange Pass** (top-down): `arrange` recursively traverses the `MeasuredNode` tree with concrete rectangles, producing an `ArrangedNode` tree with final bounds for rendering.

The measured data flows through the tree structure itself - no separate cache or `getMeasured` function is needed.

## Phase 1: Measure

Each node receives `MeasureConstraints` from its parent and returns `MeasuredNode` (itself + children, all measured).

### Constraint Propagation Rules

**Critical**: When measuring children, parents must decide whether to pass modified constraints:

1. **Space-reserving containers** (Border, Padding) **must** pass **reduced** constraints to children, accounting for reserved space
2. **Space-dividing containers** (PanelSplit) pass **unchanged** parent constraints to all children during measure - the split happens in arrange

### Measurement Rules by Node Type

#### TextContent

```fsharp
let measureText (text: string) (constraints: MeasureConstraints): MeasuredSize =
    let longestWord =
        text.Split([|' '; '\t'; '\n'|])
        |> Seq.map String.length
        |> Seq.max
        |> max 1

    let fullLineWidth = text.Length

    // Respect MaxWidth constraint when reporting MinWidth
    let constrainedMinWidth = min longestWord constraints.MaxWidth

    // Clamp preferred width to constraint
    let constrainedPreferredWidth = min fullLineWidth constraints.MaxWidth

    {
        MinWidth = constrainedMinWidth
        PreferredWidth = constrainedPreferredWidth
        MaxWidth = None  // Can grow arbitrarily wide
        MinHeightForWidth = fun w ->
            // CRITICAL: Handle w=0 case
            let safeWidth = max 1 w
            let wrappedLines = wordWrapCount text safeWidth
            max 1 wrappedLines
        PreferredHeightForWidth = fun w ->
            let safeWidth = max 1 w
            let wrappedLines = wordWrapCount text safeWidth
            max 1 wrappedLines
        MaxHeightForWidth = fun w ->
            let safeWidth = max 1 w
            Some (wordWrapCount text safeWidth)
    }

// Helper: count lines after word-wrapping
let wordWrapCount (text: string) (width: int) : int =
    // Implementation: split on whitespace, greedily pack words into lines of max width
    // Return number of lines needed
    // PRECONDITION: width >= 1 (enforced by callers)
    // (Actual implementation omitted for brevity)
```

#### Checkbox

```fsharp
let measureCheckbox: MeasuredSize = {
    MinWidth = 3  // "[ ]" or "[X]"
    PreferredWidth = 3
    MaxWidth = Some 3  // Fixed size component
    MinHeightForWidth = fun _ -> 1
    PreferredHeightForWidth = fun _ -> 1
    MaxHeightForWidth = fun _ -> Some 1
}
```

#### Bordered

**Critical Fix**: Bordered must measure its child with **reduced constraints** to account for the border space.

```fsharp
let measureBordered (child: Vdom<DesiredBounds, 'key>) (constraints: MeasureConstraints): MeasuredNode<DesiredBounds> =
    // Reduce available space by border thickness (2 on each side = 4 total, but we use 1 per side for simplicity)
    let borderThickness = 2
    let childConstraints = {
        MaxWidth = max 0 (constraints.MaxWidth - borderThickness)
        MaxHeight = max 0 (constraints.MaxHeight - borderThickness)
    }

    // Measure child with reduced constraints
    let childMeasured = measure childConstraints child

    // Container's measurements wrap child's measurements
    let containerMeasured = {
        MinWidth = childMeasured.Measured.MinWidth + borderThickness
        PreferredWidth = childMeasured.Measured.PreferredWidth + borderThickness
        MaxWidth = childMeasured.Measured.MaxWidth |> Option.map (fun w -> w + borderThickness)
        MinHeightForWidth = fun w ->
            // CRITICAL: Handle narrow widths
            let innerWidth = max 0 (w - borderThickness)
            childMeasured.Measured.MinHeightForWidth innerWidth + borderThickness
        PreferredHeightForWidth = fun w ->
            let innerWidth = max 0 (w - borderThickness)
            childMeasured.Measured.PreferredHeightForWidth innerWidth + borderThickness
        MaxHeightForWidth = fun w ->
            let innerWidth = max 0 (w - borderThickness)
            childMeasured.Measured.MaxHeightForWidth innerWidth
            |> Option.map (fun h -> h + borderThickness)
    }

    {
        Vdom = Vdom.Bordered child
        Measured = containerMeasured
        Children = [childMeasured]
    }
```

#### PanelSplit

The complex case. **Important**: During measure, pass the **same parent constraints** to both children. The split happens during arrange.

**For `Proportion p` (vertical split with fixed proportion):**

```fsharp
let measureVerticalSplitProportion
    (p: float)
    (child1: Vdom<DesiredBounds, 'key>)
    (child2: Vdom<DesiredBounds, 'key>)
    (constraints: MeasureConstraints): MeasuredNode<DesiredBounds> =

    // Both children measure with same parent constraints
    let child1Measured = measure constraints child1
    let child2Measured = measure constraints child2

    let m1 = child1Measured.Measured
    let m2 = child2Measured.Measured

    // For fixed proportion, compute minimum total width:
    // p*total >= m1.MinWidth => total >= m1.MinWidth / p
    // (1-p)*total >= m2.MinWidth => total >= m2.MinWidth / (1-p)
    let minFromChild1 = int (ceil (float m1.MinWidth / p))
    let minFromChild2 = int (ceil (float m2.MinWidth / (1.0 - p)))

    {
        Vdom = PanelSplit(Vertical, Proportion p, child1, child2)
        Measured = {
            MinWidth = max minFromChild1 minFromChild2
            // What total width lets each child hit its preference?
            PreferredWidth =
                let c1Pref = int (ceil (float m1.PreferredWidth / p))
                let c2Pref = int (ceil (float m2.PreferredWidth / (1.0 - p)))
                max c1Pref c2Pref
            MaxWidth = None  // Can grow to accommodate both
            MinHeightForWidth = fun w ->
                let w1 = int (float w * p)
                let w2 = w - w1
                max (m1.MinHeightForWidth w1) (m2.MinHeightForWidth w2)
            PreferredHeightForWidth = fun w ->
                let w1 = int (float w * p)
                let w2 = w - w1
                max (m1.PreferredHeightForWidth w1) (m2.PreferredHeightForWidth w2)
            MaxHeightForWidth = fun w ->
                let w1 = int (float w * p)
                let w2 = w - w1
                match m1.MaxHeightForWidth w1, m2.MaxHeightForWidth w2 with
                | Some h1, Some h2 -> Some (max h1 h2)
                | _ -> None
        }
        Children = [child1Measured; child2Measured]
    }
```

**For `Auto` (vertical split with content-driven proportion):**

```fsharp
let measureVerticalSplitAuto
    (child1: Vdom<DesiredBounds, 'key>)
    (child2: Vdom<DesiredBounds, 'key>)
    (constraints: MeasureConstraints): MeasuredNode<DesiredBounds> =

    let child1Measured = measure constraints child1
    let child2Measured = measure constraints child2

    let m1 = child1Measured.Measured
    let m2 = child2Measured.Measured

    // Helper: compute width split - must match arrange logic for accurate height calculation
    let computeWidthSplit (totalWidth: int) : int * int =
        let totalPref = m1.PreferredWidth + m2.PreferredWidth
        let minSum = m1.MinWidth + m2.MinWidth

        if totalWidth < minSum then
            // Can't satisfy minimums - scale proportionally by minimum requirements
            let scale = float totalWidth / float minSum
            let w1 = int (float m1.MinWidth * scale)
            (w1, totalWidth - w1)
        elif totalWidth >= minSum && totalWidth <= totalPref then
            // Between minimums and preferences - distribute by preference ratio
            let p = if totalPref = 0 then 0.5 else float m1.PreferredWidth / float totalPref
            // Satisfy minimums, distribute remainder by ratio
            let remainder = totalWidth - minSum
            let w1 = m1.MinWidth + int (float remainder * p)
            (w1, totalWidth - w1)
        else  // totalWidth > totalPref
            // More than preferences - distribute excess proportionally to preferences
            let p = if totalPref = 0 then 0.5 else float m1.PreferredWidth / float totalPref
            let extraSpace = totalWidth - totalPref
            let extraFor1 = int (float extraSpace * p)
            let w1 = m1.PreferredWidth + extraFor1
            (w1, totalWidth - w1)

    {
        Vdom = PanelSplit(Vertical, Auto, child1, child2)
        Measured = {
            MinWidth = m1.MinWidth + m2.MinWidth
            PreferredWidth = m1.PreferredWidth + m2.PreferredWidth
            MaxWidth = None
            MinHeightForWidth = fun w ->
                let w1, w2 = computeWidthSplit w
                max (m1.MinHeightForWidth w1) (m2.MinHeightForWidth w2)
            PreferredHeightForWidth = fun w ->
                let w1, w2 = computeWidthSplit w
                max (m1.PreferredHeightForWidth w1) (m2.PreferredHeightForWidth w2)
            MaxHeightForWidth = fun w ->
                let w1, w2 = computeWidthSplit w
                match m1.MaxHeightForWidth w1, m2.MaxHeightForWidth w2 with
                | Some h1, Some h2 -> Some (max h1 h2)
                | _ -> None
        }
        Children = [child1Measured; child2Measured]
    }
```

**For `Absolute n` (vertical split with fixed first width):**

```fsharp
let measureVerticalSplitAbsolute
    (n: int)
    (child1: Vdom<DesiredBounds, 'key>)
    (child2: Vdom<DesiredBounds, 'key>)
    (constraints: MeasureConstraints): MeasuredNode<DesiredBounds> =

    // IMPORTANT: Child1 is measured with constrained width since we will only
    // allocate n pixels to it during arrange. This ensures the measure pass
    // truthfully represents what space will be available.
    let child1Constraints = {
        MaxWidth = min n constraints.MaxWidth
        MaxHeight = constraints.MaxHeight
    }

    let child1Measured = measure child1Constraints child1
    let child2Measured = measure constraints child2

    let m1 = child1Measured.Measured
    let m2 = child2Measured.Measured

    {
        Vdom = PanelSplit(Vertical, Absolute n, child1, child2)
        Measured = {
            // Container's minimum is the space we promise to child1 plus child2's minimum
            MinWidth = n + m2.MinWidth
            PreferredWidth = n + m2.PreferredWidth
            MaxWidth = None
            MinHeightForWidth = fun w ->
                // Child1 gets min(n, w), child2 gets remainder
                let w1 = min n w
                let w2 = max 0 (w - w1)
                max (m1.MinHeightForWidth w1) (m2.MinHeightForWidth w2)
            PreferredHeightForWidth = fun w ->
                let w1 = min n w
                let w2 = max 0 (w - w1)
                max (m1.PreferredHeightForWidth w1) (m2.PreferredHeightForWidth w2)
            MaxHeightForWidth = fun w ->
                let w1 = min n w
                let w2 = max 0 (w - w1)
                match m1.MaxHeightForWidth w1, m2.MaxHeightForWidth w2 with
                | Some h1, Some h2 -> Some (max h1 h2)
                | _ -> None
        }
        Children = [child1Measured; child2Measured]
    }
```

Horizontal splits follow analogous logic but swap width/height.

## Phase 2: Arrange

The arrange phase takes a `MeasuredNode` tree and a concrete `Rectangle`, and produces an `ArrangedNode` tree with final bounds for every node.

### Arrangement Responsibility

**Parent containers** compute how to divide their allocated rectangle among children and recursively call arrange on each child with its final rectangle.

**All nodes** (including leaves) simply store their allocated rectangle and recursively arrange children if any.

**CRITICAL: Top-Down Height Discipline**

The arrange pass is strictly top-down. When a parent arranges a child, it gives the child a concrete `Rectangle` representing the exact space the child must occupy. The child **must** use this rectangle as-is - it cannot compute its own height based on its contents.

This is a fundamental difference from the measure pass:
- **Measure pass (bottom-up)**: Children report "I need at least X width and prefer Y width; given width W, I would need height H(W)"
- **Arrange pass (top-down)**: Parents command "You must fit into this exact rectangle"

The parent has already considered the child's reported height preferences (from `PreferredHeightForWidth`) when deciding what rectangle to give the child. The child simply accepts what it's given.

### Soft Constraints: MinWidth Violation

**IMPORTANT**: The `MinWidth` and `MinHeightForWidth` values reported during the measure pass are strong preferences, and the layout system will always attempt to satisfy them. However, when the available space provided by a parent container is insufficient (i.e., less than the sum of children's minimum requirements), the arrange pass may be forced to allocate sizes smaller than the reported minimums.

Components must be able to render gracefully even if given less than their minimum required size. This typically means:
- Text components clip or truncate content
- Fixed-size components (like checkboxes) render in a degraded but functional state
- Container components pass along the constraint violation to their children

This design choice ensures the layout system always produces a valid layout, even under severe space constraints, rather than failing or producing undefined behavior.

**Important for Proportion Splits**: `Proportion` behavior always honors the specified proportion exactly, regardless of child minimums. This design choice ensures UI stability - a log viewer allocated 25% of the screen never expands to 80% just because the log content grew. The explicit proportion parameter represents stronger programmer intent than the implicit minimums derived from content. See "Proportion vs Minimum Conflicts" below for detailed rationale.

### Child Alignment Policy

When a container's allocated height exceeds what a child needs, the child is **top-aligned** (positioned at the top of the container, with unused space below). Similarly, for width, children are **left-aligned** (positioned at the left, with unused space to the right).

This is the simplest and most predictable alignment strategy. Future versions could add explicit alignment controls if needed.

### Arrangement Algorithms by Container Type

**Note on Function Signatures:** The arrange functions shown below include a `parentVdom` parameter to ensure the returned `ArrangedNode` contains the correct VDOM reference. In a real implementation, this would typically be obtained from the `MeasuredNode.Vdom` field of the container being arranged.

#### Vertical Split with Proportion

```fsharp
let arrangeVerticalSplitProportion
    (p: float)
    (child1: MeasuredNode<DesiredBounds>)
    (child2: MeasuredNode<DesiredBounds>)
    (parentVdom: Vdom<DesiredBounds, 'key>)  // The container's VDOM
    (availableSpace: Rectangle): ArrangedNode =

    let m1 = child1.Measured
    let m2 = child2.Measured

    // Calculate widths based on proportion
    // CRITICAL: Always calculate w2 as remainder to avoid rounding gaps
    // For Proportion splits, we ALWAYS honor the proportion exactly.
    // Child minimums are soft constraints - if violated, children must
    // render gracefully in degraded space. This ensures UI stability:
    // as child content changes, the split ratio remains constant.
    let w1 = int (float availableSpace.Width * p)
    let w2 = availableSpace.Width - w1

    // Create final rectangles for children
    // CRITICAL: Use the height we were given, not a computed height
    let bounds1 = {
        TopLeftX = availableSpace.TopLeftX
        TopLeftY = availableSpace.TopLeftY
        Width = w1
        Height = availableSpace.Height
    }
    let bounds2 = {
        TopLeftX = availableSpace.TopLeftX + w1
        TopLeftY = availableSpace.TopLeftY
        Width = w2
        Height = availableSpace.Height
    }

    // Recursively arrange children
    let arranged1 = arrange child1 bounds1
    let arranged2 = arrange child2 bounds2

    {
        Vdom = parentVdom  // The container's VDOM, not a child's
        Bounds = availableSpace  // Use exactly what we were given
        Children = [arranged1; arranged2]
    }
```

#### Vertical Split with Auto

```fsharp
let arrangeVerticalSplitAuto
    (child1: MeasuredNode<DesiredBounds>)
    (child2: MeasuredNode<DesiredBounds>)
    (parentVdom: Vdom<DesiredBounds, 'key>)  // The container's VDOM
    (availableSpace: Rectangle): ArrangedNode =

    let m1 = child1.Measured
    let m2 = child2.Measured

    let totalPref = m1.PreferredWidth + m2.PreferredWidth
    let minSum = m1.MinWidth + m2.MinWidth

    let (w1, w2) =
        if availableSpace.Width < minSum then
            // Can't satisfy minimums - scale proportionally by minimum requirements
            // This violates the soft MinWidth constraint (see "Soft Constraints" section)
            let scale = float availableSpace.Width / float minSum
            let w1 = int (float m1.MinWidth * scale)
            (w1, availableSpace.Width - w1)
        elif availableSpace.Width >= minSum && availableSpace.Width <= totalPref then
            // Between minimums and preferences - distribute by preference ratio
            let p = if totalPref = 0 then 0.5 else float m1.PreferredWidth / float totalPref
            // Satisfy minimums, distribute remainder by ratio
            let remainder = availableSpace.Width - minSum
            let w1 = m1.MinWidth + int (float remainder * p)
            (w1, availableSpace.Width - w1)
        else  // availableSpace.Width > totalPref
            // More than preferences - distribute excess proportionally to preferences
            let p = if totalPref = 0 then 0.5 else float m1.PreferredWidth / float totalPref
            let extraSpace = availableSpace.Width - totalPref
            let extraFor1 = int (float extraSpace * p)
            let w1 = m1.PreferredWidth + extraFor1
            (w1, availableSpace.Width - w1)

    // Create final rectangles using the height we were given
    let bounds1 = {
        TopLeftX = availableSpace.TopLeftX
        TopLeftY = availableSpace.TopLeftY
        Width = w1
        Height = availableSpace.Height
    }
    let bounds2 = {
        TopLeftX = availableSpace.TopLeftX + w1
        TopLeftY = availableSpace.TopLeftY
        Width = w2
        Height = availableSpace.Height
    }

    let arranged1 = arrange child1 bounds1
    let arranged2 = arrange child2 bounds2

    {
        Vdom = parentVdom
        Bounds = availableSpace
        Children = [arranged1; arranged2]
    }
```

#### Vertical Split with Absolute

```fsharp
let arrangeVerticalSplitAbsolute
    (n: int)
    (child1: MeasuredNode<DesiredBounds>)
    (child2: MeasuredNode<DesiredBounds>)
    (parentVdom: Vdom<DesiredBounds, 'key>)  // The container's VDOM
    (availableSpace: Rectangle): ArrangedNode =

    let m1 = child1.Measured
    let m2 = child2.Measured

    // Child1 gets its absolute width (clamped to available)
    let w1 = min n availableSpace.Width
    let w2 = availableSpace.Width - w1

    let bounds1 = {
        TopLeftX = availableSpace.TopLeftX
        TopLeftY = availableSpace.TopLeftY
        Width = w1
        Height = availableSpace.Height
    }
    let bounds2 = {
        TopLeftX = availableSpace.TopLeftX + w1
        TopLeftY = availableSpace.TopLeftY
        Width = w2
        Height = availableSpace.Height
    }

    let arranged1 = arrange child1 bounds1
    let arranged2 = arrange child2 bounds2

    {
        Vdom = parentVdom
        Bounds = availableSpace
        Children = [arranged1; arranged2]
    }
```

#### Leaf Node and Bordered Arrangement

```fsharp
let arrangeLeaf (measured: MeasuredNode<DesiredBounds>) (bounds: Rectangle): ArrangedNode =
    {
        Vdom = measured.Vdom
        Bounds = bounds
        Children = []
    }

let arrangeBordered (measured: MeasuredNode<DesiredBounds>) (bounds: Rectangle): ArrangedNode =
    let borderThickness = 2
    let innerBounds = {
        TopLeftX = bounds.TopLeftX + 1
        TopLeftY = bounds.TopLeftY + 1
        Width = max 0 (bounds.Width - borderThickness)
        Height = max 0 (bounds.Height - borderThickness)
    }
    let childArranged = arrange measured.Children.[0] innerBounds

    {
        Vdom = measured.Vdom
        Bounds = bounds
        Children = [childArranged]
    }
```

## Integration with Current System

### Top-Level Layout Function

```fsharp
let layout (vdom: Vdom<DesiredBounds, Unkeyed>) (terminalBounds: Rectangle): ArrangedNode =
    // Phase 1: Measure with terminal bounds as constraints
    let constraints = {
        MaxWidth = terminalBounds.Width
        MaxHeight = terminalBounds.Height
    }
    let measured = measure constraints vdom

    // Phase 2: Arrange with terminal bounds
    arrange measured terminalBounds
```

### Backwards Compatibility

Existing split behaviours map naturally:
- `Proportion p`: Semantics clarified - always honors the specified proportion exactly, treating child minimums as soft constraints for UI stability
- `Absolute n`: Semantics clarified - now consistently reserves n pixels for first child

New users gain access to `Auto` split mode for content-driven layout.

### New SplitBehaviour Type

```fsharp
type SplitBehaviour =
    | Proportion of float  // Fixed proportion between children
    | Absolute of int      // Fixed size for first child
    | Auto                 // NEW: content-driven split based on preferences
```

## Worked Example

Let's trace through a simple layout:

```fsharp
// VDOM structure:
let vdom =
    Vdom.panelSplitAuto(
        Vertical,
        Vdom.textContent false "Hello world",
        Vdom.checkbox false false
    )

// Terminal bounds: 20 width × 3 height
let terminalBounds = { X = 0; Y = 0; Width = 20; Height = 3 }
```

**Measure Phase:**

1. Measure text with constraints `{MaxWidth = Some 20; MaxHeight = Some 3}`:
   - `MinWidth = 5` (longest word is "world")
   - `PreferredWidth = 11` (full "Hello world")
   - `PreferredHeightForWidth(w) = 1` if w >= 11, else `ceil(11.0 / w)`

2. Measure checkbox with same constraints:
   - `MinWidth = 3`
   - `PreferredWidth = 3`
   - `PreferredHeightForWidth(w) = 1`

3. Measure Auto split:
   - `MinWidth = 5 + 3 = 8`
   - `PreferredWidth = 11 + 3 = 14`
   - For width 20: split is 11:3 ratio → w1=15, w2=5
   - Heights: both report 1 → container reports 1

**Arrange Phase:**

Available space is `{X=0, Y=0, Width=20, Height=3}`.

Available width is 20, which exceeds preferred 14:

1. Compute proportion: `p = 11 / 14 ≈ 0.786`

2. Distribute excess space (6 pixels) proportionally:
   - Extra for text: `int(6 * 0.786) = 4`
   - Text gets: `11 + 4 = 15`
   - Checkbox gets: `20 - 15 = 5`

3. Heights: Use the provided height (3) for both children
   - Text is allocated: `{X=0, Y=0, Width=15, Height=3}`
   - Checkbox is allocated: `{X=15, Y=0, Width=5, Height=3}`

4. Final rectangles:
   - Text: `{X=0, Y=0, Width=15, Height=3}`
   - Checkbox: `{X=15, Y=0, Width=5, Height=3}`
   - Container: `{X=0, Y=0, Width=20, Height=3}` (exactly what was given)

## Implementation Strategy

1. Add `MeasuredNode` and `ArrangedNode` types
2. Implement `measure` for each node type (bottom-up pass)
   - Leaves: TextContent, Checkbox
   - Containers: Bordered (with reduced child constraints), PanelSplit
3. Implement `arrange` for each node type (top-down pass)
4. Test with simple layouts first
5. Add early cutoff optimization: if measured node reference equals previous, skip remeasure
6. Consider caching measured results by (node identity, constraints) for performance

## Critical Implementation Notes

### Zero-Width Handling

**CRITICAL:** All `...HeightForWidth` functions must handle width=0 gracefully:

```fsharp
// ✅ CORRECT: Safe handling
let heightForWidth w =
    let safeWidth = max 1 w
    computeHeight safeWidth

// ❌ WRONG: Can divide by zero
let heightForWidth w =
    textLength / w  // Crashes if w=0
```

This invariant must be maintained across all measurement functions.

### Floating Point Precision

**CRITICAL:** Always calculate the second child's width as `total - w1`:

```fsharp
// ✅ CORRECT: Guarantees no gaps
let w1 = int (float total * p)
let w2 = total - w1

// ❌ WRONG: Can create gaps
let w1 = int (float total * p)
let w2 = int (float total * (1.0 - p))
```

This pattern must be followed throughout all container arrangement code.

### Constraint Propagation

- **Border/Padding containers**: Reduce constraints before passing to children
- **Split containers**: Pass unchanged constraints to children during measure

### Height Calculation

During the **measure** pass, heights are calculated as functions of width using `PreferredHeightForWidth(w)` and similar functions. This allows the measure pass to report "given width W, I would need height H".

During the **arrange** pass, heights are provided by the parent as part of the concrete `Rectangle`. Children do not compute their own heights - they accept what the parent gives them. The parent has already considered the child's height preferences when deciding what rectangle to allocate.

### Proportion vs Minimum Conflicts

When `Proportion p` splits conflict with child minimum size requirements, **the proportion always wins**. This design choice prioritizes:

1. **UI Stability**: As child content changes (e.g., log viewer text growing), the split ratio remains constant. A log viewer allocated 25% of the screen never expands to 80% just because the log content grew.

2. **Explicit Intent**: The programmer wrote `Proportion 0.7` explicitly; minimums are inferred from content. Explicit beats implicit.

3. **TUI Constraints**: Unlike GUI apps with scrolling, TUI has hard space limits where minimums cannot always be satisfied. Prioritizing minimums would cause cascading layout failures.

4. **Measurement Already Accounts for Proportion**: The container's `MinHeightForWidth` (or `MinWidth` for vertical splits) already encodes "to maintain proportion p, I need this much space". If the parent allocated more than that minimum, the proportion can be honored.

This differs from `Auto` splits, which distribute space based on content preferences and may adjust ratios to better accommodate minimum requirements.

## Future Considerations

### Vertical Stack Panel

The current design only includes `PanelSplit`, which divides space between exactly two children. A `VerticalStack` container that arranges N children vertically (like a `<div>` list in HTML) is a common and important layout pattern, but is deferred to a future phase.

When implemented, it would follow these rules:

**Measurement:**
- `MinWidth`: `max` of all children's `MinWidth`
- `PreferredWidth`: `max` of all children's `PreferredWidth`
- `MinHeightForWidth(w)`: `sum` of all children's `MinHeightForWidth(w)`
- `PreferredHeightForWidth(w)`: `sum` of all children's `PreferredHeightForWidth(w)`

**Arrangement:**
- Each child receives the full container width
- Children are stacked vertically from top to bottom
- Each child receives its `PreferredHeightForWidth(containerWidth)` if space allows
- If insufficient height: distribute proportionally to preferences (similar to Auto split logic)
- If excess height: either leave blank at bottom, or distribute using FlexGrow weights

### FlexGrow/FlexShrink

The current implementation distributes excess space **proportionally to preferences** when available width exceeds total preferred width. When shrinking (between minimum and preferred), space is also distributed proportionally.

A full flexbox-style implementation would add explicit `FlexGrow` and `FlexShrink` properties:
- When `available > totalPreferred`: distribute excess using `FlexGrow` weights (instead of always using preference ratios)
- When `available < totalPreferred` but `>= totalMin`: shrink using `FlexShrink` weights (instead of always using preference ratios)

This is left for future enhancement. For now, the simpler proportional distribution provides predictable and intuitive behavior without requiring additional properties.

### Enhanced Alignment

The current top-left alignment is simple and predictable, but future versions could add:
- Vertical alignment: top/center/bottom
- Horizontal alignment: left/center/right
- Per-component alignment overrides

These would be specified during arrange, not measure (since alignment doesn't affect size calculations).

### Caching Strategy

For performance, implementations should:
- Cache `MeasuredNode` results keyed by `(node identity, constraints)`
- Invalidate cache when node changes (reference inequality)
- Early cutoff: if measuring a node returns the same reference as previous, skip remeasure of subtree

### Scrolling and Clipping

The current design assumes content that exceeds bounds is clipped. Future versions could add:
- Scroll containers that manage viewport within larger content
- Overflow indicators when content is clipped
- Scrollbar rendering

These are orthogonal to layout and can be added as container types later.
