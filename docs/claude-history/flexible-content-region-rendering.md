# FlexibleContent: Region-Dependent Rendering for WoofWare.Zoomies

## Motivation

Currently, WoofWare.Zoomies has region-dependent rendering logic baked into the framework (e.g., text word-wrapping), but this capability is not exposed to users. Components like `ProgressBar` need to render genuinely different content depending on the allocated space:

- A progress bar with width 80 might show `[============================>                    ] 60%`
- The same progress bar with width 20 might show `[========>    ] 60%`

These are not just different measurements of the same content—they are fundamentally different Vdom trees.

The framework's two-pass measure/arrange architecture creates a challenge: during measurement, we don't yet know the final allocated bounds, but some components need those bounds to determine their structure.

## Problem Statement

**Given**: A component that can render in multiple valid ways depending on available space.

**Want**: A mechanism for users to specify:
1. How much space the component wants/needs (measurement)
2. What content to produce given specific allocated bounds (rendering)

**Constraint**: Must not require fixed-point iteration or unbounded measure/arrange passes.

## Proposed Solution

Add a new `FlexibleContent` variant to `UnkeyedVdom` that acts as a monadic bind over the layout process:

```fsharp
type UnkeyedVdom<'bounds> =
    | Bordered of KeylessVdom<'bounds>
    | PanelSplit of SplitDirection * SplitBehaviour * KeylessVdom<'bounds> * KeylessVdom<'bounds>
    | TextContent of string * focused: bool
    | ToggleWithGlyph of uncheckedGlyph: char * checkedGlyph: char * isChecked: bool * isFocused: bool
    | Focusable of isInitialFocus: bool * KeyedVdom<'bounds>
    | Empty
    | FlexibleContent of
        measure: (MeasureConstraints -> MeasuredSize) *
        render: (Rectangle -> KeylessVdom<DesiredBounds>)
```

### Key Design Decisions

1. **Two separate functions**: `measure` and `render` are provided by the user
2. **Measurement is speculative**: The `measure` function produces a `MeasuredSize` without knowing the final bounds
3. **Rendering is definitive**: The `render` function receives the actual allocated `Rectangle` and produces final Vdom
4. **No infinite regress**: The `render` function returns `KeylessVdom<DesiredBounds>`, not another `FlexibleContent`, preventing nested flexible content

### Conceptual Model

This is analogous to a monadic bind operation where:
- The "monadic context" is the layout calculation
- The `measure` function is like `return` (injecting a measurement into the layout monad)
- The `render` function is like `bind` (producing new Vdom given the layout result)

## Implementation Plan

### Phase 1: Extend Vdom Type

**Location**: `WoofWare.Zoomies/Vdom.fs`

1. Add `FlexibleContent` case to `UnkeyedVdom<'bounds>`
2. Add public API on the `Vdom` class:

```fsharp
static member flexibleContent
    (measure: MeasureConstraints -> MeasuredSize)
    (render: Rectangle -> Vdom<DesiredBounds, Unkeyed>)
    : Vdom<DesiredBounds, Unkeyed>
```

Note: The public API takes `Vdom<DesiredBounds, Unkeyed>` for ergonomics, but internally converts to `KeylessVdom<DesiredBounds>`.

### Phase 2: Modify ArrangedNode Type

**Location**: `WoofWare.Zoomies/Layout.fs`

**Critical Change**: The `ArrangedNode` type must be extended to store the original `DesiredBounds` Vdom that was arranged. This is required for FlexibleContent but also improves the existing pipeline's ability to perform content-based diffing.

```fsharp
type ArrangedNode =
    {
        /// The VDOM node being arranged
        Vdom : KeylessVdom<Rectangle>
        /// The original DesiredBounds VDOM this was arranged from
        VDomSource : KeylessVdom<DesiredBounds>
        /// Final allocated rectangle for this node
        Bounds : Rectangle
        /// Arranged children (for container nodes)
        Children : ArrangedNode list
    }
```

**Rationale**: For FlexibleContent, the `render` function produces `KeylessVdom<DesiredBounds>` during the arrange phase. This needs to flow through to `arrangedToRendered` for content-based diffing. Without this field, the DesiredBounds Vdom is discarded after arrangement, breaking the early cutoff mechanism.

**Impact**: All existing `arrange` functions must be updated to populate `VDomSource`. For non-FlexibleContent nodes, this is simply the input Vdom from measurement. For FlexibleContent, this is the output of the `render` function.

### Phase 3: Measurement Phase

**Location**: `WoofWare.Zoomies/Layout.fs`

Add a measurement handler in the `measureUnkeyed` function:

```fsharp
| UnkeyedVdom.FlexibleContent (measure, _) ->
    let measured = measure constraints

    // Validate that the user's measurement respects constraints
    // (MinWidth <= constraints.MaxWidth, etc.)

    {
        Vdom = KeylessVdom.Unkeyed vdom
        Measured = measured
        Children = []  // No children yet - we don't know what they are
    }
```

**Validation Requirements**:
- `measured.MinWidth <= constraints.MaxWidth`
- `measured.PreferredWidth <= constraints.MaxWidth` (if we want to enforce this)
- For all `w`, `measured.MinHeightForWidth(w) >= 0`

**Error Handling**: If validation fails, either:
- Option A: Clamp values to valid ranges (forgiving)
- Option B: Throw an exception (strict)

Recommendation: **Option A** with debug logging, since user measurement functions may not perfectly anticipate all constraint scenarios.

### Phase 4: Arrangement Phase

**Location**: `WoofWare.Zoomies/Layout.fs`

Extend the `arrange` function to handle `FlexibleContent`:

```fsharp
| UnkeyedVdom.FlexibleContent (measure, render) ->
    // Call the render function with allocated bounds
    let renderedVdom = render bounds  // This is KeylessVdom<DesiredBounds>

    // Measure the rendered Vdom with constraints = allocated bounds
    let renderConstraints =
        {
            MaxWidth = bounds.Width
            MaxHeight = bounds.Height
        }

    let measuredRendered = measureEither renderConstraints renderedVdom

    // Arrange the rendered Vdom with the same bounds
    let arrangedRendered = arrange measuredRendered bounds

    {
        Vdom = KeylessVdom.Unkeyed (UnkeyedVdom.FlexibleContent (measure, render))
        VDomSource = KeylessVdom.Unkeyed (UnkeyedVdom.FlexibleContent (measure, render))
        Bounds = bounds
        Children = [arrangedRendered]
    }
```

**Critical Detail**: The `arrangedRendered` child's `VDomSource` field contains `renderedVdom` (populated by the recursive `arrange` call). This is essential - when we later traverse into the child during rendering, we need access to its original DesiredBounds representation.

**Note on VDomSource**: For the FlexibleContent node itself, we store the original FlexibleContent in VDomSource (not the rendered child). This maintains the invariant that VDomSource represents the DesiredBounds Vdom that produced this ArrangedNode.

**All existing arrange functions must be updated**: Every place that constructs an `ArrangedNode` must populate the new `VDomSource` field. For normal nodes, this is straightforward - it's the input Vdom being arranged.

### Phase 5: Rendering Phase

**Location**: `WoofWare.Zoomies/Render.fs`

Extend `arrangedToRendered` to handle FlexibleContent:

```fsharp
| UnkeyedVdom.FlexibleContent _ when arranged.Children.Length = 1 ->
    // FlexibleContent is a transparent container
    // Render the single child that was produced during arrange
    let childArranged = arranged.Children.[0]

    // Extract the child's original Vdom from its VDomSource field
    let childOriginalVdom = childArranged.VDomSource

    let prevChild =
        match previousRender with
        | Some prev when prev.OverlaidChildren.Length = 1 ->
            Some prev.OverlaidChildren.[0]
        | _ -> None

    let childRendered =
        arrangedToRendered
            keyToNode
            focusableKeys
            initialFocusKey
            prevChild
            childArranged
            childOriginalVdom  // Use the VDomSource from the child's ArrangedNode

    {
        Bounds = arranged.Bounds
        OverlaidChildren = [childRendered]
        VDomSource = originalVdom
        Self = arranged.Vdom
        ArrangedSource = Some arranged
    }
```

**Key Resolution**: The `childOriginalVdom` is obtained from `childArranged.VDomSource`, which contains the `KeylessVdom<DesiredBounds>` produced by the `render` function during arrangement. This solves the data flow problem identified in the original spec.

**For Previous Render**: When comparing with `previousRender`, we compare the child structures. If the previous FlexibleContent's child matches the current child (structurally), we can apply early cutoff.

Extend `renderToBuffer` similarly - FlexibleContent is transparent, just render its child.

### Phase 6: Early Cutoff Handling

**Location**: `WoofWare.Zoomies/Render.fs`

**Critical Requirement**: Content-based early cutoff for FlexibleContent is **mandatory**, not optional.

**Why This Is Mandatory**:

The existing rendering pipeline relies on `arrangedToRendered` returning the same `RenderedNode` *object reference* when content hasn't changed. The main render loop uses `Object.referenceEquals` checks to skip diffing entire subtrees:

```fsharp
// From existing Render.fs
match renderState.PreviousVdom with
| Some prev when Object.referenceEquals prev layoutResult ->
    // Nothing changed, skip rendering
    ()
```

If FlexibleContent always creates a new `RenderedNode` (even when the rendered content is identical), the `referenceEquals` check in its parent will fail. **This failure cascades up the tree**: the parent creates a new `RenderedNode`, causing the grandparent's check to fail, and so on. The entire branch containing the FlexibleContent will be forced to re-diff and re-render on every frame, even when nothing visually changed.

**Implementation Strategy**:

Early cutoff for FlexibleContent must operate at the child level. After calling `arrangedToRendered` on the FlexibleContent's child, compare it with the previous child:

```fsharp
| UnkeyedVdom.FlexibleContent _ when arranged.Children.Length = 1 ->
    let childArranged = arranged.Children.[0]
    let childOriginalVdom = childArranged.VDomSource

    // Check if we can reuse the previous render
    let prevChild =
        match previousRender with
        | Some prev when prev.OverlaidChildren.Length = 1 ->
            Some prev.OverlaidChildren.[0]
        | _ -> None

    let childRendered =
        arrangedToRendered
            keyToNode
            focusableKeys
            initialFocusKey
            prevChild
            childArranged
            childOriginalVdom

    // Early cutoff: if childRendered === prevChild (same object reference),
    // the recursive call already applied early cutoff, so we can too
    match prevChild with
    | Some prev when Object.ReferenceEquals(childRendered, prev) ->
        // Reuse the entire previous FlexibleContent node
        previousRender.Value
    | _ ->
        // Content changed, create new node
        {
            Bounds = arranged.Bounds
            OverlaidChildren = [childRendered]
            VDomSource = originalVdom
            Self = arranged.Vdom
            ArrangedSource = Some arranged
        }
```

**How This Works**:

1. We recursively call `arrangedToRendered` on the child, passing the previous child as context
2. That recursive call performs its own content-based comparisons
3. If nothing changed in the child subtree, the recursive call returns the same `RenderedNode` object reference
4. We detect this via `Object.ReferenceEquals` and can then reuse our entire `previousRender` node
5. This allows the reference-equality optimization to propagate up through FlexibleContent nodes

**Bounds Changes**:

If the allocated bounds change between frames, the `render` function will be called again during `arrange`, potentially producing different content. The content-based comparison will detect this difference and create a new `RenderedNode`.

**Performance Characteristics**:

- Best case (stable bounds, stable content): Zero allocation, full early cutoff
- Worst case (changing bounds/content): One additional object allocation for the FlexibleContent wrapper
- The cost is equivalent to any other container node (PanelSplit, Bordered, etc.)

### Phase 7: Focus Handling

**Location**: `WoofWare.Zoomies/Render.fs` (arrangedToRendered)

When traversing FlexibleContent, the child tree may contain focusable elements. These need to be registered properly:

```fsharp
// When recursively calling arrangedToRendered on the child:
let childRendered =
    arrangedToRendered
        keyToNode        // Same dictionary - child's focusables register here
        focusableKeys    // Same ordered set
        initialFocusKey  // Same ref
        prevChild
        childArranged
        childOriginalVdom
```

This means focusable elements inside FlexibleContent's rendered tree will be registered just like any other focusable elements.

**Edge Case**: If the FlexibleContent renders different structures based on size, focusable elements might appear/disappear. The existing focus loss handling (clearing focus when a key no longer exists) should handle this correctly.

## Usage Examples

### Example 1: Progress Bar

```fsharp
let progressBar (fraction: float) (label: string) =
    let measure (constraints: MeasureConstraints) =
        {
            MinWidth = 10  // "[>    ]" with at least 4 cells of space
            PreferredWidth = 50
            MaxWidth = None
            MinHeightForWidth = fun _ -> 1
            PreferredHeightForWidth = fun _ -> 1
            MaxHeightForWidth = fun _ -> Some 1
        }

    let render (bounds: Rectangle) =
        let barWidth = bounds.Width
        let filledCells = int (float (barWidth - 2) * fraction)
        let emptyCells = (barWidth - 2) - filledCells

        let bar =
            "[" +
            String.replicate filledCells "=" +
            ">" +
            String.replicate (emptyCells - 1) " " +
            "]"

        let barText =
            if barWidth >= label.Length + 5 then
                // Wide enough to show label inline
                sprintf "%s %s" bar label
            else
                // Just show the bar
                bar

        Vdom.textContent false barText

    Vdom.flexibleContent measure render
```

### Example 2: Adaptive Table

```fsharp
let adaptiveTable (columns: string list) (rows: string list list) =
    let measure (constraints: MeasureConstraints) =
        let minWidthPerCol = 5
        let preferredWidthPerCol = 20

        {
            MinWidth = minWidthPerCol * columns.Length
            PreferredWidth = preferredWidthPerCol * columns.Length
            MaxWidth = None
            MinHeightForWidth = fun _ -> rows.Length + 1  // +1 for header
            PreferredHeightForWidth = fun _ -> rows.Length + 1
            MaxHeightForWidth = fun _ -> Some (rows.Length + 1)
        }

    let render (bounds: Rectangle) =
        let colWidth = bounds.Width / columns.Length

        if colWidth >= 10 then
            // Render as full table with borders
            renderFullTable columns rows colWidth
        else
            // Render as compact list
            renderCompactList columns rows

    Vdom.flexibleContent measure render
```

### Example 3: Responsive Layout

```fsharp
let responsiveContent (content: string) =
    let measure (constraints: MeasureConstraints) =
        {
            MinWidth = 20
            PreferredWidth = 80
            MaxWidth = None
            MinHeightForWidth = fun w ->
                if w < 40 then 10  // Vertical layout
                else 5             // Horizontal layout
            PreferredHeightForWidth = fun w ->
                if w < 40 then 10
                else 5
            MaxHeightForWidth = fun _ -> None
        }

    let render (bounds: Rectangle) =
        if bounds.Width < 40 then
            // Vertical stack layout
            Vdom.panelSplitProportion(
                SplitDirection.Horizontal,
                0.5,
                Vdom.textContent false "Top",
                Vdom.textContent false "Bottom"
            )
        else
            // Horizontal side-by-side layout
            Vdom.panelSplitProportion(
                SplitDirection.Vertical,
                0.5,
                Vdom.textContent false "Left",
                Vdom.textContent false "Right"
            )

    Vdom.flexibleContent measure render
```

## Edge Cases and Constraints

### 1. Measurement Function Dishonesty

**Problem**: User provides a `measure` function that returns `MinWidth = 10` but the `render` function produces Vdom requiring width 50.

**Current Behavior**: The rendered Vdom will be measured with `MaxWidth = allocated width`. If allocated width is less than 50, the rendered Vdom's measurements will be clamped or it will overflow.

**Resolution**: Document that the `measure` function should provide accurate requirements. The rendered Vdom will be constrained to the allocated bounds regardless.

### 2. Structural Changes and Early Cutoff

**Problem**: Same `FlexibleContent` instance might render different structures in successive frames if the allocated bounds change.

**Resolution**: Early cutoff operates on the `FlexibleContent` node itself (checking if measure/render functions are referentially equal), not on the rendered children. If bounds change, the arrange phase will call `render` again.

### 3. Focus Persistence

**Problem**: If a FlexibleContent renders a focusable element at width 100 but not at width 50, what happens to focus when width changes from 100 to 50?

**Current Behavior**: The existing focus handling will detect that the focused key no longer exists in `focusableKeys` and clear focus.

**Recommendation**: Document this behavior. Users should use stable keys for focusable elements if they want focus to persist across structural changes.

### 4. Performance with Large Rendered Trees

**Problem**: If a FlexibleContent renders a large tree, frequent re-rendering could impact performance.

**Mitigation**:
- Content-based early cutoff (mandatory) prevents re-rendering when content hasn't changed
- Early cutoff propagates up through FlexibleContent just like any other container
- Users should ensure `measure` and `render` functions are stable (defined outside render loops) to enable reference equality checks
- If bounds don't change and `render` produces the same structure, zero additional work occurs

### 5. Nested FlexibleContent

**Problem**: Can a `render` function produce another FlexibleContent?

**Answer**: No, by type signature. The `render` function returns `KeylessVdom<DesiredBounds>`, and all FlexibleContent nodes carry `DesiredBounds` type parameter. This prevents nesting.

**Workaround**: If genuinely needed, compose in measurement space - have the outer FlexibleContent's `measure` function account for inner flexible behavior.

## Testing Strategy

### Unit Tests

1. **Basic rendering**: FlexibleContent that renders different text at different widths
2. **Measurement validation**: Ensure validation catches invalid measurements
3. **Focus handling**: FlexibleContent containing focusable elements
4. **Structural changes**: FlexibleContent that changes structure, verify focus is cleared
5. **Edge cases**: Zero-width bounds, zero-height bounds
6. **Early cutoff**: Verify re-rendering only occurs when needed

### Integration Tests

1. **ProgressBar component**: Verify rendering at various widths
2. **Responsive layouts**: Verify switching between horizontal/vertical layouts
3. **Nested in PanelSplit**: FlexibleContent as child of splits
4. **Nested in Bordered**: FlexibleContent inside borders
5. **Performance**: Large FlexibleContent trees with stable functions

## Migration Path

This is a purely additive change:
- No existing Vdom constructors change
- No breaking changes to measurement or arrangement algorithms
- Existing code continues to work unchanged

Users can adopt FlexibleContent incrementally for components that need region-dependent rendering.

## Future Enhancements

### Potential Enhancement: Constraint Callback

Instead of just `Rectangle`, provide more layout context:

```fsharp
type RenderContext = {
    Bounds: Rectangle
    ParentBounds: Rectangle
    TerminalBounds: Rectangle
}

render: (RenderContext -> KeylessVdom<DesiredBounds>)
```

This would allow components to make decisions based on global terminal size, not just local allocated space.

## Implementation Checklist

- [ ] **CRITICAL**: Add `VDomSource: KeylessVdom<DesiredBounds>` field to `ArrangedNode` type
- [ ] Update all existing `arrange` functions to populate `VDomSource` field
- [ ] Add `FlexibleContent` case to `UnkeyedVdom<'bounds>`
- [ ] Add `Vdom.flexibleContent` public API
- [ ] Implement `measureUnkeyed` case for FlexibleContent with validation
- [ ] Implement `arrange` case for FlexibleContent (ensuring VDomSource is populated correctly)
- [ ] Implement `arrangedToRendered` case for FlexibleContent with content-based early cutoff
- [ ] Implement `renderToBuffer` case for FlexibleContent (transparent container)
- [ ] Verify early cutoff propagates correctly through FlexibleContent nodes
- [ ] Add unit tests for basic functionality
- [ ] Add unit tests for edge cases
- [ ] Add unit tests specifically for early cutoff behavior
- [ ] Add integration tests for real components
- [ ] Document usage patterns and constraints
- [ ] Update examples with ProgressBar or similar component

## Open Questions

1. **Validation strategy**: Should invalid measurements throw or be clamped?
   - **Recommendation**: Clamp with debug warning

2. **Error propagation**: What if `render` throws an exception?
   - **Recommendation**: Let it propagate, same as any other Vdom construction error

3. **Performance impact of VDomSource**: Does storing the extra field on every ArrangedNode measurably impact memory or performance?
   - **Status**: Should be negligible, but profile if concerned
   - **Note**: This field improves the overall system's ability to perform early cutoff, potentially offsetting any memory cost

## Key Design Insights from Review

### The Cascading Failure Problem

The existing rendering pipeline achieves performance through reference-equality-based early cutoff. When `arrangedToRendered` determines nothing has changed in a subtree, it returns the exact same `RenderedNode` object from the previous frame. The render loop detects this via `Object.ReferenceEquals` and skips diffing that entire subtree.

Any node that always creates new `RenderedNode` objects breaks this optimization for all ancestors. This isn't just a local inefficiency - it cascades up the tree, forcing re-processing of the entire branch.

**Implication**: Content-based early cutoff for FlexibleContent is mandatory for system correctness, not an optimization.

### The Data Flow Impedance Mismatch

The original design had a fundamental data flow problem:

1. During `arrange`, FlexibleContent calls `render(bounds)` to produce `KeylessVdom<DesiredBounds>`
2. This DesiredBounds Vdom is measured and arranged to produce `ArrangedNode`
3. The original `ArrangedNode` type only stored `KeylessVdom<Rectangle>`, discarding the DesiredBounds source
4. During `arrangedToRendered`, we need the DesiredBounds Vdom for content comparison
5. **The data we needed was destroyed during arrangement**

**Solution**: Add `VDomSource: KeylessVdom<DesiredBounds>` to `ArrangedNode`. This:
- Preserves the DesiredBounds representation needed for content comparison
- Applies universally to all nodes, not just FlexibleContent
- May improve early cutoff opportunities across the entire system

### The Reference Equality Insight

Early cutoff works by propagating object identity up the tree:
1. Leaf nodes compare content and reuse previous `RenderedNode` if unchanged
2. Container nodes recursively process children
3. If all children return the same object references as previous frame, container reuses its previous `RenderedNode`
4. This propagates upward, creating large subtrees that skip re-rendering entirely

FlexibleContent must participate in this protocol. It can't "opt out" of early cutoff without breaking optimization for all ancestors.

## Summary

FlexibleContent provides a clean, type-safe mechanism for users to implement region-dependent rendering. The design:

- Maintains the two-pass measure/arrange architecture
- Prevents infinite regress through careful type constraints
- Integrates naturally with existing Vdom constructs via the new `VDomSource` field on `ArrangedNode`
- Preserves the reference-equality-based early cutoff optimization through mandatory content-based diffing
- Enables powerful new component patterns (progress bars, responsive layouts, adaptive tables)

The implementation requires changes in five locations:
1. ArrangedNode type definition (add VDomSource field)
2. All existing arrange functions (populate VDomSource)
3. Vdom type definition (add FlexibleContent variant)
4. Measurement logic (handle FlexibleContent case)
5. Rendering logic (handle FlexibleContent case with proper early cutoff)

The most significant change is the addition of `VDomSource` to `ArrangedNode`. While required for FlexibleContent, this field benefits the entire system by preserving the original DesiredBounds representation throughout the pipeline, potentially improving early cutoff opportunities system-wide.

All changes maintain backward compatibility in the public API, though internal pipeline code requires updates to populate the new field.
