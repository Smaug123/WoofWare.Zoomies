# Cell Styling and Component Refactor

## Overview

This document describes a refactor to add per-cell styling to text nodes and subsequently remove `Button` and `ToggleWithGlyph` from the core VDOM, reimplementing them as library functions in `Components`.

## Motivation

Currently, `Button` and `ToggleWithGlyph` are hardcoded as cases in `UnkeyedVdom`:

```fsharp
type UnkeyedVdom =
    | Text of string
    | Button of label: string * isFocused: bool * isPressed: bool
    | ToggleWithGlyph of unchecked: char * checked: char * isChecked: bool * isFocused: bool
    | PanelSplit of ...
    | ...
```

This violates the design goal of having all primitives be user-accessible. Users cannot create their own button-like components with custom visual feedback without either:
1. Requesting a new VDOM case be added to the framework
2. Accepting that they can't use terminal styling (colours, inversion)

The root cause is that `Text` has no styling — it's just a string. The framework must therefore bake styling logic into dedicated primitives.

## Design Goals

- **User-accessible styling**: Applications can apply foreground/background colours to any text.
- **Remove special cases**: `Button` and `ToggleWithGlyph` become library code, not VDOM primitives.
- **Minimal API surface**: Only leaf-level styling; no inherited styles.
- **Backward compatible**: Existing code continues to work, possibly with deprecation warnings.

## Core Types

### CellStyle

```fsharp
/// Visual styling for a terminal cell.
[<Struct>]
type CellStyle =
    {
        /// Foreground (text) colour. None means terminal default.
        Foreground : ConsoleColor voption
        /// Background colour. None means terminal default.
        Background : ConsoleColor voption
    }

[<RequireQualifiedAccess>]
module CellStyle =
    /// No explicit styling; uses terminal defaults.
    let none : CellStyle =
        { Foreground = ValueNone; Background = ValueNone }

    /// Inverted colours (light text on dark background or vice versa).
    /// Note: actual effect depends on terminal configuration.
    let inverted : CellStyle =
        { Foreground = ValueSome ConsoleColor.Black
          Background = ValueSome ConsoleColor.White }

    /// Set foreground colour only.
    let withForeground (c : ConsoleColor) (s : CellStyle) : CellStyle =
        { s with Foreground = ValueSome c }

    /// Set background colour only.
    let withBackground (c : ConsoleColor) (s : CellStyle) : CellStyle =
        { s with Background = ValueSome c }
```

Using `ConsoleColor` directly ties us to the .NET console API, but that's already the case throughout the framework. A future abstraction layer could map to a richer colour model.

### TerminalCell Update

The existing `TerminalCell` type already has colour fields:

```fsharp
type TerminalCell =
    {
        Char : char
        BackgroundColor : ConsoleColor voption
        TextColor : ConsoleColor voption
    }
```

No changes needed to `TerminalCell` itself.

## VDOM Changes

### Option A: Extend Text to Carry Style

```fsharp
type UnkeyedVdom =
    | Text of content: string * style: CellStyle
    | PanelSplit of ...
    | Bordered of ...
    | Empty
    // Button and ToggleWithGlyph REMOVED
```

Update `Vdom.text`:

```fsharp
module Vdom =
    /// Creates a text node with default styling.
    static member text (content : string) : Vdom<DesiredBounds, Unkeyed> =
        Vdom.Unkeyed (UnkeyedVdom.Text (content, CellStyle.none), Teq.refl)

    /// Creates a text node with explicit styling.
    static member styledText (content : string) (style : CellStyle) : Vdom<DesiredBounds, Unkeyed> =
        Vdom.Unkeyed (UnkeyedVdom.Text (content, style), Teq.refl)
```

### Option B: Separate StyledText Case

```fsharp
type UnkeyedVdom =
    | Text of string
    | StyledText of content: string * style: CellStyle
    | PanelSplit of ...
    | ...
```

This preserves the simple `Text` case for unstyled content, potentially enabling some optimisations.

**Recommendation**: Option A. The `CellStyle.none` value is a singleton struct with no allocation, so there's no meaningful overhead. Having two text cases complicates rendering and measurement logic for no real benefit.

## Rendering Changes

Update the text rendering logic in `Render.fs`:

```fsharp
| UnkeyedVdom.Text (content, style) ->
    clearBoundsWithSpaces dirty bounds
    if bounds.Width > 0 && bounds.Height > 0 then
        let centerY = bounds.Height / 2
        let startX = max 0 ((bounds.Width - content.Length) / 2)
        let mutable x = startX

        for ch in content do
            if x < bounds.Width then
                let cell =
                    {
                        Char = ch
                        BackgroundColor = style.Background
                        TextColor = style.Foreground
                    }
                setAtRelativeOffset dirty bounds x centerY (ValueSome cell)
                x <- x + 1
```

The existing Button rendering logic already does colour handling; this generalises it.

## Component Library

### Button

```fsharp
[<RequireQualifiedAccess>]
type Button =
    /// Creates a button with automatic focus and activation visual state.
    static member make
        (ctx : VdomContext, key : NodeKey, label : string,
         ?isFirstToFocus : bool, ?isInitiallyFocused : bool)
        =
        let isFocused = VdomContext.focusedKey ctx = Some key
        let isPressed = VdomContext.wasRecentlyActivated key ctx

        let leftBracket, rightBracket =
            match isFocused, isPressed with
            | true, true -> "[*", "*]"
            | true, false -> "[[", "]]"
            | false, true -> " *", "* "
            | false, false -> "[ ", " ]"

        let content = $"{leftBracket} {label} {rightBracket}"
        let style = if isPressed then CellStyle.inverted else CellStyle.none

        Vdom.styledText content style
        |> Vdom.withTag "button"  // If tagging is enabled
        |> Vdom.withKey key
        |> Vdom.withFocusTracking (
            ?isFirstToFocus = isFirstToFocus,
            ?isInitiallyFocused = isInitiallyFocused
        )
```

### Checkbox

```fsharp
[<RequireQualifiedAccess>]
type Checkbox =
    /// Creates a checkbox with automatic focus state.
    static member make
        (ctx : VdomContext, key : NodeKey, isChecked : bool,
         ?isFirstToFocus : bool, ?isInitiallyFocused : bool)
        =
        let isFocused = VdomContext.focusedKey ctx = Some key
        let glyph = if isChecked then '☑' else '☐'

        let content =
            if isFocused then $"[{glyph}]"
            else string glyph

        Vdom.text content
        |> Vdom.withTag "checkbox"
        |> Vdom.withKey key
        |> Vdom.withFocusTracking (
            ?isFirstToFocus = isFirstToFocus,
            ?isInitiallyFocused = isInitiallyFocused
        )
```

### ToggleWithGlyph (Generalised)

```fsharp
[<RequireQualifiedAccess>]
type Toggle =
    /// Creates a toggle with custom glyphs and automatic focus state.
    static member make
        (ctx : VdomContext, key : NodeKey,
         uncheckedGlyph : char, checkedGlyph : char, isChecked : bool,
         ?isFirstToFocus : bool, ?isInitiallyFocused : bool)
        =
        let isFocused = VdomContext.focusedKey ctx = Some key
        let glyph = if isChecked then checkedGlyph else uncheckedGlyph

        let content =
            if isFocused then $"[{glyph}]"
            else string glyph

        Vdom.text content
        |> Vdom.withTag "toggle"
        |> Vdom.withKey key
        |> Vdom.withFocusTracking (
            ?isFirstToFocus = isFirstToFocus,
            ?isInitiallyFocused = isInitiallyFocused
        )
```

## Migration Path

### Phase 1: Add CellStyle (Non-Breaking)

1. Add `CellStyle` type
2. Add `Vdom.styledText` method
3. Update `Text` case to carry `CellStyle` internally
4. Update rendering to use style
5. Existing `Vdom.text` calls continue to work (they get `CellStyle.none`)

### Phase 2: Add New Components (Non-Breaking)

1. Add `Button.make`, `Checkbox.make`, `Toggle.make` to `Components` module
2. These use `Vdom.styledText` internally
3. Document as the preferred API

### Phase 3: Deprecate Old Primitives

1. Mark `Vdom.button`, `Vdom.checkbox`, `Vdom.toggleWithGlyph` as `[<Obsolete>]`
2. Point users to `Components.*` equivalents

### Phase 4: Remove Old Primitives (Breaking)

1. Remove `Button` and `ToggleWithGlyph` from `UnkeyedVdom`
2. Remove deprecated `Vdom.*` methods
3. Major version bump

Given the framework is pre-1.0, phases 3-4 could be collapsed: just remove the old primitives directly.

## Measurement Implications

Button and ToggleWithGlyph currently report specific size preferences during measurement. When they become library code calling `Vdom.styledText`, this still works correctly — the text content determines the measurement.

```fsharp
// Button "Submit" with focus brackets becomes:
// "[[ Submit ]]" — 12 characters
// The Text node measures this correctly
```

No changes to measurement logic required.

## What Remains in UnkeyedVdom

After the refactor:

```fsharp
type UnkeyedVdom =
    | Text of content: string * style: CellStyle
    | PanelSplit of direction: SplitDirection * behaviour: SplitBehaviour * c1: KeylessVdom * c2: KeylessVdom
    | Bordered of inner: KeylessVdom
    | Empty
```

These are the true structural primitives:
- **Text**: Styled characters (the only leaf that displays content)
- **PanelSplit**: Spatial subdivision (the only way to compose children)
- **Bordered**: Visual decoration requiring multiple regions
- **Empty**: The unit of VDOM

Everything else (Button, Checkbox, Toggle, future ListBox, etc.) is library code built from these primitives.

## Open Questions

### Should Bordered Also Become Library Code?

Bordered is five regions: top, bottom, left, right, center. It could theoretically be implemented as four PanelSplits plus styled text for the border characters.

However:
1. The nesting would be complex and error-prone
2. Border character selection (corners, edges) has fiddly logic
3. Bordered is genuinely structural, not just "styled text"

Recommendation: Keep Bordered as a primitive. It's not "styling applied to content" — it's "content embedded in a decorative frame", which is a different concern.

### Per-Character Styling?

The current design applies one style to an entire text string. Should we support per-character styling?

```fsharp
type StyledSpan = { Text: string; Style: CellStyle }
| RichText of StyledSpan list
```

This would enable syntax highlighting, inline formatting, etc. However:
1. Significantly more complex
2. Measurement becomes harder (spans might wrap)
3. Not needed for Button/Checkbox use case

Recommendation: Defer. If needed, add `Vdom.richText` later without removing `Vdom.styledText`.

### Alignment Within Text Bounds

Currently, text is centered horizontally and vertically within its bounds. Should styling interact with alignment?

No — alignment is a layout concern handled during rendering. Style is purely about colours. Keep them separate.

## Summary

| Change | Type |
|--------|------|
| Add `CellStyle` struct | New type |
| Add `Vdom.styledText` | New API |
| Modify `Text` to carry `CellStyle` | Internal change |
| Add `Components.Button.make` | New API |
| Add `Components.Checkbox.make` | New API |
| Add `Components.Toggle.make` | New API |
| Remove `UnkeyedVdom.Button` | Breaking (defer or do immediately) |
| Remove `UnkeyedVdom.ToggleWithGlyph` | Breaking (defer or do immediately) |

The result is a smaller, more orthogonal VDOM with styling as a user-accessible primitive rather than baked into specific components.
