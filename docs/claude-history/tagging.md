# VDOM Tagging System

## Overview

This document describes a lightweight tagging mechanism for VDOM nodes, enabling semantic inspection and debugging without affecting rendering. Tags are string annotations that describe what a VDOM node *represents* (e.g., "button", "checkbox", "login-form") as opposed to what it *is* structurally (Text, PanelSplit, etc.).

## Motivation

When components like Button are implemented as library functions rather than VDOM primitives, the semantic information is lost:

```fsharp
// Before: VDOM knows this is a Button
UnkeyedVdom.Button ("Submit", isFocused, isPressed)

// After: VDOM just sees styled text
Vdom.text "[ Submit ]" CellStyle.inverted
```

This makes debugging and testing harder. A `debugDump` of the VDOM tree shows only structural primitives, not the higher-level components the user actually wrote.

Tags restore this information without coupling the VDOM to specific component types.

## Design Goals

- **Zero-cost when disabled**: Production builds should pay nothing for tagging infrastructure.
- **Simple API**: String tags with no complex inheritance or query semantics.
- **Non-invasive**: Tags have no effect on layout, rendering, or equality comparisons.
- **Composable**: Multiple tags can be attached to a single node.

## Core Types

```fsharp
/// Global configuration for the tagging system.
[<RequireQualifiedAccess>]
module VdomTagging =
    /// When false, Vdom.withTag is a no-op and no allocations occur.
    /// Defaults to false. Set to true before constructing VDOMs to enable tagging.
    val mutable Enabled : bool

/// Stored in VDOM nodes. When tagging is disabled, this is always Empty.
type VdomTags = private VdomTags of ImmutableArray<string>

[<RequireQualifiedAccess>]
module VdomTags =
    /// The empty tag collection. This is a singleton; no allocation occurs.
    val empty : VdomTags

    /// Add a tag, returning a new collection. If VdomTagging.Enabled is false,
    /// returns the input unchanged (no allocation).
    val add : string -> VdomTags -> VdomTags

    /// Get all tags as a sequence.
    val toSeq : VdomTags -> string seq

    /// Check if a specific tag is present.
    val contains : string -> VdomTags -> bool
```

## VDOM Integration

The `VdomTags` field is added to the VDOM representation. The exact placement depends on implementation details, but conceptually:

```fsharp
type Vdom<'bounds, 'keyed> =
    private
    | Unkeyed of UnkeyedVdom * Teq<'keyed, Unkeyed> * VdomTags
    | Keyed of KeyedVdom * Teq<'keyed, Keyed> * VdomTags
```

All existing VDOM constructors initialise tags to `VdomTags.empty`.

### API Addition

```fsharp
module Vdom =
    /// Attach a semantic tag to this node. Tags are metadata only and do not
    /// affect rendering or layout.
    ///
    /// If VdomTagging.Enabled is false, this is a no-op returning the input unchanged.
    val withTag : string -> Vdom<'bounds, 'keyed> -> Vdom<'bounds, 'keyed>

    /// Read the tags attached to this node.
    val tags : Vdom<'bounds, 'keyed> -> VdomTags
```

## Implementation Notes

### Zero-Cost When Disabled

The key insight is that `ImmutableArray<'T>.Empty` is a singleton — accessing it allocates nothing. When `VdomTagging.Enabled` is false:

```fsharp
module VdomTags =
    let add (tag : string) (tags : VdomTags) : VdomTags =
        if not VdomTagging.Enabled then
            tags  // No-op: return input unchanged
        else
            let (VdomTags arr) = tags
            VdomTags (arr.Add tag)
```

Since all VDOM nodes start with `VdomTags.empty`, and `add` is a no-op when disabled, no tag-related allocations ever occur in production.

### Equality and Hashing

Tags are explicitly excluded from VDOM equality and hash calculations. Two VDOMs that differ only in tags are considered equal. This ensures that adding debug tags doesn't break caching or early-cutoff optimisation.

```fsharp
// Equality ignores tags
let vdomEquals (a : Vdom<_,_>) (b : Vdom<_,_>) : bool =
    // Compare structural content only, not tags
    ...
```

### No Inheritance

Tags are per-node only. A PanelSplit with tag "form" does not cause its children to have tag "form". This avoids the complexity of inherited styling and keeps the semantics trivial.

If users want hierarchical tagging, they can encode it in the strings themselves:

```fsharp
Vdom.text "Submit" style
|> Vdom.withTag "button"
|> Vdom.withTag "form/submit-button"  // User convention, not framework feature
```

## Debug Dump Format

A helper function for debugging:

```fsharp
module Vdom =
    /// Produce a human-readable tree representation of the VDOM structure,
    /// including tags and keys.
    val debugDump : Vdom<'bounds, 'keyed> -> string
```

Example output:

```
PanelSplit Horizontal Auto
├── [button, primary-action] Keyed "submit": Text "[ Submit ]"
└── [button] Keyed "cancel": Text "[ Cancel ]"
```

The format shows:
- Structural node type
- Tags in square brackets (if any)
- Key (if keyed)
- Leaf content summary

## Usage Example

```fsharp
// In application startup, enable tagging for debug builds
#if DEBUG
VdomTagging.Enabled <- true
#endif

// In component library
module Components =
    let button ctx key label =
        let isFocused = VdomContext.focusedKey ctx = Some key
        let isPressed = VdomContext.wasRecentlyActivated key ctx

        let brackets = if isFocused then "[[", "]]" else "[ ", " ]"
        let style = if isPressed then CellStyle.inverted else CellStyle.defaultStyle

        Vdom.text $"{fst brackets} {label} {snd brackets}" style
        |> Vdom.withTag "button"
        |> Vdom.withKey key
        |> Vdom.withFocusTracking

// In tests
[<Test>]
let ``login form has submit button`` () =
    VdomTagging.Enabled <- true
    let vdom = LoginForm.view ctx state
    let dump = Vdom.debugDump vdom
    Assert.That(dump, Does.Contain "[button]")
    Assert.That(dump, Does.Contain "submit")
```

## Future Considerations

### Structured Tags

The current design uses plain strings. A future iteration might support structured tags:

```fsharp
type VdomTag =
    | Component of name: string
    | Role of string  // Accessibility role
    | TestId of string
    | Custom of string
```

This would enable typed queries and better tooling. However, it's unclear what structure would actually be useful, so starting with strings allows the patterns to emerge from usage.

### Query API

A more sophisticated API might support queries like "find all nodes with tag X":

```fsharp
module Vdom =
    val findByTag : string -> Vdom<'bounds, 'keyed> -> VdomQuery list
```

This is complicated by the type parameters — child nodes have different `'bounds` and `'keyed` types. Punting on this for now; `debugDump` plus string matching handles the immediate debugging use case.

### Automatic Tagging

Could the framework automatically tag nodes based on which `Vdom.*` constructor created them? E.g., `Vdom.text` automatically adds tag "text"?

Probably not worth it — the structural type is already visible in `debugDump`. The value of tags is *semantic* information that only the user knows.

## Summary

| Aspect | Decision |
|--------|----------|
| Tag type | `string` |
| Multiplicity | Multiple tags per node |
| Inheritance | None (per-node only) |
| Equality | Tags excluded from VDOM equality |
| Zero-cost | Via global `Enabled` flag and singleton empty array |
| Query API | Just `debugDump` for now |

The design prioritises simplicity and zero runtime cost when disabled, deferring more sophisticated features until usage patterns clarify what's actually needed.
