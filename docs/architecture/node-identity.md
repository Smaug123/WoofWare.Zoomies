# Query-Based Node Identity

*(Mostly drafted by Claude as the result of a long design discussion. As of commit 2908a0732989cb775224ab950eb9e542c748ed19, this was the intended future design.)*

## Problem Statement

In an immediate-mode UI framework, the VDOM is reconstructed afresh each frame.
This creates a challenge for managing focus, and more generally for any operation that considers elements to persist over time:

1. **Dependency cycle**: Focus state affects how we render (focused elements look different), but we need the rendered tree to determine what can be focused. A careless implementation would introduce a circular dependency here.
2. **Semantic identity**: The framework cannot automatically determine that "this checkbox in the new VDOM" is "the same" checkbox as last frame when tree structure changes. As far as the framework is concerned, in the worst case, the entire VDOM is new even if it is composed of identical components.
3. **Boilerplate vs. flexibility**: We want to avoid forcing identity management on all components, while still supporting complex dynamic UIs.

## Design Principles

1. **Identity is opt-in**: Most components don't need stable identity and shouldn't pay for it.
2. **Identity is separate from state**: The framework provides identity primitives; applications manage their own state, and project the state into a VDOM, only handing identities where the information is actually relevant.
3. **Query-based, not callback-based**: The framework maintains focus state and other metadata; applications query it to make rendering decisions.
4. **Primitive foundation**: Low-level tools should enable clean high-level libraries without baking in opinions.

## Architecture

We use focus-handling as the motivating example, but the same mechanism can in principle handle other metadata.

### Core Types

```fsharp
type NodeKey = private NodeKey of string

module NodeKey =
    val make : string -> NodeKey
```

Node keys are opaque identifiers that applications can attach to VDOM nodes to give them stable identity across frames.

### Framework API

```fsharp
type Keyed = private | Keyed
type Unkeyed = private | Unkeyed

module Vdom =
    /// Attach a stable key to a VDOM node
    val withKey : NodeKey -> Vdom<'bounds, 'keyed> -> Vdom<'bounds, Keyed>

    /// Mark a node as participating in automatic focus tracking
    val withFocusTracking : Vdom<'bounds, Keyed> -> Vdom<'bounds, Keyed>

module RenderState =
    /// Query which key had focus in the previous frame
    val focusedKey : RenderState -> NodeKey option

    /// Query the rendered bounds of a keyed node
    val layoutOf : NodeKey -> RenderState -> Rectangle option
```

### How It Works

1. **VDOM Construction**: Application queries `RenderState.focusedKey` to determine what was focused last frame, and uses this to construct the VDOM with appropriate styling/state.

2. **Rendering**: The framework walks the VDOM tree, noting the tree position of all `withFocusTracking` nodes and any explicit `withKey` annotations.

3. **Input Processing**: Tab key cycles through `withFocusTracking` nodes in tree order. The framework updates its internal "which key is focused" state.

4. **Next Frame**: The cycle repeats with the updated focus state available for querying.

### Identity Resolution

Focus can be tracked only by explicit keys.
The API of `Vdom.withFocusTracking` requires (with the phantom `'keyed` parameter) that the node be keyed.

## Usage Examples

### Absolutely DIY: Manual Focus Management

At the very lowest level, the user is free to handle focus entirely themselves.
They need not query the `focusedKey` at all, but instead can track incoming keystrokes and maintain their own focus knowledge.
They don't need to `withKey` any node.

```fsharp
let render (state: State) (renderState: RenderState) =
    let currentFocus = RenderState.focusedKey renderState

    let checkbox1 =
        Vdom.checkbox
            // `state.Focus` got updated in the WorldProcessor loop:
            // the user identified a Tab key coming in, and used it
            // to update the value of their user-controlled Focus field.
            (state.Focus = '1')
            state.IsChecked1

    let checkbox2 =
        Vdom.checkbox
            (state.Focus = '1')
            state.IsChecked2

    Vdom.panelSplit Direction.Vertical checkbox1 checkbox2
```

### Low-Level: Framework-handled Focus Management

```fsharp
let render (state: State) (renderState: RenderState) =
    let currentFocus = RenderState.focusedKey renderState

    let checkbox1Key = NodeKey.make "checkbox-1"
    let checkbox1 =
        Vdom.checkbox
            (currentFocus = Some checkbox1Key)
            state.IsChecked1
        |> Vdom.withKey checkbox1Key
        |> Vdom.withFocusTracking

    let checkbox2Key = NodeKey.make "checkbox-2"
    let checkbox2 =
        Vdom.checkbox
            (currentFocus = Some checkbox2Key)
            state.IsChecked2
        |> Vdom.withKey checkbox2Key
        |> Vdom.withFocusTracking

    Vdom.panelSplit Direction.Vertical checkbox1 checkbox2
```

### High-Level: Focus Manager Library

*This section is concept art only; it hasn't been coded up anywhere, and is pure vibes from Claude Sonnet 4.5. Don't expect it to stay like this.*

```fsharp
type FocusManager<'id when 'id : comparison> =
    private {
        mutable Current: 'id option
        IdToKey: 'id -> NodeKey
        KeyToId: Map<NodeKey, 'id>
    }

    member this.Sync(renderState: RenderState) =
        match RenderState.focusedKey renderState with
        | Some key ->
            this.Current <- Map.tryFind key this.KeyToId
        | None ->
            this.Current <- None

    member this.IsFocused(id: 'id) =
        this.Current = Some id

    member this.Checkbox(id: 'id, isChecked: bool) =
        let key = this.IdToKey id
        let isFocused = this.IsFocused id
        Vdom.checkbox isFocused isChecked
        |> Vdom.withKey key
        |> Vdom.withFocusTracking

// Usage
let focusManager = FocusManager.create ["checkbox1"; "checkbox2"]

let render state renderState =
    focusManager.Sync(renderState)

    Vdom.panelSplit Direction.Vertical
        (focusManager.Checkbox("checkbox1", state.IsChecked1))
        (focusManager.Checkbox("checkbox2", state.IsChecked2))
```

## Trade-offs

### Chosen Approach: Query-Based Identity

**Advantages:**
- Identity is opt-in and pay-for-what-you-use
- Clear separation between framework primitives and application state
- Enables sophisticated libraries while keeping core simple
- No hidden magic or implicit state threading

**Disadvantages:**
- Requires explicitly querying previous frame's state during VDOM construction
- User must understand the identity system for dynamic UIs
- Some boilerplate at the low level (mitigated by libraries)

### Alternative: Callback-Based

(This was the first implementation of focus, during the initial proof of concept of WoofWare.Zoomies, as of commit 2908a0732989cb775224ab950eb9e542c748ed19.)

**Advantages:**
- No need for explicit identity primitives
- Simple mental model: "tell me when this is focused"

**Disadvantages:**
- Forces user to maintain focus state in application code
- User must manually map framework events to semantic identity
- Conflates framework state management with application state
- No way to query layout or other node-specific information

### Alternative: Baked-In Identity (React-style)

**Advantages:**
- Framework handles all identity management
- Minimal boilerplate

**Disadvantages:**
- Forces identity on all components (performance and mental-model cost)
- Opinionated about state management
- Harder to build alternative patterns on top
- Against the "small set of primitives" philosophy

## Future Extensions

This identity system can be extended to support:

- **Animation**: Query previous frame's layout to compute smooth transitions
- **Scroll state**: Preserve scroll position across rerenders
- **Text input**: Maintain cursor position in text fields
- **Custom state**: Applications can maintain their own keyed state dictionaries

The key insight is that node identity is a general primitive, not specific to focus management.

## Implementation Notes

### RenderState Changes

```fsharp
type RenderState =
    private {
        // ... existing fields ...
        KeyToNode: Dictionary<NodeKey, RenderedNode>
        mutable FocusedKey: NodeKey option
    }
```

During layout, track all keyed nodes.
During input processing, update `FocusedKey` when Tab is pressed or appropriately-located mouse clicks occur.

### Tab Navigation

Tab cycles through `withFocusTracking` nodes in tree order (depth-first traversal).
The framework doesn't need to understand semantic meaning; it just walks to "the next `withFocusTracking` thing in the tree."
