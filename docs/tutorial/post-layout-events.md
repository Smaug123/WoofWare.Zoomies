---
title: Post-Layout Events
category: Tutorial
categoryindex: 1
index: 1
---

# Post-layout events

There are several motivating examples for post-layout events.
In this tutorial we'll examine *virtualised selection-lists*.
There is also a how-to on [being unable to deal with a render](../how_to/emergency-rerender.md), which uses post-layout events.

## Problem statement

Some VDOM components depend intrinsically on their layout information in order to know what is being displayed.
One such component is a virtualised list, which needs to know the set of elements that turned out to be visible on screen during the last render so as to keep that set stable over time.

The WoofWare.Zoomies render flow has a [strict order to its event loop](../explanation/render-loop.md) in which there is only one way for that information to flow back to the user state that will persist to the next tick.

In this tutorial, we'll see how to implement the virtualised list: we'll essentially step through the `MultiSelection` component which comes out of the box in `WoofWare.Zoomies.Components`.

## Why does a virtualised list need post-layout events?

A virtualised list which wants to display 100 rows may find out at the start of the "render" phase that it's actually only got space for 5 rows.
Suppose our UI has a mode where it closes the panel displaying the list, displays something else for a while, and then reopens the panel, and it wants to keep the list looking the same when it gets reopened.
Then the user state has to know what elements were on screen so that it can ensure the same ones reappear.

But the list is virtualised: we can only know what elements were on screen *after* layout is complete and we know what space was available to render into.

That means we have to propagate the knowledge back to the user state: we can't just rely on the VDOM to get this right by itself, because user state is the only way information can persist between renders.

## Implementing the `MultiSelection` component: the shell without post-layout events

*This section just sets up the multi-selection component; it doesn't handle post-layout events yet, so it doesn't yet solve the problem. See the next section for that.*

The component is a multiple-choice checkbox list with a cursor indicating which checkbox has user focus right now.
We'll model every row as its own VDOM node.

User interaction: we'll have the *entire* component be a singleton for the purposes of [automatic focus handling](../how_to/automatic-focus.md).
That lets us hook up directional up/down arrows to move focus within the component, reserving the automatic focus handling's Tab character for moving out of the list entirely.

To keep it simple, we'll omit labels and will just display a list of checkboxes.
In real life, you might want to show labels, and e.g. consider letting row labels wrap onto more than one line.

### Design

Since we can't know even the structure of what we're rendering until we know what space we're rendering into, the component *must* be `Vdom.flexibleContent`, which means we need a `measure` function and a `render` function.

Once we know what we're rendering, we'll treat the whole thing as a `Table` (which is overkill for this example, but any extension at all will want to be a `Table`, so we'll just do that from the start).

### The `measure` function

During [the "measure" phase](../explanation/render-loop.md), WoofWare.Zoomies asks each component how much space it's going to need;
we need to tell the framework our size.

```fsharp
let totalItems : int = failwith "This is computed from the user's parameters to our component"

let measure (constraints : MeasureConstraints) : MeasuredSize =
    {
        // One checkbox takes up to 3 characters if we include the `[]` focus marker
        MinWidth = 3
        // Ask for as much width as possible; not necessary for this example,
        // but in real life we have labels to display too
        PreferredWidth = constraints.MaxWidth
        // We don't have a size cap.
        MaxWidth = None
        // Always try and show at least one item...
        MinHeightForWidth = 1
        // ... but we'd prefer to show every item!
        PreferredHeightForWidth = fun _ -> totalItems
        // And we have no need for any more height than "one per row",
        // since we're not accommodating wrapping here.
        MaxHeightForWidth = fun _ -> Some totalItems
    }
```

### The `render` function

Now we need to build a VDOM given that we know the bounds into which it's rendering.

```fsharp
let items = failwith "probably derived from user's parameters to our component: some description of what is displayed on each row"
let cursor : int = failwith "derive a cursor from user state; cursor is an index into `items`"
let selectedItems : int Set = failwith "derive from user state; these are indices into `items`"

let render (bounds : Rectangle) : Vdom<DesiredBounds> =
    let viewportHeight = bounds.Height

    // The cursor must always be displayed, hence the `min cursor ...`.
    let indexOfFirstRow = min cursor (max 0 (totalItems - viewportHeight))
    let visibleRowCount = min viewportHeight (totalItems - indexOfFirstRow)

    if visibleRowCount <= 0 then
        Vdom.empty
    else

    let visibleRows = items.[indexOfFirstRow..indexOfFirstRow + visibleRowCount - 1]

    // The Table component keys its cells, so we need to provide a key
    // it can use to derive the cell keys.
    let tableKey = failwith "make a key from user's parameters to our component"

    // Table cells are a ragged array of `Vdom`s.
    let cells =
        visibleRows
        |> Array.mapi (fun visibleIndex cell ->
            let indexInItems = visibleIndex + indexOfFirstRow
            let cursorIsHere = indexInItems = cursor
            let itemIsSelected = selectedItems.Contains indexInItems

            // We don't need the checkboxes to participate in automatic focus tracking,
            // since we'll be handling that ourselves with up/down arrows rather than Tab;
            // so we use `make'` instead of `make`.
            let checkbox = Checkbox.make' (isChecked = itemIsSelected, isFocused = cursorIsHere)
            // If you were labelling the checkboxes, you'd add an extra element to this array for the label.
            [| checkbox |]
        )

    // Just let each table row display however it wants to.
    let rowSpecs = [||]

    // The first column should always have width 3, for the checkbox.
    let colSpecs = [| Column.Fixed 3 |]

    Table.make tableKey cells colSpecs rowSpecs
```

### The VDOM component

This is a call to `Vdom.flexibleContent`.

```fsharp
let checkboxListKey : NodeKey = failwith "from user parameters to the component"

let content = Vdom.flexibleContent measure render
// The entire checkbox-list needs to be able to obtain focus from the
// automatic focus handling mechanism.
let focusable = content |> Vdom.withKey checkboxListKey

Vdom.withFocusTracking focusable
```

### The overall code

```fsharp
// we use `unit` for the item type because we don't display
// anything other than a checkbox; in real life you'll probably
// have more to display!
// `cursor` and `selectedItems` should be obtained from user state;
// often also `items`, if the list has contents which aren't known at app startup time.
let make (key : NodeKey) (cursor : int) (selectedItems : int Set) (items : unit[]) : Vdom<DesiredBounds> =
    let totalItems = Seq.length items

    let measure (constraints : MeasureConstraints) : MeasuredSize =
        {
            // One checkbox takes up to 3 characters if we include the `[]` focus marker
            MinWidth = 3
            // Ask for as much width as possible; not necessary for this example,
            // but in real life we have labels to display too
            PreferredWidth = constraints.MaxWidth
            // We don't have a size cap.
            MaxWidth = None
            // Always try and show at least one item...
            MinHeightForWidth = 1
            // ... but we'd prefer to show every item!
            PreferredHeightForWidth = fun _ -> totalItems
            // And we have no need for any more height than "one per row",
            // since we're not accommodating wrapping here.
            MaxHeightForWidth = fun _ -> Some totalItems
        }

    let render (bounds : Rectangle) : Vdom<DesiredBounds> =
        let viewportHeight = bounds.Height

        // The cursor must always be displayed, hence the `min cursor ...`.
        let indexOfFirstRow = min cursor (max 0 (totalItems - viewportHeight))
        let visibleRowCount = min viewportHeight (totalItems - indexOfFirstRow)

        if visibleRowCount <= 0 then
            Vdom.empty
        else

        let visibleRows = items.[indexOfFirstRow..indexOfFirstRow + visibleRowCount - 1]

        // Derive a child key for the inner table, so it doesn't collide with
        // the parent's key.
        let tableKey = key |> NodeKey.child (NodeKeySegment.make "table")

        // Table cells are a ragged array of `Vdom`s.
        let cells =
            visibleRows
            |> Array.mapi (fun visibleIndex cell ->
                let indexInItems = visibleIndex + indexOfFirstRow
                let cursorIsHere = indexInItems = cursor
                let itemIsSelected = selectedItems.Contains indexInItems

                // We don't need the checkboxes to participate in automatic focus tracking,
                // since we'll be handling that ourselves with up/down arrows rather than Tab;
                // so we use `make'` instead of `make`.
                let checkbox = Checkbox.make' (isChecked = itemIsSelected, isFocused = cursorIsHere)
                // If you were labelling the checkboxes, you'd add an extra element to this array for the label.
                [| checkbox |]
            )

        // Just let each table row display however it wants to.
        let rowSpecs = [||]

        // The first column should always have width 3, for the checkbox.
        let colSpecs = [| Column.Fixed 3 |]

        Table.make tableKey cells colSpecs rowSpecs

    let content = Vdom.flexibleContent measure render
    // The entire checkbox-list needs to be able to obtain focus from the
    // automatic focus handling mechanism.
    let focusable = content |> Vdom.withKey key

    Vdom.withFocusTracking focusable
```

## Implementing post-layout events

Now that we have the basic display of a virtualised list, we can start the tutorial proper.

The problem with the above implementation is that user state never knows what was on screen: it can't know `indexOfFirstRow`, which means the information can't be persisted across renders.
User state only knows a cursor, and this isn't enough to keep the display stable over time.

The fix is to communicate this information back to the user state:

```fsharp
type MyPostLayoutEvent =
    | MultiSelectSize of NodeKey * indexOfFirstRow : int * visibleRowCount : int

let vdomContext : IVdomContext<MyPostLayoutEvent> = failwith "the framework passes this in to VDOM construction"
let checkboxListKey : NodeKey = failwith "the user passed this in to VDOM construction"

let render (bounds : Rectangle) =
    let viewportHeight = bounds.Height

    // The cursor must always be displayed, hence the `min cursor ...`.
    let indexOfFirstRow = min cursor (max 0 (totalItems - viewportHeight))
    let visibleRowCount = min viewportHeight (totalItems - indexOfFirstRow)

    // NEW!
    vdomContext.PostLayoutEvent (MyPostLayoutEvent.MultiSelectSize (checkboxListKey, indexOfFirstRow, visibleRowCount))

    if visibleRowCount = 0 then
        // continue as before
```

To *use* this information, we handle the post-layout event in `WorldProcessor` by updating user state.

```fsharp
type MyUserState =
    {
        // The cursor position in the list
        Cursor : int
        // Which items are selected
        SelectedItems : int Set
        // Viewport information from the last render - this is what we're persisting!
        IndexOfFirstVisibleRow : int
        VisibleRowCount : int
    }

let processWorld (worldBridge : IWorldBridge<MyAppEvent>) =
    { new WorldProcessor<MyAppEvent, MyPostLayoutEvent, MyUserState> with
        member _.ProcessWorld (changes, prevVdomContext, state) =
            // Handle user input events (keystrokes, etc.) here;
            // this is where we would handle e.g. the up/down keystrokes
            // that move focus within the list
            ProcessWorldResult.make state

        member _.ProcessPostLayoutEvents (events : ReadOnlySpan<MyPostLayoutEvent>, _ctx : IVdomContext, state : MyUserState) : MyUserState =
            let mutable indexOfFirst = state.IndexOfFirstVisibleRow
            let mutable visibleCount = state.VisibleRowCount

            for event in events do
                match event with
                | MyPostLayoutEvent.MultiSelectSize (key, newIndexOfFirst, newVisibleCount) ->
                    indexOfFirst <- newIndexOfFirst
                    visibleCount <- newVisibleCount

            { state with
                IndexOfFirstVisibleRow = indexOfFirst
                VisibleRowCount = visibleCount
            }
    }
```

Now we can update the `render` function to use the persisted viewport information:

```fsharp
let render (bounds : Rectangle) : Vdom<DesiredBounds> =
    let viewportHeight = bounds.Height

    // Use the persisted scroll position if we have one, otherwise compute fresh
    let indexOfFirstRow =
        if state.VisibleRowCount > 0 && viewportHeight = state.VisibleRowCount then
            // Viewport hasn't changed size, keep the same scroll position
            state.IndexOfFirstVisibleRow
        else
            // Viewport changed or first render: ensure cursor is visible
            min cursor (max 0 (totalItems - viewportHeight))

    let visibleRowCount = min viewportHeight (totalItems - indexOfFirstRow)

    // Post the layout event so we persist this for next time
    vdomContext.PostLayoutEvent (MyPostLayoutEvent.MultiSelectSize (checkboxListKey, indexOfFirstRow, visibleRowCount))

    // ... rest of render as before ...
```

## How stabilisation works

When you post a layout event that changes user state, the framework doesn't immediately paint to the screen.
Instead, it re-renders the VDOM with the new state, which may post more events, which may change state again.

This *stabilisation loop* continues until either:
1. No new post-layout events are posted, or
2. The state doesn't change (so re-rendering would be pointless), or
3. A maximum iteration count is reached (to prevent infinite loops)

Crucially, intermediate frames are never painted to the terminal.
Only the final, stabilised frame is flushed to the screen.
This means users never see flickering while the framework loops through user code working out the correct scroll position.

## Summary

Post-layout events solve the problem of components that need layout information to determine what to display:

1. **Post events during render**: When your component learns its bounds, call `ctx.PostLayoutEvent(...)` to communicate this back to user state.

2. **Handle events in `ProcessPostLayoutEvents`**: Update your state with the viewport information so it persists across renders.

3. **Use persisted state on next render**: Your component can use the stored information to maintain stable display when the UI is hidden and re-shown.

4. **The framework handles stabilisation**: You don't need to worry about intermediate states or flickering. Only the final frame is painted.
