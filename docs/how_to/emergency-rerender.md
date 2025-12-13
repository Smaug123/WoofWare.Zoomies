---
title: Emergency Rerender
category: How-To Guides
categoryindex: 2
index: 4
---

# How to deal with an emergency "component could not render into available space" situation

The general philosophy of the WoofWare.Zoomies framework is that components should gracefully do what they can with the space that's assigned to them:
the user is free to make their terminal arbitrarily small, and you just have to deal with that.
But I'm not going to force my world view on you: perhaps you have a component that it really makes no sense to render into a sufficiently small space.
(Perhaps you've written a Space Invaders game or something and you haven't been given room to display any of the game.)
You've diligently advertised its minimum size in a `Vdom.flexibleContent`, but the framework hands you back a space that's too small and you would prefer to bail out rather than try and squeeze yourself down further.

You can already solve this problem using `Vdom.flexibleContent` at the top level, if you can predict in advance from the root of the Vdom that some component isn't going to fit; but that involves effectively simulating a layout algorithm yourself inside your VDOM construction code, so we really don't recommend it except in the very simplest cases.
Instead, you can use [post-layout events](../tutorial/post-layout-events.md) from within *any* component, not just the top-level one: during the render, you can signal the emergency to your user state, causing a rerender at the top level (during the stabilisation cycle before you receive any more user input) and potentially completely altering the entire UI before the aborted UI is ever painted.

At some point, I hope to provide some sugar for this in the framework, but for now you're on your own implementing this.
Here's how you can do it.

## Problem statement

A deeply nested inner component simply can't meaningfully render into the space that was assigned to it.
That component wishes to bail out and cause the *entire* UI to rerender in a different way.

## Defining user state

Your user state is the way you'll know you need to render the emergency display during stabilisation after you discover you can't render what you wanted.

```fsharp
type State =
    {
        // You should have one of these for every point in the VDOM which
        // could signal an emergency. This example has only one such point.
        // (This way, you can clear the correct boolean when the terminal
        // becomes wide enough, so you stop rendering the emergency display!)
        Emergency : bool
    }
```

## Consuming the event that will update user state

The render loop is going to inform the `State` through a post-layout event:

```fsharp
type PostLayoutEvent =
    // You should have one of these cases for every point in the VDOM
    // which could signal an emergency. This example has only one such
    // point.
    | Emergency of bool

let processWorld (worldBridge : IWorldBridge<_>) =
    { new WorldProcessor<_, PostLayoutEvent, State> with
        member _.ProcessPostLayoutEvents (events, _ctx, state) =
            let mutable component1IsEmergency = state.Emergency
            for evt in events do
                match evt with
                | PostLayoutEvent.Emergency e -> component1IsEmergency <- e
            { state with Emergency = component1IsEmergency }
        member _.ProcessWorld (_, _, _) = failwith "whatever you had before"
    }
```

An important point about performance: it's fine to process an `Emergency` event in `ProcessPostLayoutEvents` on every tick.
Zoomies's [early cutoff mechanism](../explanation/cutoff.md) means the repeated setting of `Emergency = false` (resp. `true`) in the happy path (resp. sad path) *won't* cause rerenders: since user state remains equal after the `ProcessPostLayoutEvents` call, Zoomies doesn't rerender.
It's only when the value of `Emergency` *changes* that the UI rerenders.

## Emitting the event that updates the state

```fsharp
let tooSmallWrapper (ctx : IVdomContext<PostLayoutEvent>) (state : State) : Vdom<DesiredBounds> =
    let measure (constraints : MeasureConstraints) =
        {
            // To keep things simple, I'm just asking for the max dimensions.
            // In real life, your component might have a better idea how
            // much space it needs.
            // Specifying MaxWidth here can remove space from nearby components
            // which would like to expand into this component's space!
            // I recommend trying different things out empirically to see what
            // works best for you.
            MinWidth = constraints.MaxWidth
            PreferredWidth = constraints.MaxWidth
            MaxWidth = None
            MinHeightForWidth = fun _ -> constraints.MaxHeight
            PreferredHeightForWidth = fun _ -> constraints.MaxHeight
            MaxHeightForWidth = fun _ -> None
        }

    let render (bounds : Rectangle) =
        if bounds.Width < 10 || bounds.Height < 10 then
            ctx.PostLayoutEvent (PostLayoutEvent.Emergency true)
            // This Vdom is not going to paint to the screen, so it's not
            // very important what goes here.
            // Emitting the event on the line above, and altering our state in
            // the WorldProcessor in response, causes Zoomies to rerender before
            // the current render even paints;
            // and we will be setting up the top-level VDOM to render something
            // different on seeing that new state.
            Vdom.textContent "too small"
        else
            // We can safely render. A previous tick might have displayed
            // the emergency UI, so we need to be sure we're no longer in
            // emergency mode.
            ctx.PostLayoutEvent (PostLayoutEvent.Emergency false)
            whateverVdomYouWantedBefore ctx state

    Vdom.flexibleContent measure render
```

This wrapper *without* the `PostLayoutEvent` works fine at the top level, choosing to render a completely different interface if the terminal is too small, but the value added by the post-layout event here is that even a deeply nested inner component can trigger a rerender: it's "global to the entire VDOM" rather than having to be decided at the top level.

## Consuming the emergency user state

Now your top-level VDOM can look like this:

```fsharp
let vdom (ctx : IVdomContext<PostLayoutEvent>) (state : State) : Vdom<DesiredBounds> =
    if state.Emergency then
        Vdom.textContent "embiggen terminal"
    else
        topLevelVdomYouWantedBefore ctx state
```
