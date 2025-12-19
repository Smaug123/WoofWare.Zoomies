---
title: Cutoff
category: Explanation
categoryindex: 3
index: 1
---

# Cutoff

WoofWare.Zoomies *aggressively* cuts off execution early to prevent rerendering.
Your Vdom construction function will not even be called if circumstances haven't changed according to the WoofWare.Zoomies execution engine: that is, if your own user state hasn't changed, automatic state tracking hasn't changed, and if relevant facts about the terminal haven't changed.

We don't attempt to repaint components which haven't changed since the last render.
For example, if you have two text boxes side by side, and you change the contents of one of them (and no other changes take place, e.g. layout changes), the unchanged one won't be repainted.

And even if a component *has* changed since the last render - even if the entire Vdom has changed - in almost all cases we only repaint the cells on the screen that have changed since the last paint.
(The exception is terminal resize events, where we unconditionally rerender everything, for simplicity.)

## What it means for "user state to change"

We use the `.Equals` method on your user state type to determine whether user state has changed since the last render.
(Recall that user state is expected to be immutable. You "change user state" by returning a new user state from `WorldProcessor.ProcessWorld` or other methods on `WorldProcessor`, whereupon that new state takes the place of the old one within the WoofWare.Zoomies internals.)

One upshot of this is that, for example, if you simply discard all the keystrokes in an incoming batch during `WorldProcessor.ProcessWorld` (perhaps because you only recognise the "space" key but the user mashed the keyboard without hitting "space"), and therefore you don't change your user state, you will not be asked to recompute the Vdom this time round the render loop.

## What it means for "automatic state tracking to have changed"

If you've opted into WoofWare.Zoomies's [automatic focus management](../how_to/automatic-focus.md), for example, then the user pressing the Tab key may cause a different element to become focused.
If that happens, you'll be asked to recompute the Vdom.

## What it means for "the terminal to have changed"

If the terminal has changed size, you'll be asked to recompute the Vdom, and a full rerender will occur, including (unusually) a complete repaint of the screen.

## Forcing a rerender

If I've got the cutoff mechanism wrong, you can work around it by making some part of your user state dirty to force a rerender.
For example, perhaps you could have an int counter `Dirty : int` that you increment.

However, this should not be necessary.
All changes to the world that you'd want to cause a rerender should come in through the `WorldStateChange` system, and you should be putting your own events through there too (via the `IWorldBridge`).
And if you want to cause a rerender because you found out too late what you were displaying (e.g. because your VDOM is dynamic, with its contents depending on the exact layout area it's displaying in), you should again put your own events through `IVdomContext<_>.PostLayoutEvent`.

It's a bug in WoofWare.Zoomies if you find you are having to depend on mutable state through some channel that's not mediated by the framework.

### Example: reacting to external events

Say you're writing a polling filesystem watcher, that scans for files in the current working directory on a 1s timer so as to display them in a list.
In this case, you should set up the timer just before you return a `WorldProcessor`, and register that timer using `IWorldBridge.SubscribeEvent`.
The subscription pipes `ApplicationEvent`s into the WoofWare.Zoomies world, giving you the chance to adjust your user state in response when the WoofWare.Zoomies framework calls your `WorldProcessor.ProcessWorld`.

### Example: reacting to your own layout

You can implement a virtualised list (which has to know what is displayed and what is not) by posting events to yourself *during render* (after you know exactly what you're displaying), which you handle in `WorldProcessor.ProcessPostLayoutEvents`.
See the [post-layout events tutorial](../tutorial/post-layout-events.md) to understand the mechanics of this.
