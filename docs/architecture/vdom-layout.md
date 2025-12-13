---
title: Measure/Arrange Layout
category: Architecture
categoryindex: 4
index: 1
---

# Measure/Arrange layout system

*The design is originally Sonnet 4.5's with input from Gemini 2.5 Pro. This current document is free-range and organic.*

## What components are subject to layout constraints

The fundamental primitive is the panel-split.
All other components are ultimately laid out within tiled panel-splits.
So the *only* component which contributes constraints to layout is the panel-split (though it decides how much space it wants based on the other components it contains, including non-panel-splits like text boxes).

There are currently three possible ways you can specify a split: by an absolute number of terminal character cells, by a proportion of the rendering area, or by "auto" (content-based layout).

## What happens when layout takes place

1. WoofWare.Zoomies decides to rerender, perhaps because user state has changed or the terminal has been resized.
1. Zoomies invokes the application author's code, asking it for a Vdom; the author gets to know the terminal size at this point, but not much else.
1. The author's code tells Zoomies it wants this specific Vdom. In that Vdom, components declare their size preferences declaratively: they can choose a minimum, preferred, and maximum size, and they can declare that if they *were* given some amount of width, then they would require some amount of height.
1. Zoomies traverses the Vdom measuring each component, to collect layout constraints for each: the "measure" phase.
1. Zoomies solves the constraints and lays out the component in space: the "arrange" phase.
1. Zoomies renders the Vdom to those rectangles: the "render" phase. (There is a mechanism for the render phase to cause further user state updates and perhaps trigger rerenders; see [Post-Layout Events](./post-layout-events.md).)

The constraint solving algorithm is intended to be simple and predictable, being purely top-down (since constraints were collected bottom-up during the measurement phase).
It accommodates three types of behaviour for a component: "proportion", "absolute", and "auto".

* "Proportion" gives some proportion of the available space to a child. This ignores any constraints advertised by child nodes (the motivating example is a log viewer docked to the side of a UI taking up 20% of the width, which we don't want to expand just because the lines it contains are very long).
* "Absolute" gives a fixed number of cells to a child. This ignores any constraints advertised by child nodes.
* "Auto" is content-driven layout that considers the constraints advertised by child nodes. When available space is too limited to satisfy all minima simultaneously, it violates minima proportionally among all children. When available space is between minima and preferences, it satisfies minima first, then distributes the excess according to the ratio of child components' preferences. When available space exceeds preferences, it distributes the excess proportionally among all components.

## What happens when constraints couldn't be satisfied

The constraint solving algorithm is purely top-down.
A parent node gets given some space, and then must work out for itself how to partition that space among any of its children; the children have no choice about what space they render into.

That means you may find during the "render" phase that your component, which requested a minimum height of `3` cells during the "measure" phase, actually received `1` cell during the "arrange" phase (if the constraints were impossible to solve so the framework just did the best it could)!
For the same reason, you may also find that your aspect ratio is nothing like what you requested.

In all cases, Zoomies tells the component during the render phase what bounds it has to render into, and it's up to the component to do something sensible - maybe it decides to clip text, for example.

## Width and height

When declaring your size preferences, the fundamental unit of measurement is width in "cells" (roughly one character of output, if we ignore extra-wide graphemes like the Basmala "﷽", which as far as I can tell renders horribly in most terminals; general strategy for wide grapheme handling is not yet defined); you then specify min/preferred/max height as a function of the width.
This lets you announce how much space it would take to re-flow text within a text box, for example, as the width changes.

## Flexible components

Some components, like the progress bar, don't mind how big they are.
Such a component only knows what to render once it knows what size it's going to be.

For this use-case, there is `Vdom.flexibleContent`, which contains a `render` function which declares during the "render" phase that an entire declarative VDOM is to appear in the space that the "arrange" phase has allocated to the `flexibleContent` node.
`Vdom.flexibleContent` also contains a `measure` function which gets invoked during the "measure" phase of the render loop, to express a preference about its size, but once `measure` has completed and the "arrange" phase has decided where this node is going to be rendered to, there's no changing those bounds.

Effectively, an entire new render loop gets started within the `flexibleContent`'s allocated space with the inner VDOM, to determine what will appear there; and we keep going until we've hit the bottom and there are no more `flexibleContent`s nested.
