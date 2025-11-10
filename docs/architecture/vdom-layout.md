# Measure/Arrange layout system

*The design is originally Sonnet 4.5's with input from Gemini 2.5 Pro. The original design doc it produced is [in the claude-history folder](../claude-history/vdom-layout.md). This current document is free-range and organic.*

## What components are subject to layout constraints

The fundamental primitive is the panel-split.
All other components are ultimately laid out within tiled panel-splits.
So the *only* component which contributes constraints to layout is the panel-split (though it decides how much space it wants based on the other components it contains, including non-panel-splits like text boxes).

There are currently three possible ways you can specify a split: by an absolute number of terminal character cells, by a proportion of the rendering area, or by "auto" (content-based layout).

## What happens when layout takes place

1. WoofWare.Zoomies decides to rerender, perhaps because user state has changed or the terminal has been resized.
1. Zoomies invokes the application author's code, asking it for a Vdom; the author gets to know the terminal size, but not much else.
1. The author's code tells Zoomies it wants this specific Vdom. In that Vdom, components declare their size preferences declaratively: they can choose a minimum, preferred, and maximum size.
1. Zoomies traverses the Vdom measuring each component, to collect layout constraints for each.
1. Zoomies solves the constraints and lays out the component in space.
1. Zoomies renders the Vdom.

The constraint solving algorithm is intended to be simple and predictable, being purely top-down (since constraints were collected bottom-up during the measurement phase).
1. Allocate the minimum amount of space to satisfy minimum requirements.
1. Distribute excess space proportionally among all children.

## What happens when constraints couldn't be satisfied

The constraint solving algorithm is purely top-down.
A parent node gets given some space, and then must work out for itself how to partition that space among any of its children; the children have no choice about what space they render into.

That means you may find that you requested a minimum height of `3` cells, but actually you've only been given `1` (if the constraints were impossible to solve so the framework just did the best it could)!
For the same reason, you may also find that your aspect ratio is nothing like what you requested.

In all cases, the render phase (which happens after layout, which itself happens after measurement) will tell the component what bounds it has to render into, and it's up to the component to do something sensible - maybe it decides to clip text, for example.

## Conflict resolution

Conflicts can only occur with "auto" or "absolute" panel splits, not "proportion" splits alone.

Satisfying proportion requests takes precedence over minimum size requirements. The motivating example is a log viewer, docked to the side of the UI, which shouldn't expand to fill the UI just because its content grew.

## Width and height

When declaring your size preferences, the fundamental unit of measurement is width; you then specify min/preferred/max height as a function of the width.
(This lets you announce how much space it would take to re-flow text within a text box, for example, as the width changes.)
