---
title: The Render Loop
category: Explanation
categoryindex: 3
index: 2
---

# The render loop

The event loop in WoofWare.Zoomies happens in distinct steps.
We start at the point where something has caused the framework to decide to rerender: perhaps user state has changed in response to an input from the world, or perhaps the terminal has resized, for example.

1. Zoomies invokes the application author's code, asking it for a Vdom. (The function invoked is the Vdom-producing argument to `App.run`.) The author gets an `IVdomContext<_>`, so they know the terminal size on the previous render, and the key (if any) of the component which the [automatic focus-tracking mechanism](../how_to/automatic-focus.md) had focused on the last tick.
1. The author's code returns a Vdom.
1. Zoomies traverses the Vdom measuring each component, to collect layout constraints for each: the "measure" phase.
  * The built-in primitives of WoofWare.Zoomies all implicitly declare their size preferences, but there is one which cannot: `Vdom.flexibleContent`.
  * This explicitly contains a user-provided function which specifies the measurements to be used in this phase; note that measurement does not descend into the sub-Vdom specified by a `flexibleContent` (in fact we haven't even executed the user-provided function which gives us that sub-Vdom yet).
1. Zoomies solves the constraints and lays out the component in space: the "arrange" phase.
1. Zoomies renders the Vdom to those rectangles: the "render" phase.
  * The `Vdom.flexibleContent` component contains a user-provided function which we call, now that we know the exact space we're going to render the Vdom into.
  * The `flexibleContent` now yields a sub-`Vdom` at this point; we loop back to the "measure" phase for that sub-`Vdom` within the measured bounds of the `flexibleContent` node.
  * We keep running this measure/arrange/render loop, on hopefully-successively-decreasing sizes of measured space, until there are no more `flexibleContent`s to resolve.
1. Optionally: there is a mechanism for the render phase to cause further user state updates and perhaps trigger rerenders; see [Post-Layout Events](../user_facing/tutorial/post-layout-events.md). If a post-layout event causes a user state change, we loop all the way back to the top and rerender.
1. Finally, we repaint the screen.

Notably, the constraint solving mechanism is only two-pass: we ask each component how big it wants to be, then we do a single pass to solve the constraints (imperfectly but quickly and predictably), and now we know the layout exactly.
Dynamic `Vdom.flexibleContent` nodes don't fundamentally change this property: they're opaque, merely causing additional disjoint runs of the layout algorithm within the space that a parent run of the algorithm carved out for them.

Note also that a component has to do the best it can with what it's got, when it comes to rendering!
The framework provides no guarantees in general that the space a component was assigned is even slightly related to the space it requested.
You might have to render the complete works of Shakespeare in a 1x1 box; you have very little recourse for signalling an emergency once measurement and arrangement have taken place and your component is now trying to render.
(There *is* a recourse using post-layout events, though the framework doesn't currently give you any sugar for this; see the [emergency rerender](../how_to/emergency-rerender.md) guide.)
