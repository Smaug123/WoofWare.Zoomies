# Post-layout events

WoofWare.Zoomies tries to ensure data is owned by one specific component, and carefully manages the data flow between the phases of rendering.
But some components want broader access to multiple pieces of state at once.

For these scenarios, Zoomies provides a channel for the render process itself to trigger updates to user state, effectively jumping back to the beginning of the render loop *before* seeing any new user inputs.

## Concrete problem: virtualised list

Consider a list which contains 10 elements, displaying on a console that is only 5 cells high.

Stepping through [the usual render loop](./vdom-layout.md), Zoomies assigns to the list an area of height 5 during the "arrange" phase, and proceeds to the "render" phase.

At this point, the VDOM discovers that it's only got space for five elements; consulting its user state, it renders (say) elements 3 through 7 to the screen (perhaps because there's a user cursor on element 7, and the VDOM wants to keep that on the screen).

But now it needs to tell the user state that it's rendered those elements.
Otherwise, say the user presses the "down" arrow so that the list is forced to scroll: all the state knows is that element 7 is highlighted so element 8 needs to be highlighted.
It *doesn't* know that element 7 was at the bottom of the screen in the displayed list, and that it therefore needs to scroll the list to display elements 4 through 8!
The state needs to know the component's bounds so that it knows what's on screen and can decide which boxes to display in the list.

It would be possible in principle for the user to keep track of this bounds information themselves, but in that case they are effectively implementing a layout algorithm in their own code just so that they know what's on screen, which is extremely sad given that they knew what was on screen the tick before.

Rather than asking the user to deduce what happened on the previous tick during the "render" phase, we instead give them a channel through which they can *know* what happened.

## Solution

During the "render" phase, the user can call `IVdomContext<'event>.PostLayoutEvent`, which adds a (user-defined) `'event` to the head of a separate queue, morally like the `IWorldBridge` queue but dedicated solely to post-layout events.
This event might, for example, say "I rendered the `my-fancy-table` selection list into a viewport with height 5".
You can now store this information in your user state so that you can react to the "down arrow" keystroke on the next tick by defining a VDOM which already has the right entries visible (assuming the render area stays the same on this tick), rather than hoping the VDOM can deduce what entries are to be visible.

## Concrete mechanism

If there are any events in the post-layout queue after the render phase has finished, the framework calls your `AppConfig.HandlePostLayout` function for each event so that you get a chance to update user state.
Then, if user state has changed, we rerender; we repeat this process, handling post-layout events and rerendering, until the rerender stops producing post-layout events (or we hit the iteration limit).

Post-layout events are processed one at a time via the `HandlePostLayout : 'postLayoutEvent -> 'state -> 'state` function. The entire batch of events from a single render is drained before checking whether the state changed, to avoid per-event re-stabilization.
