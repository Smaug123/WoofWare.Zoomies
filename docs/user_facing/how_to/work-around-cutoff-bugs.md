# How to: work around any automatic cutoff bugs

WoofWare.Zoomies *aggressively* [cuts off execution early](../explanation/cutoff.md) to prevent rerendering.
It's possible that I've written bugs into this system, though.

Neither of the following mechanisms should be necessary: all changes to the world that you'd want to cause a rerender should come in through the `WorldStateChange` system, and you should be putting your own events through there too (via the `IWorldBridge`).
It's a bug in WoofWare.Zoomies if this doesn't happen (or your own code is not correctly funnelling all state changes through the `IWorldBridge`), but here is how you can work around bugs if I've got it wrong.

## Forcing a rerender by dirtying your state

You can make some part of your user state dirty to force a rerender.
For example, perhaps you could have an int counter `Dirty : int` that you increment.
As long as the dirty state participates in `yourUserState.Equals` such that `oldState <> newState` after the dirtying, we'll rerender.

## Forcing a rerender by only partially processing the event stream

WoofWare.Zoomies exposes `WorldStateChange`s to you, for processing in your custom `WorldProcessor`.
You can force a rerender by returning a `ProcessWorldResult` that has `ProcessWorldResult.withRerender` set.
This indicates to the framework that you have only partially processed the `WorldStateChange` stream, and will cause a rerender before you are immediately handed the rest of the stream again (that you told us you didn't process).
(You won't receive any events that you've already processed: the input `Span` starts from the first index you told us was unprocessed.)

# Example: correctly reacting to external events

Say you're writing a polling filesystem watcher, that scans for files in the current working directory on a 1s timer so as to display them in a list.
In this case, you should set up the timer just before you return a `WorldProcessor`, and register that timer using `IWorldBridge.SubscribeEvent`.
The subscription pipes `ApplicationEvent`s into the WoofWare.Zoomies world, giving you the chance to adjust your user state in response when the WoofWare.Zoomies framework calls your `WorldProcessor.ProcessWorld`.

