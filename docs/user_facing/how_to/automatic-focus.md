# How to: use automatic focus tracking

WoofWare.Zoomies follows the design principle that you should be able to drop down to the lowest levels yourself, if you want to take complete control of the UI.
But in most cases, you don't want to handle things like "elements can have focus" yourself (e.g. handling the tab keystrokes).
WoofWare.Zoomies can handle this for you.

## Opting in

When instantiating the UI, an argument allows you to select whether to handle focus.

```fsharp
App.run
    initialState
    (fun userState ->
        // here!
        true
    )
    processWorld
    computeVdom
```

Your `'userState -> bool` function gets invoked on every pass round the render loop, in response to every batch of external changes.
So, if you really want to, you can choose to turn automatic focus tracking on or off dynamically.
