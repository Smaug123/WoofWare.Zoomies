---
title: Automatic Focus Tracking
category: How-To Guides
categoryindex: 2
index: 1
---

# How to: use automatic focus tracking

WoofWare.Zoomies follows the design principle that you should be able to drop down to the lowest levels yourself, if you want to take complete control of the UI.
But in most cases, you don't want to handle things like "elements can have focus" yourself (e.g. handling the tab keystrokes).
WoofWare.Zoomies can handle this for you.

## Opting in

When building your `AppConfig`, set the `FocusHandling` field:

```fsharp
let config =
    AppConfig.make initialState transition view
    |> AppConfig.withFocusHandling FocusHandling.FrameworkManaged
```

`FocusHandling.FrameworkManaged` tells the framework to intercept Tab/Shift+Tab keystrokes and cycle focus among `withFocusTracking` nodes.
`FocusHandling.UserManaged` passes all keystrokes through to your `HandleInput`/`ActivationResolver` without any framework focus handling.
