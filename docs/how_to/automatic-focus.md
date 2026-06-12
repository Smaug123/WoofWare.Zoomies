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
    // FrameworkManaged is the default, so this line is optional:
    |> AppConfig.withFocusHandling FocusHandling.FrameworkManaged
```

`FocusHandling.FrameworkManaged` (the default) tells the framework to intercept Tab/Shift+Tab keystrokes and cycle focus among `withFocusTracking` nodes.
`FocusHandling.UserManaged` stops the framework from intercepting Tab/Shift+Tab for focus cycling; they are passed through as regular keystrokes to your `ActivationResolver` and `HandleInput`. Focus tracking still operates in this mode: focusable nodes are registered, `isInitiallyFocused` assigns focus on the first render, and the `ActivationResolver` still fires for the currently-focused element.
