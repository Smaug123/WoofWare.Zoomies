---
title: Debug Layout
category: How-To Guides
categoryindex: 2
index: 2
---

# How to: debug the layout algorithm

`App.run` accepts an optional `debugWriter : StreamWriter option` parameter.
When `Some writer` is provided, the framework writes detailed layout information to that writer every frame.

The sample application (`WoofWare.Zoomies.App`) reads the `WOOFWARE_ZOOMIES_DEBUG_TO_FILE` environment variable (set to `true` or `1`) and, if set, creates a `StreamWriter` to a temporary file with a path like `/tmp/zoomies-layout-<guid>.txt`.
That file path will be printed to stderr when the application starts.
This environment variable handling is specific to the sample app; your own application should construct and pass the `StreamWriter` directly to `App.run`.
