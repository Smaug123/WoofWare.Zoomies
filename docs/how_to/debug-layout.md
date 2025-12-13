---
title: Debug Layout
category: How-To Guides
categoryindex: 2
index: 2
---

# How to: debug the layout algorithm

When starting the WoofWare.Zoomies framework, you can set the `WOOFWARE_ZOOMIES_DEBUG_TO_FILE` environment variable to `true` or `1` to have the framework log debug output to a file.
When enabled, the framework will write detailed layout information to a temporary file with a path like `/tmp/zoomies-layout-<guid>.txt`.
That file path will be printed to stderr when the application starts.
