---
title: Home
index: 0
---

# WoofWare.Zoomies

A reactive immediate-mode TUI framework for F#.

WoofWare.Zoomies uses a virtual DOM approach to terminal user interfaces: you describe what the UI should look like, and the framework handles rendering efficiently. The framework aggressively cuts off unnecessary work, only rerendering when state actually changes.

## Getting Started

The lowest-level primitives are available. You can build a TUI using:

- **Virtual DOM nodes**: text content, checkboxes, bordered panels, split layouts
- **Direction-based panel splitting**: vertical or horizontal, with proportional, absolute, or content-based sizing
- **Focus management**: opt-in automatic focus tracking, or handle it yourself
- **Event processing**: keystroke handling via the `WorldStateChange` system

## Documentation

{{fsdocs-list-of-documents}}

See the [API reference documentation](reference/index.html) for full API details.

## Philosophy

WoofWare.Zoomies follows the principle that from a small set of powerful and coherent primitives, it should be possible to build higher-level ergonomic libraries. When using those libraries, the coherence of the underlying primitives permits seamless drop-down to lower levels for customisation.

The framework:

- Renders the world afresh each cycle (immediate-mode)
- Stores state internally for efficiency, but the programmer simply provides a virtual DOM on request
- Never throws exceptions on bad user input; invalid inputs are handled gracefully
- Aims to avoid doing work unless necessary

Ultimately, the framework will incorporate something like Jane Street's Bonsai for efficient incremental updates.

## Licence

MIT. See [LICENSE.md](https://github.com/Smaug123/WoofWare.Zoomies3/blob/main/LICENSE.md).
