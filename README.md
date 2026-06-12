# WoofWare.Zoomies

An [immediate-mode](https://en.wikipedia.org/wiki/Immediate_mode_(computer_graphics)) [terminal user interface](https://en.wikipedia.org/wiki/Text-based_user_interface) library using a [virtual DOM](https://en.wikipedia.org/wiki/Virtual_DOM) sort of approach.

It uses an incremental computation engine (inspired by Jane Street's [Incremental](https://github.com/janestreet/incremental)) to efficiently propagate state changes through the UI, with aggressive early cutoff to avoid unnecessary work.

# Status

The very lowest-level primitives are there.
You could use this to create a TUI, if you *really* had to.

# Docs

See the [docs](./docs) folder.

# Licence

WoofWare.Zoomies is licenced to you under the MIT licence; see [LICENSE.md](./LICENSE.md).
