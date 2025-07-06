# WoofWare.Zoomies

A port of Jane Street's [bonsai](https://github.com/janestreet/bonsai), a library for expressing reactive [incremental](https://en.wikipedia.org/wiki/Incremental_computing) computation.

# Status

Currently totally unusably incomplete; this is more of a placeholder than anything at the moment.
See [porting_progress.txt](./porting_progress.txt) for details.

# Division

`incremental` is `WoofWare.PlayFetch`.

`bonsai` is `WoofWare.Zoomies`.

# Porting notes

The commits taken were:

* bonsai at [90209249fe584da9f8a0334b3ae69fd0207ea978](https://github.com/janestreet/bonsai/commit/90209249fe584da9f8a0334b3ae69fd0207ea978)
* core_kernel at [774a6821b14cbcdcde02cbbca1984ea32bf06184](https://github.com/janestreet/core_kernel/blob/774a6821b14cbcdcde02cbbca1984ea32bf06184)
* incremental at [4c3946aafe786e4846f8ec3f4825e7bc689a70fa](https://github.com/janestreet/incremental/tree/4c3946aafe786e4846f8ec3f4825e7bc689a70fa)

# Licence

WoofWare.Zoomies is a derivative work of [bonsai](https://github.com/janestreet/bonsai/commit/90209249fe584da9f8a0334b3ae69fd0207ea978), [core_kernel](https://github.com/janestreet/core_kernel/blob/774a6821b14cbcdcde02cbbca1984ea32bf06184), and [incremental](https://github.com/janestreet/incremental/tree/4c3946aafe786e4846f8ec3f4825e7bc689a70fa), which are used under the MIT licence (all glory to Jane Street); see [LICENSE_janestreet.md](./LICENSE_janestreet.md).

WoofWare.Zoomies is licenced to you under the MIT licence; see [LICENSE.md](./LICENSE.md).
