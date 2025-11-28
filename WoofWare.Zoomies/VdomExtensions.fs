namespace WoofWare.Zoomies

/// Additional Vdom members that depend on Layout
[<AutoOpen>]
module VdomExtensions =
    type Vdom with

        /// <summary>Measures a VDOM node to determine its size requirements given constraints.</summary>
        /// <remarks>
        /// This helper exposes the framework's measurement logic to components that need to inspect
        /// child measurements (e.g., to compute aggregate constraints or build custom layouts).
        /// It is primarily intended for use within FlexibleContent measure callbacks.
        /// </remarks>
        /// <param name="vdom">The VDOM node to measure.</param>
        /// <param name="constraints">The constraints within which to measure the node.</param>
        /// <returns>The measured size information for the node.</returns>
        static member measure (vdom : Vdom<DesiredBounds, 'keyed>) (constraints : MeasureConstraints) : MeasuredSize =
            let keylessVdom =
                match vdom with
                | Vdom.Keyed (inner, _) -> KeylessVdom.Keyed inner
                | Vdom.Unkeyed (inner, _) -> KeylessVdom.Unkeyed inner

            let measured = Layout.measureEither constraints keylessVdom
            measured.Measured
