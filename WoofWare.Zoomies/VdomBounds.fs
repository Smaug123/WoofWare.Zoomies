namespace WoofWare.Zoomies

/// Additional Vdom members that depend on Layout
[<RequireQualifiedAccess>]
module VdomBounds =
    /// <summary>Measures a VDOM node to determine its size requirements given constraints.</summary>
    /// <remarks>
    /// <para>
    /// It might help to think of this as "asking the VDOM node to express its preferences about the size it wants to
    /// take up on screen".
    /// </para>
    ///
    /// <para>
    /// This helper exposes the framework's measurement logic to components that need to inspect
    /// child measurements (e.g., to compute aggregate constraints or build custom layouts).
    /// It is primarily intended for use within <c>Vdom.flexibleContent</c> measure callbacks.
    /// </para>
    /// </remarks>
    /// <param name="vdom">The VDOM node to measure.</param>
    /// <param name="constraints">The constraints within which to measure the node.</param>
    /// <returns>The measured size information for the node.</returns>
    let measure (vdom : Vdom<DesiredBounds>) (constraints : MeasureConstraints) : MeasuredSize =
        let measured = Layout.measureEither constraints vdom
        measured.Measured
