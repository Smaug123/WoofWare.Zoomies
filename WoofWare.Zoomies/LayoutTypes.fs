namespace WoofWare.Zoomies

/// A rectangle representing a region in the terminal, used for layout and rendering
[<Struct>]
type Rectangle =
    {
        TopLeftX : int
        TopLeftY : int
        Width : int
        Height : int
    }

/// <summary>Constraints imposed by the parent VDOM element during the "measure" phase of layout.</summary>
/// <remarks>
/// The "measure" phase of layout comes before the "render" phase: in the "measure" phase, the framework decides where
/// on the screen each component is going to render, finding a single solution to the constraint satisfaction problem.
/// Then, during the "render" phase, each component decides how to render itself into the space that was granted to it.
///
/// The framework will give you a <c>MeasureConstraints</c>; you interact with the type through the construction
/// of a <c>Vdom.flexibleContent</c>, where you indicate the space constraints your component would prefer to be granted
/// given that its parent has imposed these constraints.
/// </remarks>
type MeasureConstraints =
    {
        /// Maximum available width.
        /// Invariant: n >= 0
        MaxWidth : int
        /// Maximum available height.
        /// Invariant: n >= 0
        MaxHeight : int
    }

/// <summary>Size requirements reported by a node during the "measure" phase of layout.</summary>
/// <remarks>
/// The "measure" phase of layout comes before the "render" phase: in the "measure" phase, the framework decides where
/// on the screen each component is going to render, finding a single solution to the constraint satisfaction problem.
/// Then, during the "render" phase, each component decides how to render itself into the space that was granted to it.
///
/// Invariants (must hold for all valid MeasuredSize values):
/// <list type="bullet">
/// <item><description><c>0 &lt;= MinWidth &lt;= PreferredWidth</c></description></item>
/// <item><description>If <c>MaxWidth = Some m</c>, then <c>MinWidth &lt;= PreferredWidth &lt;= m</c></description></item>
/// <item><description>If <c>MaxWidth = Some m</c>, then <c>MinWidth &lt;= m</c> (nodes must not report a MinWidth exceeding constraints)</description></item>
/// <item><description>For any width w >= 0, <c>MinHeightForWidth(w) >= 0</c></description></item>
/// <item><description>For any width w >= 0, <c>MinHeightForWidth(w) &lt;= PreferredHeightForWidth(w)</c></description></item>
/// </list>
/// </remarks>
type MeasuredSize =
    {
        /// Minimum width needed to render without data loss.
        /// Must respect any MaxWidth constraint from measurement.
        /// May be violated by the arrange pass if insufficient space is available.
        MinWidth : int
        /// Preferred width if space is available.
        PreferredWidth : int
        /// Maximum useful width (None = unbounded). Arrangement may allocate beyond this.
        MaxWidth : int option
        /// Minimum height needed given some width
        MinHeightForWidth : int -> int
        /// Preferred height given some width
        PreferredHeightForWidth : int -> int
        /// Maximum useful height given some width (None = unbounded)
        MaxHeightForWidth : int -> int option
    }
