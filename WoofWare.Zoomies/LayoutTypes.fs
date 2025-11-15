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

/// Constraints provided by parent during measurement
type MeasureConstraints =
    {
        /// Maximum available width.
        /// Invariant: n >= 0
        MaxWidth : int
        /// Maximum available height.
        /// Invariant: n >= 0
        MaxHeight : int
    }

/// Size requirements reported by a node
/// Invariants (must hold for all valid MeasuredSize values):
/// - 0 <= MinWidth <= PreferredWidth
/// - If MaxWidth = Some m, then MinWidth <= PreferredWidth <= m
/// - If MaxWidth = Some m, then MinWidth <= m (nodes must not report a MinWidth exceeding constraints)
/// - For any width w >= 0, MinHeightForWidth(w) >= 0
/// - For any width w >= 0, MinHeightForWidth(w) <= PreferredHeightForWidth(w)
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
