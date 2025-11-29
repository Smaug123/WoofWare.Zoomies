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
/// </remarks>
type MeasuredSize =
    {
        /// Minimum width needed to render without data loss.
        /// Must respect any MaxWidth constraint from measurement.
        /// May be violated by the arrange pass if insufficient space is available.
        ///
        /// You are responsible for ensuring that this is (inclusively) between 0 and PreferredWidth.
        MinWidth : int
        /// Preferred width if space is available.
        ///
        /// You are responsible for ensuring that this is at most MaxWidth, if you give a MaxWidth.
        PreferredWidth : int
        /// Maximum useful width (None = unbounded). Arrangement may allocate beyond this.
        MaxWidth : int option
        /// Minimum height needed given some width.
        ///
        /// You are responsible for ensuring that the output height is nonnegative for any of the (nonnegative) width
        /// inputs we give you.
        MinHeightForWidth : int -> int
        /// Preferred height given some width.
        ///
        /// You are responsible for ensuring that the output height is at least the MinHeightForWidth, for any of the
        /// (nonnegative) width inputs we give you.
        PreferredHeightForWidth : int -> int

        /// Maximum useful height given some width (None = unbounded)
        MaxHeightForWidth : int -> int option
    }

    /// You can call this to check whether you're upholding the invariants.
    /// Probably don't do that in Release mode!
    member this.Invariant (width : int option) =
        if this.MinWidth < 0 then
            failwith $"MinWidth was negative: %i{this.MinWidth}"

        if this.MinWidth > this.PreferredWidth then
            failwith $"MinWidth out of bounds: %i{this.MinWidth} vs %i{this.PreferredWidth}"

        match this.MaxWidth with
        | None -> ()
        | Some m ->
            if this.PreferredWidth > m then
                failwith $"MaxWidth out of bounds: preferred width %i{this.PreferredWidth} was bigger than %i{m}"

        match width with
        | None -> ()
        | Some width ->
            if width < 0 then
                // vacuously passes I guess - could consider throwing here because the user is holding us wrong
                ()
            else
                let minHeight = this.MinHeightForWidth width

                if minHeight < 0 then
                    failwith $"MinHeightForWidth for width %i{width} was negative: %i{minHeight}"

                let preferredHeight = this.PreferredHeightForWidth width

                if preferredHeight < minHeight then
                    failwith
                        $"PreferredHeightForWidth was smaller than min height for width %i{width}: %i{preferredHeight} vs %i{minHeight}"
