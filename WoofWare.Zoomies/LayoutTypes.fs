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
/// The "measure" phase of layout comes first. In the "measure" phase, the framework traverses the VDOM discovering
/// what size constraints each node expresses. (Then the subsequent "arrange" phase resolves those size
/// constraints into physical rectangles, and the "render" phase then draws the components into those rectangles.)
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
/// The "measure" phase of layout comes first. In the "measure" phase, the framework traverses the VDOM discovering
/// what size constraints each node expresses. (Then the subsequent "arrange" phase resolves those size
/// constraints into physical rectangles, and the "render" phase then draws the components into those rectangles.)
///
/// Note that the subsequent "arrange" phase may decide to allocate a component much less space, or more space, than
/// it requested during the "measure" phase with its <c>MeasuredSize</c> response!
/// </remarks>
type MeasuredSize =
    {
        /// Minimum width needed to render without data loss.
        ///
        /// The arrange pass may violate this preference if there's too little space available.
        ///
        /// You are responsible for ensuring that this is (inclusively) between 0 and PreferredWidth. Although the
        /// measure phase currently takes minima as necessary to ensure the ordering invariant, we strongly recommend
        /// you maintain the invariants yourself, in case we change how conflicting requirements are resolved in the
        /// future.
        MinWidth : int
        /// Preferred width if space is available.
        ///
        /// You are responsible for ensuring that this is at most MaxWidth, if you give a MaxWidth, although
        /// the measure phase currently takes minima as necessary to ensure the ordering invariant.
        PreferredWidth : int
        /// Maximum useful width (None = unbounded). The arrange pass may violate this preference if there's
        /// too much space available.
        MaxWidth : int option
        /// Minimum height needed given some width.
        ///
        /// The arrange pass may violate this preference if there's too little space available.
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
        ///
        /// The arrange pass may violate this preference if there's too much space available.
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
                // ... so minHeight >= 0

                let preferredHeight = this.PreferredHeightForWidth width

                if preferredHeight < minHeight then
                    failwith
                        $"PreferredHeightForWidth was smaller than min height for width %i{width}: %i{preferredHeight} vs %i{minHeight}"
                // ... so preferredHeight >= minHeight (>= 0)

                match this.MaxHeightForWidth width with
                | None -> ()
                | Some maxHeight ->
                    if maxHeight < preferredHeight then
                        failwith
                            $"MaxHeightForWidth was smaller than preferred height for width %i{width}: %i{maxHeight} vs %i{preferredHeight}"
                    // ... so maxHeight >= preferredHeight (>= minHeight >= 0)
                    ()
