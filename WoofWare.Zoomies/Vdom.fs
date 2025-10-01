namespace WoofWare.Zoomies

/// Specify the direction to split, when splitting a panel.
type SplitDirection =
    /// Split so that the divider runs vertically: one component is to the left and one is to the right.
    | Vertical
    /// Split so that the divider runs horizontally: one component is on top and one is on the bottom.
    | Horizontal

/// Specifies how a component should be sized along one dimension (width or height).
type SizeConstraint =
    /// Component must be exactly this many cells in size.
    | Fixed of int
    /// Component must be at least this many cells in size, but can grow larger if space is available.
    | AtLeast of int
    /// Component can be up to this many cells in size, but no larger.
    | AtMost of int
    /// Component must be between the minimum and maximum number of cells in size (inclusive).
    | Between of min : int * max : int
    /// Take as much space as possible
    | Greedy
    /// Size to fit content
    | Content
    /// Try to satisfy the first constraint. If rendering fails with that constraint, the fallback is acceptable.
    | Preferred of preferred : SizeConstraint * fallback : SizeConstraint

/// Specifies the desired dimensions and layout priority for a component.
/// The layout engine will attempt to satisfy these constraints, allocating space based on priority when there are conflicts.
type DesiredBounds =
    {
        /// Desired width constraint for this component.
        Width : SizeConstraint
        /// Desired height constraint for this component.
        Height : SizeConstraint
        /// More positive numbers are higher priority.
        Priority : int
    }

    /// Default constraints: size to content, at lowest priority.
    static member Default =
        {
            Width = SizeConstraint.Content
            Height = SizeConstraint.Content
            Priority = 0
        }

/// Determines how space is divided when a panel is split into two components.
[<RequireQualifiedAccess>]
type SplitBehaviour =
    /// Split using a proportion: the first component (that is, the top or left component) gets this fraction of the
    /// available space (must be between 0 and 1, exclusive).
    /// For example, Proportion 0.3 on a vertical split gives the left component 30% of the space and the right component 70%.
    | Proportion of float
    /// Split using an absolute cell count: the first component gets exactly this many cells, and the second gets the remainder.
    | Absolute of int

type VdomContent<'bounds> =
    internal
    | Bordered of Vdom<'bounds>
    | PanelSplit of SplitDirection * SplitBehaviour * child1 : Vdom<'bounds> * child2 : Vdom<'bounds>
    | TextContent of string * focused : bool * onReceiveFocus : (unit -> unit) option
    | Checkbox of isChecked : bool * isFocused : bool * onReceiveFocus : (unit -> unit)

and Vdom<'bounds> =
    internal
        {
            Constraints : DesiredBounds
            Content : VdomContent<'bounds>
        }

/// Interface for implementing a catamorphism (fold) over the Vdom tree structure.
/// Each method corresponds to one type of content node, and receives the computed results from child nodes.
/// Use this to transform or analyze a Vdom tree in a recursive, bottom-up manner.
type VdomCata<'ret> =
    /// Called when visiting a bordered component.
    abstract AtBordered : DesiredBounds -> 'ret -> 'ret
    /// Called when visiting a split panel.
    abstract AtPanelSplit : DesiredBounds -> SplitDirection -> SplitBehaviour -> child1 : 'ret -> child2 : 'ret -> 'ret
    /// Called when visiting text content.
    abstract AtTextContent : DesiredBounds -> string -> focused : bool -> onReceiveFocus : (unit -> unit) option -> 'ret

    /// Called when visiting a checkbox.
    abstract AtCheckbox :
        DesiredBounds -> isChecked : bool -> isFocused : bool -> onReceiveFocus : (unit -> unit) -> 'ret

[<RequireQualifiedAccess>]
module Vdom =

    /// Wraps VdomContent with the specified constraints to create a Vdom node.
    /// This is the fundamental constructor for building Vdom trees.
    let ofContent<'bounds> (constraints : DesiredBounds) (content : VdomContent<'bounds>) : Vdom<'bounds> =
        {
            Constraints = constraints
            Content = content
        }

    /// Creates a new Vdom node which has the same content as the input, but different layout constraints.
    /// Use this to adjust the sizing of a component, for example.
    let withConstraints<'bounds> (constraints : DesiredBounds) (vdom : Vdom<'bounds>) : Vdom<'bounds> =
        { vdom with
            Constraints = constraints
        }

    /// <summary>Creates a text content component displaying the given string.</summary>
    /// <remarks>
    /// The <c>onReceiveFocus</c> parameter is only relevant when running WoofWare.Zoomies in 'automatic focus-handling'
    /// mode.
    /// In that case, the component starts unfocused, and if <c>onReceiveFocus</c> is <c>Some</c>, the component can
    /// receive keyboard focus.
    /// </remarks>
    let textContent (onReceiveFocus : (unit -> unit) option) s : Vdom<DesiredBounds> =
        VdomContent.TextContent (s, false, onReceiveFocus)
        |> ofContent DesiredBounds.Default

    /// <summary>Creates a split panel where two components share space according to a proportion.</summary>
    /// <remarks>
    /// The first component <c>c1</c> receives proportion <c>p</c> of the space, and the second component <c>c2</c> receives <c>1 - p</c>.
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="p">Proportion of the space to allocate to the first component (that is, the top or left one). Must be between 0 and 1, exclusive.</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component.</param>
    /// <exception cref="ArgumentException">The proportion <c>p</c> was not between 0 and 1, exclusive.</exception>
    let panelSplitProportion
        (d : SplitDirection)
        (p : float)
        (c1 : Vdom<DesiredBounds>)
        (c2 : Vdom<DesiredBounds>)
        : Vdom<DesiredBounds>
        =
        if not (p > 0.0 && p < 1.0) then
            invalidArg "p" "proportion must be between 0 and 1"

        VdomContent.PanelSplit (d, SplitBehaviour.Proportion p, c1, c2)
        |> ofContent DesiredBounds.Default

    /// <summary>Creates a split panel where the first component receives a fixed number of cells.</summary>
    /// <remarks>
    /// The first component <c>c1</c> receives exactly <c>p</c> cells, and the second component <c>c2</c> receives all remaining space.
    /// </remarks>
    /// <param name="d">Determines whether components are arranged left/right (<c>Vertical</c>, first component is left)
    /// or top/bottom (<c>Horizontal</c>, first component is top).</param>
    /// <param name="p">The number of cells to allocate to the first component (that is, the top or left one).</param>
    /// <param name="c1">The Vdom to display in the first (top or left) component.</param>
    /// <param name="c2">The Vdom to display in the second (bottom or right) component.</param>
    let panelSplitAbsolute
        (d : SplitDirection)
        (p : int)
        (c1 : Vdom<DesiredBounds>)
        (c2 : Vdom<DesiredBounds>)
        : Vdom<DesiredBounds>
        =
        VdomContent.PanelSplit (d, SplitBehaviour.Absolute p, c1, c2)
        |> ofContent DesiredBounds.Default

    /// <summary>Creates a checkbox component with the specified state.</summary>
    /// <param name="onReceiveFocus">When running WoofWare.Zoomies in 'automatic focus-handling' mode,
    /// this callback is invoked whenever the checkbox receives focus. Use this to update your application state so you
    /// know which component is focused.</param>
    /// <param name="isFocused">
    /// Specifies that this checkbox currently has keyboard focus. Derive the value of this parameter from your
    /// application state. (Required because checkboxes render in such a way that focus is visible.)
    /// </param>
    /// <param name="isChecked">Specifies that this checkbox is currently checked. Derive the value of this parameter
    /// from your application state.</param>
    let checkbox (onReceiveFocus : unit -> unit) (isFocused : bool) (isChecked : bool) : Vdom<DesiredBounds> =
        let bounds =
            {
                DesiredBounds.Height = SizeConstraint.Fixed 1
                Width =
                    // at render time, we'll check how much space we were given, so we render either as
                    // `☑︎`, or as ` ☑︎ ` vs `[☑︎]`, respectively.
                    SizeConstraint.Preferred (SizeConstraint.Fixed 1, SizeConstraint.Fixed 3)
                Priority = 100
            }

        VdomContent.Checkbox (isChecked, isFocused, onReceiveFocus) |> ofContent bounds

    /// Creates a bordered wrapper around a component, drawing a border around its content.
    let bordered (inner : Vdom<'bounds>) : Vdom<'bounds> =
        VdomContent.Bordered inner |> ofContent DesiredBounds.Default

    /// Creates a checkbox with a text label positioned to its right.
    /// The checkbox occupies 3 cells on the left, and the label text fills the remaining space.
    let labelledCheckbox
        (onReceiveFocus : unit -> unit)
        (isFocused : bool)
        (isChecked : bool)
        (label : string)
        : Vdom<DesiredBounds>
        =
        // TODO: centre this text horizontally so it's next to the checkbox
        textContent None label
        |> panelSplitAbsolute SplitDirection.Vertical 3 (checkbox onReceiveFocus isFocused isChecked)

    /// Performs a catamorphism (bottom-up fold) over a Vdom tree using the provided VdomCata implementation.
    /// Each child is recursively processed before its parent, allowing you to compute aggregate information or transform the tree.
    let rec cata<'bounds, 'ret> (c : VdomCata<'ret>) (vdom : Vdom<'bounds>) : 'ret =
        match vdom.Content with
        | VdomContent.Bordered vdom -> c.AtBordered vdom.Constraints (cata c vdom)
        | VdomContent.PanelSplit (direction, prop, child1, child2) ->
            c.AtPanelSplit vdom.Constraints direction prop (cata c child1) (cata c child2)
        | VdomContent.TextContent (s, focused, onReceiveFocus) ->
            c.AtTextContent vdom.Constraints s focused onReceiveFocus
        | VdomContent.Checkbox (isChecked, isFocused, onReceiveFocus) ->
            c.AtCheckbox vdom.Constraints isChecked isFocused onReceiveFocus

    /// Identity catamorphism that reconstructs the Vdom tree unchanged.
    /// This is here as a sanity check and as an example; it's not directly useful.
    let idCata : VdomCata<Vdom<'bounds>> =
        { new VdomCata<_> with
            member _.AtBordered bounds v =
                VdomContent.Bordered v |> ofContent bounds

            member _.AtCheckbox bounds isChecked isFocused onReceiveFocus =
                VdomContent.Checkbox (isChecked, isFocused, onReceiveFocus) |> ofContent bounds

            member _.AtPanelSplit bounds dir prop child1 child2 =
                VdomContent.PanelSplit (dir, prop, child1, child2) |> ofContent bounds

            member _.AtTextContent bounds contents focused onReceiveFocus =
                VdomContent.TextContent (contents, focused, onReceiveFocus) |> ofContent bounds
        }

    /// Tracks the focus state during a traversal of the Vdom tree, used for implementing focus navigation when running
    /// WoofWare.Zoomies in 'automatic focus-handling' mode.
    type internal FocusState =
        {
            /// The first focusable element encountered in tree order, regardless of current focus.
            /// Use this to wrap focus from the end back to the beginning.
            FirstUnfocusedAbsolute : (unit -> unit) option
            /// The first focusable element encountered after the currently focused element in tree order.
            /// Use this to advance focus to the next element.
            FirstUnfocusedAfter : (unit -> unit) option
            /// Whether a focused element has been found during traversal.
            FocusFound : bool
        }

    /// Catamorphism that computes focus navigation information for the tree.
    /// Use with the cata function to find the next focusable element after the currently focused one.
    /// Assumes at most one element has focus in the tree.
    let internal advanceFocusCata : VdomCata<FocusState> =
        { new VdomCata<_> with
            member _.AtBordered _ v = v

            member _.AtCheckbox _ isChecked isFocused onReceiveFocus =
                if isFocused then
                    {
                        FirstUnfocusedAfter = None
                        FirstUnfocusedAbsolute = None
                        FocusFound = true
                    }
                else
                    {
                        FirstUnfocusedAfter = None
                        FirstUnfocusedAbsolute = Some onReceiveFocus
                        FocusFound = false
                    }

            member _.AtTextContent _ contents isFocused onReceiveFocus =
                if isFocused then
                    {
                        FirstUnfocusedAbsolute = None
                        FirstUnfocusedAfter = None
                        FocusFound = true
                    }
                else
                    {
                        FirstUnfocusedAbsolute = onReceiveFocus
                        FirstUnfocusedAfter = onReceiveFocus
                        FocusFound = false
                    }

            member _.AtPanelSplit _ dir prop child1 child2 =
                if child1.FocusFound then
                    {
                        FocusFound = true
                        FirstUnfocusedAfter = child1.FirstUnfocusedAfter |> Option.orElse child2.FirstUnfocusedAbsolute
                        FirstUnfocusedAbsolute =
                            child1.FirstUnfocusedAbsolute |> Option.orElse child2.FirstUnfocusedAbsolute
                    }
                elif child2.FocusFound then
                    {
                        FocusFound = true
                        FirstUnfocusedAfter = child2.FirstUnfocusedAfter
                        FirstUnfocusedAbsolute =
                            child1.FirstUnfocusedAbsolute |> Option.orElse child2.FirstUnfocusedAbsolute
                    }
                else
                    {
                        FocusFound = false
                        FirstUnfocusedAfter = None
                        FirstUnfocusedAbsolute =
                            child1.FirstUnfocusedAbsolute |> Option.orElse child2.FirstUnfocusedAbsolute
                    }
        }
