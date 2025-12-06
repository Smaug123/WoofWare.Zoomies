namespace WoofWare.Zoomies

/// Description of how to combine cases during a fold
type VdomCataCase<'Vdom, 'KeyedVdom, 'UnkeyedVdom> =
    /// How to operate on the Keyed case
    abstract Keyed : 'KeyedVdom -> 'Vdom
    /// How to operate on the Unkeyed case
    abstract Unkeyed : 'UnkeyedVdom -> 'Vdom

/// Description of how to combine cases during a fold
type KeyedVdomCataCase<'Vdom, 'KeyedVdom, 'UnkeyedVdom> =
    /// How to operate on the WithKey case
    abstract WithKey : NodeKey -> 'UnkeyedVdom -> 'KeyedVdom

/// Description of how to combine cases during a fold
type UnkeyedVdomCataCase<'Vdom, 'KeyedVdom, 'UnkeyedVdom> =
    /// How to operate on the Bordered case
    abstract Bordered : 'Vdom -> 'UnkeyedVdom

    /// How to operate on the PanelSplit case
    abstract PanelSplit : SplitDirection -> SplitBehaviour -> child1 : 'Vdom -> child2 : 'Vdom -> 'UnkeyedVdom

    /// How to operate on the TextContent case
    abstract TextContent :
        content : string ->
        style : CellStyle ->
        alignment : ContentAlignment ->
        focused : bool ->
        wrap : bool ->
            'UnkeyedVdom

    /// How to operate on the Focusable case
    abstract Focusable : isFirstToFocus : bool -> isInitiallyFocused : bool -> 'KeyedVdom -> 'UnkeyedVdom
    /// How to operate on the Empty case
    abstract Empty : 'UnkeyedVdom

    /// How to operate on the FlexibleContent case
    abstract FlexibleContent :
        measure : (MeasureConstraints -> MeasuredSize) -> render : (Rectangle -> 'Vdom) -> 'UnkeyedVdom

    /// How to operate on the Tag case
    abstract Tag : tag : string -> inner : 'Vdom -> 'UnkeyedVdom

/// Specifies how to perform a fold (catamorphism) over the type UnkeyedVdom and its friends.
type VdomCata'<'Vdom, 'KeyedVdom, 'UnkeyedVdom> =
    {
        /// How to perform a fold (catamorphism) over the type Vdom
        Vdom : VdomCataCase<'Vdom, 'KeyedVdom, 'UnkeyedVdom>
        /// How to perform a fold (catamorphism) over the type KeyedVdom
        KeyedVdom : KeyedVdomCataCase<'Vdom, 'KeyedVdom, 'UnkeyedVdom>
        /// How to perform a fold (catamorphism) over the type UnkeyedVdom
        UnkeyedVdom : UnkeyedVdomCataCase<'Vdom, 'KeyedVdom, 'UnkeyedVdom>
    }

/// Specialisation of <see cref="VdomCata'" /> where all result types are the same.
type VdomCata<'r> = VdomCata'<'r, 'r, 'r>

/// Methods to perform a catamorphism over the type UnkeyedVdom
[<RequireQualifiedAccess>]
module UnkeyedVdomCata =
    [<RequireQualifiedAccess>]
    type private Instruction =
        | Process__Vdom of Vdom<DesiredBounds>
        | Process__KeyedVdom of KeyedVdom<DesiredBounds>
        | Process__UnkeyedVdom of UnkeyedVdom<DesiredBounds>
        | Vdom_Keyed
        | Vdom_Unkeyed
        | KeyedVdom_WithKey of NodeKey
        | UnkeyedVdom_Bordered
        | UnkeyedVdom_PanelSplit of SplitDirection * SplitBehaviour
        | UnkeyedVdom_Focusable of isFirstToFocus : bool * isInitiallyFocused : bool
        | UnkeyedVdom_Tag of tag : string

    let rec private loop<'Vdom, 'KeyedVdom, 'UnkeyedVdom>
        (cata : VdomCata'<'Vdom, 'KeyedVdom, 'UnkeyedVdom>)
        (instructions : ResizeArray<Instruction>)
        =
        let unkeyedVdomStack = ResizeArray<'UnkeyedVdom> ()
        let keyedVdomStack = ResizeArray<'KeyedVdom> ()
        let vdomStack = ResizeArray<'Vdom> ()

        while instructions.Count > 0 do
            let currentInstruction = instructions.[instructions.Count - 1]
            instructions.RemoveAt (instructions.Count - 1)

            match currentInstruction with
            | Instruction.Process__Vdom x ->
                match x with
                | Vdom.Keyed arg0_0 ->
                    instructions.Add Instruction.Vdom_Keyed
                    instructions.Add (Instruction.Process__KeyedVdom arg0_0)
                | Vdom.Unkeyed arg0_0 ->
                    instructions.Add Instruction.Vdom_Unkeyed
                    instructions.Add (Instruction.Process__UnkeyedVdom arg0_0)
            | Instruction.Process__KeyedVdom x ->
                match x with
                | KeyedVdom.KeyedVdom (arg0_0, arg1_0) ->
                    instructions.Add (Instruction.KeyedVdom_WithKey arg0_0)
                    instructions.Add (Instruction.Process__UnkeyedVdom arg1_0)
            | Instruction.Process__UnkeyedVdom x ->
                match x with
                | UnkeyedVdom.Bordered (arg0_0) ->
                    instructions.Add Instruction.UnkeyedVdom_Bordered
                    instructions.Add (Instruction.Process__Vdom arg0_0)
                | UnkeyedVdom.PanelSplit (arg0_0, arg1_0, child1, child2) ->
                    instructions.Add (Instruction.UnkeyedVdom_PanelSplit (arg0_0, arg1_0))
                    instructions.Add (Instruction.Process__Vdom child1)
                    instructions.Add (Instruction.Process__Vdom child2)
                | UnkeyedVdom.TextContent (content, style, alignment, focused, wrap) ->
                    cata.UnkeyedVdom.TextContent content style alignment focused wrap
                    |> unkeyedVdomStack.Add
                | UnkeyedVdom.Focusable (isFirstToFocus, isInitiallyFocused, arg2_0) ->
                    instructions.Add (Instruction.UnkeyedVdom_Focusable (isFirstToFocus, isInitiallyFocused))
                    instructions.Add (Instruction.Process__KeyedVdom arg2_0)
                | UnkeyedVdom.Empty -> cata.UnkeyedVdom.Empty |> unkeyedVdomStack.Add
                | UnkeyedVdom.FlexibleContent ({
                                                   Measure = measure
                                                   Render = render
                                               }) ->
                    // Fold the render function so that it returns the catamorphism result
                    let foldedRender bounds = runVdom cata (render bounds)
                    cata.UnkeyedVdom.FlexibleContent measure foldedRender |> unkeyedVdomStack.Add
                | UnkeyedVdom.Tag (tag, inner) ->
                    instructions.Add (Instruction.UnkeyedVdom_Tag tag)
                    instructions.Add (Instruction.Process__Vdom inner)
            | Instruction.Vdom_Keyed ->
                let arg0_0 = keyedVdomStack.[keyedVdomStack.Count - 1]
                keyedVdomStack.RemoveAt (keyedVdomStack.Count - 1)
                cata.Vdom.Keyed arg0_0 |> vdomStack.Add
            | Instruction.Vdom_Unkeyed ->
                let arg0_0 = unkeyedVdomStack.[unkeyedVdomStack.Count - 1]
                unkeyedVdomStack.RemoveAt (unkeyedVdomStack.Count - 1)
                cata.Vdom.Unkeyed arg0_0 |> vdomStack.Add
            | Instruction.KeyedVdom_WithKey arg0_0 ->
                let arg1_0 = unkeyedVdomStack.[unkeyedVdomStack.Count - 1]
                unkeyedVdomStack.RemoveAt (unkeyedVdomStack.Count - 1)
                cata.KeyedVdom.WithKey arg0_0 arg1_0 |> keyedVdomStack.Add
            | Instruction.UnkeyedVdom_Bordered ->
                let arg0_0 = vdomStack.[vdomStack.Count - 1]
                vdomStack.RemoveAt (vdomStack.Count - 1)
                cata.UnkeyedVdom.Bordered arg0_0 |> unkeyedVdomStack.Add
            | Instruction.UnkeyedVdom_PanelSplit (arg0_0, arg1_0) ->
                let child1 = vdomStack.[vdomStack.Count - 1]
                vdomStack.RemoveAt (vdomStack.Count - 1)
                let child2 = vdomStack.[vdomStack.Count - 1]
                vdomStack.RemoveAt (vdomStack.Count - 1)
                cata.UnkeyedVdom.PanelSplit arg0_0 arg1_0 child1 child2 |> unkeyedVdomStack.Add
            | Instruction.UnkeyedVdom_Focusable (isFirstToFocus, isInitiallyFocused) ->
                let arg2_0 = keyedVdomStack.[keyedVdomStack.Count - 1]
                keyedVdomStack.RemoveAt (keyedVdomStack.Count - 1)

                cata.UnkeyedVdom.Focusable isFirstToFocus isInitiallyFocused arg2_0
                |> unkeyedVdomStack.Add
            | Instruction.UnkeyedVdom_Tag tag ->
                let inner = vdomStack.[vdomStack.Count - 1]
                vdomStack.RemoveAt (vdomStack.Count - 1)
                cata.UnkeyedVdom.Tag tag inner |> unkeyedVdomStack.Add

        vdomStack, keyedVdomStack, unkeyedVdomStack

    /// Execute the catamorphism.
    and internal runVdom<'VdomRet, 'KeyedVdomRet, 'UnkeyedVdomRet>
        (cata : VdomCata'<'VdomRet, 'KeyedVdomRet, 'UnkeyedVdomRet>)
        (x : Vdom<DesiredBounds>)
        : 'VdomRet
        =
        let instructions = ResizeArray ()
        instructions.Add (Instruction.Process__Vdom x)

        let vdomRetStack, keyedVdomRetStack, unkeyedVdomRetStack =
            loop<'VdomRet, 'KeyedVdomRet, 'UnkeyedVdomRet> cata instructions

        Seq.exactlyOne vdomRetStack

    /// Execute the catamorphism.
    and internal runKeyedVdom<'VdomRet, 'KeyedVdomRet, 'UnkeyedVdomRet>
        (cata : VdomCata'<'VdomRet, 'KeyedVdomRet, 'UnkeyedVdomRet>)
        (x : KeyedVdom<DesiredBounds>)
        : 'KeyedVdomRet
        =
        let instructions = ResizeArray ()
        instructions.Add (Instruction.Process__KeyedVdom x)

        let vdomRetStack, keyedVdomRetStack, unkeyedVdomRetStack = loop cata instructions

        Seq.exactlyOne keyedVdomRetStack

    /// Execute the catamorphism.
    and internal runUnkeyedVdom<'VdomRet, 'KeyedVdomRet, 'UnkeyedVdomRet>
        (cata : VdomCata'<'VdomRet, 'KeyedVdomRet, 'UnkeyedVdomRet>)
        (x : UnkeyedVdom<DesiredBounds>)
        : 'UnkeyedVdomRet
        =
        let instructions = ResizeArray ()
        instructions.Add (Instruction.Process__UnkeyedVdom x)

        let vdomRetStack, keyedVdomRetStack, unkeyedVdomRetStack = loop cata instructions

        Seq.exactlyOne unkeyedVdomRetStack

[<RequireQualifiedAccess>]
module VdomCata =
    /// Execute a catamorphism over a Vdom value.
    let run<'ret, 'keyedRet, 'unkeyedRet>
        (cata : VdomCata'<'ret, 'keyedRet, 'unkeyedRet>)
        (vdom : Vdom<DesiredBounds>)
        : 'ret
        =
        UnkeyedVdomCata.runVdom cata vdom
