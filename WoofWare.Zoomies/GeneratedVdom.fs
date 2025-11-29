namespace WoofWare.Zoomies

/// Description of how to combine cases during a fold
type KeylessVdomCataCase<'KeylessVdom, 'KeyedVdom, 'UnkeyedVdom> =
    /// How to operate on the Keyed case
    abstract Keyed : 'KeyedVdom -> 'KeylessVdom
    /// How to operate on the Unkeyed case
    abstract Unkeyed : 'UnkeyedVdom -> 'KeylessVdom

/// Description of how to combine cases during a fold
type KeyedVdomCataCase<'KeylessVdom, 'KeyedVdom, 'UnkeyedVdom> =
    /// How to operate on the WithKey case
    abstract WithKey : NodeKey -> 'UnkeyedVdom -> 'KeyedVdom

/// Description of how to combine cases during a fold
type UnkeyedVdomCataCase<'KeylessVdom, 'KeyedVdom, 'UnkeyedVdom> =
    /// How to operate on the Bordered case
    abstract Bordered : 'KeylessVdom -> 'UnkeyedVdom

    /// How to operate on the PanelSplit case
    abstract PanelSplit :
        SplitDirection -> SplitBehaviour -> child1 : 'KeylessVdom -> child2 : 'KeylessVdom -> 'UnkeyedVdom

    /// How to operate on the TextContent case
    abstract TextContent :
        content : string -> style : CellStyle -> alignment : ContentAlignment -> focused : bool -> 'UnkeyedVdom

    /// How to operate on the Focusable case
    abstract Focusable : isFirstToFocus : bool -> isInitiallyFocused : bool -> 'KeyedVdom -> 'UnkeyedVdom
    /// How to operate on the Empty case
    abstract Empty : 'UnkeyedVdom

    /// How to operate on the FlexibleContent case
    abstract FlexibleContent :
        measure : (MeasureConstraints -> MeasuredSize) -> render : (Rectangle -> 'KeylessVdom) -> 'UnkeyedVdom

    /// How to operate on the Tag case
    abstract Tag : tag : string -> inner : 'KeylessVdom -> 'UnkeyedVdom

/// Specifies how to perform a fold (catamorphism) over the type UnkeyedVdom and its friends.
type VdomCata'<'KeylessVdom, 'KeyedVdom, 'UnkeyedVdom> =
    {
        /// How to perform a fold (catamorphism) over the type KeylessVdom
        KeylessVdom : KeylessVdomCataCase<'KeylessVdom, 'KeyedVdom, 'UnkeyedVdom>
        /// How to perform a fold (catamorphism) over the type KeyedVdom
        KeyedVdom : KeyedVdomCataCase<'KeylessVdom, 'KeyedVdom, 'UnkeyedVdom>
        /// How to perform a fold (catamorphism) over the type UnkeyedVdom
        UnkeyedVdom : UnkeyedVdomCataCase<'KeylessVdom, 'KeyedVdom, 'UnkeyedVdom>
    }

/// Specialisation of <see cref="VdomCata'" /> where all result types are the same.
type VdomCata<'r> = VdomCata'<'r, 'r, 'r>

/// Methods to perform a catamorphism over the type UnkeyedVdom
[<RequireQualifiedAccess>]
module UnkeyedVdomCata =
    [<RequireQualifiedAccess>]
    type private Instruction =
        | Process__KeylessVdom of KeylessVdom<DesiredBounds>
        | Process__KeyedVdom of KeyedVdom<DesiredBounds>
        | Process__UnkeyedVdom of UnkeyedVdom<DesiredBounds>
        | KeylessVdom_Keyed
        | KeylessVdom_Unkeyed
        | KeyedVdom_WithKey of NodeKey
        | UnkeyedVdom_Bordered
        | UnkeyedVdom_PanelSplit of SplitDirection * SplitBehaviour
        | UnkeyedVdom_Focusable of isFirstToFocus : bool * isInitiallyFocused : bool
        | UnkeyedVdom_Tag of tag : string

    let rec private loop<'KeylessVdom, 'KeyedVdom, 'UnkeyedVdom>
        (cata : VdomCata'<'KeylessVdom, 'KeyedVdom, 'UnkeyedVdom>)
        (instructions : ResizeArray<Instruction>)
        =
        let unkeyedVdomStack = ResizeArray<'UnkeyedVdom> ()
        let keyedVdomStack = ResizeArray<'KeyedVdom> ()
        let keylessVdomStack = ResizeArray<'KeylessVdom> ()

        while instructions.Count > 0 do
            let currentInstruction = instructions.[instructions.Count - 1]
            instructions.RemoveAt (instructions.Count - 1)

            match currentInstruction with
            | Instruction.Process__KeylessVdom x ->
                match x with
                | KeylessVdom.Keyed arg0_0 ->
                    instructions.Add Instruction.KeylessVdom_Keyed
                    instructions.Add (Instruction.Process__KeyedVdom arg0_0)
                | KeylessVdom.Unkeyed arg0_0 ->
                    instructions.Add Instruction.KeylessVdom_Unkeyed
                    instructions.Add (Instruction.Process__UnkeyedVdom arg0_0)
            | Instruction.Process__KeyedVdom x ->
                match x with
                | KeyedVdom.WithKey (arg0_0, arg1_0) ->
                    instructions.Add (Instruction.KeyedVdom_WithKey arg0_0)
                    instructions.Add (Instruction.Process__UnkeyedVdom arg1_0)
            | Instruction.Process__UnkeyedVdom x ->
                match x with
                | UnkeyedVdom.Bordered (arg0_0) ->
                    instructions.Add Instruction.UnkeyedVdom_Bordered
                    instructions.Add (Instruction.Process__KeylessVdom arg0_0)
                | UnkeyedVdom.PanelSplit (arg0_0, arg1_0, child1, child2) ->
                    instructions.Add (Instruction.UnkeyedVdom_PanelSplit (arg0_0, arg1_0))
                    instructions.Add (Instruction.Process__KeylessVdom child1)
                    instructions.Add (Instruction.Process__KeylessVdom child2)
                | UnkeyedVdom.TextContent (content, style, alignment, focused) ->
                    cata.UnkeyedVdom.TextContent content style alignment focused
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
                    let foldedRender bounds = runKeylessVdom cata (render bounds)
                    cata.UnkeyedVdom.FlexibleContent measure foldedRender |> unkeyedVdomStack.Add
                | UnkeyedVdom.Tag (tag, inner) ->
                    instructions.Add (Instruction.UnkeyedVdom_Tag tag)
                    instructions.Add (Instruction.Process__KeylessVdom inner)
            | Instruction.KeylessVdom_Keyed ->
                let arg0_0 = keyedVdomStack.[keyedVdomStack.Count - 1]
                keyedVdomStack.RemoveAt (keyedVdomStack.Count - 1)
                cata.KeylessVdom.Keyed arg0_0 |> keylessVdomStack.Add
            | Instruction.KeylessVdom_Unkeyed ->
                let arg0_0 = unkeyedVdomStack.[unkeyedVdomStack.Count - 1]
                unkeyedVdomStack.RemoveAt (unkeyedVdomStack.Count - 1)
                cata.KeylessVdom.Unkeyed arg0_0 |> keylessVdomStack.Add
            | Instruction.KeyedVdom_WithKey arg0_0 ->
                let arg1_0 = unkeyedVdomStack.[unkeyedVdomStack.Count - 1]
                unkeyedVdomStack.RemoveAt (unkeyedVdomStack.Count - 1)
                cata.KeyedVdom.WithKey arg0_0 arg1_0 |> keyedVdomStack.Add
            | Instruction.UnkeyedVdom_Bordered ->
                let arg0_0 = keylessVdomStack.[keylessVdomStack.Count - 1]
                keylessVdomStack.RemoveAt (keylessVdomStack.Count - 1)
                cata.UnkeyedVdom.Bordered arg0_0 |> unkeyedVdomStack.Add
            | Instruction.UnkeyedVdom_PanelSplit (arg0_0, arg1_0) ->
                let child1 = keylessVdomStack.[keylessVdomStack.Count - 1]
                keylessVdomStack.RemoveAt (keylessVdomStack.Count - 1)
                let child2 = keylessVdomStack.[keylessVdomStack.Count - 1]
                keylessVdomStack.RemoveAt (keylessVdomStack.Count - 1)
                cata.UnkeyedVdom.PanelSplit arg0_0 arg1_0 child1 child2 |> unkeyedVdomStack.Add
            | Instruction.UnkeyedVdom_Focusable (isFirstToFocus, isInitiallyFocused) ->
                let arg2_0 = keyedVdomStack.[keyedVdomStack.Count - 1]
                keyedVdomStack.RemoveAt (keyedVdomStack.Count - 1)

                cata.UnkeyedVdom.Focusable isFirstToFocus isInitiallyFocused arg2_0
                |> unkeyedVdomStack.Add
            | Instruction.UnkeyedVdom_Tag tag ->
                let inner = keylessVdomStack.[keylessVdomStack.Count - 1]
                keylessVdomStack.RemoveAt (keylessVdomStack.Count - 1)
                cata.UnkeyedVdom.Tag tag inner |> unkeyedVdomStack.Add

        keylessVdomStack, keyedVdomStack, unkeyedVdomStack

    /// Execute the catamorphism.
    and internal runKeylessVdom<'KeylessVdomRet, 'KeyedVdomRet, 'UnkeyedVdomRet>
        (cata : VdomCata'<'KeylessVdomRet, 'KeyedVdomRet, 'UnkeyedVdomRet>)
        (x : KeylessVdom<DesiredBounds>)
        : 'KeylessVdomRet
        =
        let instructions = ResizeArray ()
        instructions.Add (Instruction.Process__KeylessVdom x)

        let keylessVdomRetStack, keyedVdomRetStack, unkeyedVdomRetStack =
            loop<'KeylessVdomRet, 'KeyedVdomRet, 'UnkeyedVdomRet> cata instructions

        Seq.exactlyOne keylessVdomRetStack

    /// Execute the catamorphism.
    and internal runKeyedVdom<'KeylessVdomRet, 'KeyedVdomRet, 'UnkeyedVdomRet>
        (cata : VdomCata'<'KeylessVdomRet, 'KeyedVdomRet, 'UnkeyedVdomRet>)
        (x : KeyedVdom<DesiredBounds>)
        : 'KeyedVdomRet
        =
        let instructions = ResizeArray ()
        instructions.Add (Instruction.Process__KeyedVdom x)

        let keylessVdomRetStack, keyedVdomRetStack, unkeyedVdomRetStack =
            loop cata instructions

        Seq.exactlyOne keyedVdomRetStack

    /// Execute the catamorphism.
    and internal runUnkeyedVdom<'KeylessVdomRet, 'KeyedVdomRet, 'UnkeyedVdomRet>
        (cata : VdomCata'<'KeylessVdomRet, 'KeyedVdomRet, 'UnkeyedVdomRet>)
        (x : UnkeyedVdom<DesiredBounds>)
        : 'UnkeyedVdomRet
        =
        let instructions = ResizeArray ()
        instructions.Add (Instruction.Process__UnkeyedVdom x)

        let keylessVdomRetStack, keyedVdomRetStack, unkeyedVdomRetStack =
            loop cata instructions

        Seq.exactlyOne unkeyedVdomRetStack

[<RequireQualifiedAccess>]
module VdomCata =
    /// Execute a catamorphism over a Vdom value.
    let run (cata : VdomCata<'r>) (vdom : Vdom<DesiredBounds, 'keyed>) : 'r =
        match vdom with
        | Vdom.Keyed (keyed, _) -> UnkeyedVdomCata.runKeyedVdom cata keyed
        | Vdom.Unkeyed (unkeyed, _) -> UnkeyedVdomCata.runUnkeyedVdom cata unkeyed
