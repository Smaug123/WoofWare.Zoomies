namespace WoofWare.Zoomies.Port

open TypeEquality
open WoofWare.Zoomies.Port.External

// Create type IDs for common types
module private MetaTypeIds =
    let unitTypeId = TypeId.create<unit> "unit"

[<RequireQualifiedAccess>]
module Model =

    /// Model type identifier - GADT converted to Crate pattern
    type ModelIdEval<'ret> =
        abstract Eval<'a> : 'a ModelId -> 'ret

    and ModelIdCrate =
        abstract Apply<'ret> : ModelIdEval<'ret> -> 'ret

    and ModelId<'a> =
        | Leaf of TypeId<'a>
        | Tuple of TupleModelIdCrate<'a>
        | Map of MapModelIdCrate<'a>
        | MapOn of MapOnModelIdCrate<'a>
        | MultiModel of Map<int, HiddenModel> * Teq<'a, Map<int, HiddenModel>>

    and TupleModelIdEval<'a, 'ret> =
        abstract Eval<'a1, 'a2> : 'a1 ModelId * 'a2 ModelId * Teq<'a, 'a1 * 'a2> -> 'ret

    and TupleModelIdCrate<'a> =
        abstract Apply<'ret> : TupleModelIdEval<'a, 'ret> -> 'ret

    and MapModelIdEval<'a, 'ret> =
        abstract Eval<'k, 'result when 'k : comparison> :
            TypeId<'k> * 'result ModelId * Teq<'a, Map<'k, 'result>> -> 'ret

    and MapModelIdCrate<'a> =
        abstract Apply<'ret> : MapModelIdEval<'a, 'ret> -> 'ret

    and MapOnModelIdEval<'a, 'ret> =
        abstract Eval<'k_model, 'k_io, 'result when 'k_model : comparison> :
            TypeId<'k_model> * TypeId<'k_io> * 'result ModelId * Teq<'a, Map<'k_model, 'k_io * 'result>> -> 'ret

    and MapOnModelIdCrate<'a> =
        abstract Apply<'ret> : MapOnModelIdEval<'a, 'ret> -> 'ret

    and MultiModelIdEval<'ret> =
        abstract Eval : Map<int, HiddenModel> -> 'ret

    and MultiModelIdCrate =
        abstract Apply<'ret> : MultiModelIdEval<'ret> -> 'ret

    and Model<'a> =
        {
            Default : 'a
            Equal : 'a -> 'a -> bool
            TypeId : 'a ModelId
            SexpOf : 'a -> string
        }

    and HiddenModelEval<'ret> =
        abstract Eval<'m> : 'm * 'm Model -> 'ret

    and HiddenModel =
        abstract Apply<'ret> : HiddenModelEval<'ret> -> 'ret

    [<RequireQualifiedAccess>]
    module TupleModelIdCrate =
        let make<'a1, 'a2> (a : 'a1 ModelId) (b : 'a2 ModelId) : TupleModelIdCrate<'a1 * 'a2> =
            { new TupleModelIdCrate<_> with
                member _.Apply eval = eval.Eval<'a1, 'a2> (a, b, Teq.refl)
            }

    [<RequireQualifiedAccess>]
    module MapModelIdCrate =
        let make<'k, 'result when 'k : comparison> (k : TypeId<'k>) (by : 'result ModelId) : MapModelIdCrate<_> =
            { new MapModelIdCrate<_> with
                member _.Apply eval =
                    eval.Eval<'k, 'result> (k, by, Teq.refl)
            }

    [<RequireQualifiedAccess>]
    module MapOnModelIdCrate =
        let make<'k_model, 'k_io, 'result when 'k_model : comparison>
            (kModel : TypeId<'k_model>)
            (kIo : TypeId<'k_io>)
            (by : 'result ModelId)
            : MapOnModelIdCrate<_>
            =
            { new MapOnModelIdCrate<_> with
                member _.Apply eval =
                    eval.Eval<'k_model, 'k_io, 'result> (kModel, kIo, by, Teq.refl)
            }

    [<RequireQualifiedAccess>]
    module MultiModelIdCrate =
        let make (multiModel : Map<int, HiddenModel>) : MultiModelIdCrate =
            { new MultiModelIdCrate with
                member _.Apply eval = eval.Eval multiModel
            }

    [<RequireQualifiedAccess>]
    module HiddenModel =
        let make<'m> (model : 'm) (info : 'm Model) : HiddenModel =
            { new HiddenModel with
                member _.Apply eval = eval.Eval<'m> (model, info)
            }

        let sexpOf (hidden : HiddenModel) : string =
            hidden.Apply
                { new HiddenModelEval<string> with
                    member _.Eval (model, info) = info.SexpOf model
                }

        let equal (hidden1 : HiddenModel) (hidden2 : HiddenModel) : bool =
            hidden1.Apply
                { new HiddenModelEval<bool> with
                    member _.Eval (m1, info1) =
                        hidden2.Apply
                            { new HiddenModelEval<bool> with
                                member _.Eval (m2, info2) =
                                    // Type equality check would need runtime type comparison
                                    // For now, use reference equality as approximation
                                    if obj.ReferenceEquals (info1, info2) then
                                        info1.Equal m1 (unbox m2)
                                    else
                                        false
                            }
                }

    let unit : unit Model =
        {
            Default = ()
            Equal = (=)
            TypeId = Leaf MetaTypeIds.unitTypeId
            SexpOf = fun () -> "()"
        }

    let both (model1 : 'a Model) (model2 : 'b Model) : ('a * 'b) Model =
        {
            Default = (model1.Default, model2.Default)
            Equal = fun (a1, b1) (a2, b2) -> model1.Equal a1 a2 && model2.Equal b1 b2
            TypeId = Tuple (TupleModelIdCrate.make model1.TypeId model2.TypeId)
            SexpOf = fun (a, b) -> sprintf "(%s, %s)" (model1.SexpOf a) (model2.SexpOf b)
        }

    let map<'k, 'a when 'k : comparison> (kTypeId : TypeId<'k>) (model : 'a Model) : Map<'k, 'a> Model =
        {
            Default = Map.empty
            Equal =
                fun m1 m2 ->
                    if Map.count m1 <> Map.count m2 then
                        false
                    else
                        Map.forall
                            (fun k v1 ->
                                match Map.tryFind k m2 with
                                | Some v2 -> model.Equal v1 v2
                                | None -> false
                            )
                            m1
            TypeId = Map (MapModelIdCrate.make kTypeId model.TypeId)
            SexpOf =
                fun m ->
                    let pairs =
                        Map.toSeq m |> Seq.map (fun (k, v) -> sprintf "%A: %s" k (model.SexpOf v))

                    sprintf "Map[%s]" (String.concat "; " pairs)
        }

    let ofModule<'a when 'a : equality>
        (sexpOf : 'a -> string)
        (equal : ('a -> 'a -> bool) option)
        (defaultValue : 'a)
        (name : string)
        : 'a Model
        =
        let equal = defaultArg equal (=)
        let typeId = TypeId.create<'a> (sprintf "%s-model" name)

        {
            Default = defaultValue
            Equal = equal
            TypeId = Leaf typeId
            SexpOf = sexpOf
        }

[<RequireQualifiedAccess>]
module MultiModel =

    type MultiModel = Map<int, Model.HiddenModel>

    let sexpOf (mm : MultiModel) : string =
        let pairs =
            Map.toSeq mm
            |> Seq.map (fun (k, v) -> sprintf "%d: %s" k (Model.HiddenModel.sexpOf v))

        sprintf "MultiModel[%s]" (String.concat "; " pairs)

    let findExn (mm : MultiModel) (key : int) : Model.HiddenModel = Map.find key mm

    let set (mm : MultiModel) (key : int) (data : Model.HiddenModel) : MultiModel = Map.add key data mm

    let toModels (mm : MultiModel) : Map<int, Model.HiddenModel> = mm
    let ofModels (models : Map<int, Model.HiddenModel>) : MultiModel = models

    let modelInfo (defaultValue : MultiModel) : MultiModel Model.Model =
        {
            Default = defaultValue
            Equal =
                fun m1 m2 ->
                    if Map.count m1 <> Map.count m2 then
                        false
                    else
                        Map.forall
                            (fun k v1 ->
                                match Map.tryFind k m2 with
                                | Some v2 -> Model.HiddenModel.equal v1 v2
                                | None -> false
                            )
                            m1
            TypeId = Model.MultiModel (defaultValue, Teq.refl)
            SexpOf = sexpOf
        }

[<RequireQualifiedAccess>]
module MetaInput =

    // Input uses the same ModelId structure as Model
    type Input<'a> = 'a Model.ModelId

    let sameWitness<'a, 'b> (a : Input<'a>) (b : Input<'b>) : Teq<'a, 'b> option =
        // Simplified implementation - would need full GADT pattern matching
        None

    let sameWitnessExn<'a, 'b> (a : Input<'a>) (b : Input<'b>) : Teq<'a, 'b> =
        match sameWitness a b with
        | Some teq -> teq
        | None -> failwith "Type witness failed"

    let unit : Input<unit> = Model.Leaf MetaTypeIds.unitTypeId

    let both<'a, 'b> (a : Input<'a>) (b : Input<'b>) : Input<'a * 'b> =
        Model.Tuple (Model.TupleModelIdCrate.make a b)

    let map<'k, 'a when 'k : comparison> (k : TypeId<'k>) (by : Input<'a>) : Input<Map<'k, 'a>> =
        Model.Map (Model.MapModelIdCrate.make k by)

    let create<'a> () : Input<'a> = Model.Leaf (TypeId.create<'a> "input")

    [<RequireQualifiedAccess>]
    module Hidden =

        type InputHiddenEval<'ret> =
            abstract Eval<'input, 'key> : 'input * Input<'input> * 'key -> 'ret

        type InputHidden<'key> =
            abstract Apply<'ret> : InputHiddenEval<'ret> -> 'ret

        [<RequireQualifiedAccess>]
        module InputHidden =
            let make<'input, 'key> (input : 'input) (typeId : Input<'input>) (key : 'key) : InputHidden<'key> =
                { new InputHidden<'key> with
                    member _.Apply eval =
                        eval.Eval<'input, 'key> (input, typeId, key)
                }

        let unitInput : Input<InputHidden<unit>> =
            Model.Leaf (TypeId.create<InputHidden<unit>> "lazy input")

        let intInput : Input<InputHidden<int>> =
            Model.Leaf (TypeId.create<InputHidden<int>> "enum input")
