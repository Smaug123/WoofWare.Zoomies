// Written by human
namespace WoofWare.Zoomies.Port

type Model<'a when 'a : equality> =
    {
        default_ : 'a
    }

module Model =

    type HiddenEval<'ret> =
        abstract Eval<'m when 'm : equality> : 'm * 'm Model -> 'ret

    and Hidden =
        abstract Apply<'ret> : HiddenEval<'ret> -> 'ret

    module Hidden =

        let create (model : 'a Model) a =
            { new Hidden with
                member _.Apply (eval : HiddenEval<'ret>) : 'ret = eval.Eval (a, model)
            }

        let lazy_ : Hidden option Model =
            {
                default_ = None
            }

    let unit =
        {
            default_ = ()
        }

    let both
        {
            default_ = a
        }
        {
            default_ = b
        }
        =
        {
            default_ = (a, b)
        }

    let map<'key, 'value when 'value : equality and 'key : comparison> (_ : 'value Model) =
        {
            default_ = Map.empty<'key, 'value>
        }

    let mapOn<'key, 'keyIo, 'value when 'value : equality and 'key : comparison and 'keyIo : comparison>
        (_ : 'value Model)
        =

        {
            default_ = Map.empty<'key, 'keyIo * 'value>
        }

    let ofModule<'a when 'a : equality>
        (sexpOf : 'a -> string)
        (equal : ('a -> 'a -> bool) option)
        (defaultValue : 'a)
        (name : string)
        : 'a Model
        =
        // For the simplified Model structure, we just need the default value
        // The sexpOf, equal, and name parameters are not used in this implementation
        {
            default_ = defaultValue
        }

type MultiModel = Map<int, Model.Hidden>

module MultiModel =

    let modelInfo (default_ : MultiModel) : MultiModel Model =
        {
            default_ = default_
        }

type MetaInput<'a> =
    {
        typeId : TypeId<'a>
    }

module MetaInput =

    type HiddenEval<'key, 'ret> =
        abstract Eval<'input> : 'input * 'input MetaInput * 'key -> 'ret

    and Hidden<'key> =
        abstract Apply<'key, 'ret> : HiddenEval<'key, 'ret> -> 'ret


    let create<'a> () : 'a MetaInput =
        {
            typeId = TypeId.create "input"
        }

    let both (_ : 'a MetaInput) (_ : 'b MetaInput) = create<'a * 'b> ()

    let unit : unit MetaInput =
        {
            typeId = TypeId.create "lazy input"
        }

    let int : int MetaInput =
        {
            typeId = TypeId.create "enum input"
        }
