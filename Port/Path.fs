// Human reviewed
namespace WoofWare.Zoomies.Port

open System
open System.Text

// Path tracking for computation graphs - represents the location of a component in the tree
module Path =

    [<RequireQualifiedAccess>]
    module Elem =

        [<RequireQualifiedAccess>]
        [<StructuralComparison>]
        [<StructuralEquality>]
        type Elem =
            | SubstFrom
            | SubstInto
            | Assoc of Keyed.Keyed
            | Switch of int

        let toString (elem : Elem) =
            let offset = int 'a'
            let lowerNibbleToAlpha c = (c &&& 0b1111) + offset |> char

            let charToAlpha (buf : StringBuilder) (c : char) =
                let c = int c
                let lower = lowerNibbleToAlpha c
                let upper = lowerNibbleToAlpha (c >>> 4)
                buf.Append(upper).Append (lower) |> ignore

            let keyedToString (k : Keyed.Keyed) =
                let buf = StringBuilder ()
                // For now, use a simplified string representation
                let keyStr = Keyed.toString k

                for c in keyStr do
                    charToAlpha buf c

                buf.ToString ()

            let intToString (i : int) =
                let buf = StringBuilder ()
                let str = i.ToString ()

                for c in str do
                    charToAlpha buf c

                buf.ToString ()

            match elem with
            | Elem.SubstFrom -> "x"
            | Elem.SubstInto -> "y"
            | Elem.Assoc k -> keyedToString k
            | Elem.Switch i -> intToString i

    [<RequireQualifiedAccess>]
    module RunLengthEncoding =
        type Run =
            {
                Element : Elem.Elem
                Count : int
            }

        let rec compareRLE (a : Run list) (b : Run list) =
            match a, b with
            | [], [] -> 0
            | [], _ -> -1
            | _, [] -> 1
            | aRun :: aRest, bRun :: bRest ->
                let c = compare aRun.Element bRun.Element

                if c = 0 then
                    match aRun.Count - bRun.Count with
                    | 0 -> compare aRest bRest
                    | diff when diff > 0 ->
                        compare
                            ({ aRun with
                                Count = diff
                             }
                             :: aRest)
                            bRest
                    | diff ->
                        compare
                            aRest
                            ({ bRun with
                                Count = -diff
                             }
                             :: bRest)
                else
                    c


        [<CustomComparison ; CustomEquality>]
        type RunLengthEncoding =
            {
                List : Run list
            }

            interface IComparable<RunLengthEncoding> with
                member this.CompareTo (other : RunLengthEncoding) : int =
                    if obj.ReferenceEquals (this, other) then
                        0
                    else
                        compareRLE this.List other.List

            interface IComparable with
                member this.CompareTo (obj : obj) : int =
                    match obj with
                    | :? RunLengthEncoding as other -> (this :> IComparable<RunLengthEncoding>).CompareTo other
                    | _ -> invalidArg "obj" "type mismatch"

            interface IEquatable<RunLengthEncoding> with
                member this.Equals (other : RunLengthEncoding) : bool =
                    (this :> IComparable<RunLengthEncoding>).CompareTo (other) = 0

            override this.Equals (obj : obj) : bool =
                match obj with
                | :? RunLengthEncoding as other -> (this :> IEquatable<RunLengthEncoding>).Equals other
                | _ -> false

            override this.GetHashCode () : int = this.List.GetHashCode ()

        let ofElemList (elements : Elem.Elem list) : RunLengthEncoding =
            let rec helper acc elements =
                match acc, elements with
                | [], first :: rest ->
                    helper
                        [
                            {
                                Element = first
                                Count = 1
                            }
                        ]
                        rest
                | _, [] -> List.rev acc
                | current :: accRest, elem :: rest ->
                    if current.Element = elem then
                        let updated =
                            { current with
                                Count = current.Count + 1
                            }

                        helper (updated :: accRest) rest
                    else
                        let newRun =
                            {
                                Element = elem
                                Count = 1
                            }

                        helper (newRun :: current :: accRest) rest

            {
                List = helper [] elements
            }

    // Internal representation for string building
    [<RequireQualifiedAccess>]
    type StringRepr<'a> =
        | Stringified of string
        | Parts of parent : 'a * elem : Elem.Elem

    [<CustomComparison>]
    [<CustomEquality>]
    type Path =
        {
            ItemsReversed : Elem.Elem list
            mutable ItemsForTesting : Elem.Elem list option
            mutable StringRepr : StringRepr<Path>
            mutable RunLengthEncodedItems : RunLengthEncoding.RunLengthEncoding option
        }

        member path.RunLengthEncoding =
            match path.RunLengthEncodedItems with
            | Some items -> items
            | None ->
                let items = RunLengthEncoding.ofElemList (List.rev path.ItemsReversed)
                path.RunLengthEncodedItems <- Some items
                items

        interface IComparable<Path> with
            member this.CompareTo (other : Path) : int =
                if obj.ReferenceEquals (this, other) then
                    0
                else
                    compare this.RunLengthEncoding other.RunLengthEncoding

        interface IComparable with
            member this.CompareTo (obj : obj) : int =
                match obj with
                | :? Path as other -> (this :> IComparable<Path>).CompareTo other
                | _ -> invalidArg "obj" "type mismatch"

        interface IEquatable<Path> with
            member this.Equals (other : Path) : bool =
                (this :> IComparable<Path>).CompareTo (other) = 0

        override this.Equals (obj : obj) : bool =
            match obj with
            | :? Path as other -> (this :> IEquatable<Path>).Equals other
            | _ -> false

        override this.GetHashCode () : int = this.ItemsReversed.GetHashCode ()


    let empty =
        {
            ItemsForTesting = Some []
            ItemsReversed = []
            StringRepr = StringRepr.Stringified "bonsai_path"
            RunLengthEncodedItems =
                Some
                    {
                        List = []
                    }
        }

    let append (path : Path) (elem : Elem.Elem) =
        {
            ItemsReversed = elem :: path.ItemsReversed
            ItemsForTesting = None
            StringRepr = StringRepr.Parts (path, elem)
            RunLengthEncodedItems = None
        }

    let rec toUniqueIdentifierString (path : Path) =
        match path.StringRepr with
        | StringRepr.Stringified s -> s
        | StringRepr.Parts (parent, elem) ->
            let parentStr = toUniqueIdentifierString parent
            let result = parentStr + "_" + Elem.toString elem
            path.StringRepr <- StringRepr.Stringified result
            result

    let raiseDuplicate (path : Path) : 'a =
        failwithf
            "BUG: [Bonsai.Path.t] should be unique for all components, but duplicate paths were discovered. Path: %A"
            path

    module ForTesting =
        let items (path : Path) =
            match path.ItemsForTesting with
            | Some items -> items
            | None ->
                let items = List.rev path.ItemsReversed
                path.ItemsForTesting <- Some items
                items

        let slowButCorrectCompareForBisimulation (a : Path) (b : Path) =
            if obj.ReferenceEquals (a, b) then
                0
            else
                let itemsA = items a
                let itemsB = items b

                let rec compareItems listA listB =
                    match listA, listB with
                    | [], [] -> 0
                    | [], _ -> -1
                    | _, [] -> 1
                    | x :: xs, y :: ys ->
                        let c = compare x y
                        if c = 0 then compareItems xs ys else c

                compareItems itemsA itemsB
