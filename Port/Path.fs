namespace WoofWare.Zoomies.Port

open System.Text

// Path tracking for computation graphs - represents the location of a component in the tree
module Path =

    [<RequireQualifiedAccess>]
    module Elem =
        let keyed (compare : 'a -> 'a -> int) (id : TypeId<'a>) : ('a -> Keyed.Keyed) =
            fun key -> Keyed.create key id compare

        [<RequireQualifiedAccess>]
        [<StructuralComparison>]
        [<StructuralEquality>]
        type Elem =
            | SubstFrom
            | SubstInto
            | Assoc of Keyed.Keyed
            | Switch of int

        let compare (a : Elem) (b : Elem) =
            match a, b with
            | Elem.SubstFrom, Elem.SubstFrom -> 0
            | Elem.SubstFrom, _ -> -1
            | _, Elem.SubstFrom -> 1
            | Elem.SubstInto, Elem.SubstInto -> 0
            | Elem.SubstInto, _ -> -1
            | _, Elem.SubstInto -> 1
            | Elem.Assoc keyedA, Elem.Assoc keyedB -> compare keyedA keyedB
            | Elem.Assoc _, _ -> -1
            | _, Elem.Assoc _ -> 1
            | Elem.Switch a, Elem.Switch b -> a.CompareTo (b)

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

        type RunLengthEncoding = Run list

        let rec compare (a : RunLengthEncoding) (b : RunLengthEncoding) =
            match a, b with
            | [], [] -> 0
            | [], _ -> -1
            | _, [] -> 1
            | aRun :: aRest, bRun :: bRest ->
                let c = Elem.compare aRun.Element bRun.Element

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
                    if Elem.compare current.Element elem = 0 then
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

            helper [] elements

    // Internal representation for string building
    [<RequireQualifiedAccess>]
    type StringRepr<'a> =
        | Stringified of string
        | Parts of parent : 'a * elem : Elem.Elem

    [<StructuralComparison>]
    [<StructuralEquality>]
    type Path =
        {
            ItemsReversed : Elem.Elem list
            mutable ItemsForTesting : Elem.Elem list option
            mutable StringRepr : StringRepr<Path>
            mutable RunLengthEncodedItems : RunLengthEncoding.RunLengthEncoding option
        }

    let private runLengthEncoding (path : Path) =
        match path.RunLengthEncodedItems with
        | Some items -> items
        | None ->
            let items = RunLengthEncoding.ofElemList (List.rev path.ItemsReversed)
            path.RunLengthEncodedItems <- Some items
            items

    let compare (a : Path) (b : Path) =
        if obj.ReferenceEquals (a, b) then
            0
        else
            RunLengthEncoding.compare (runLengthEncoding a) (runLengthEncoding b)

    let empty =
        {
            ItemsForTesting = Some []
            ItemsReversed = []
            StringRepr = StringRepr.Stringified "bonsai_path"
            RunLengthEncodedItems = Some []
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
            let elemStr = Elem.toString elem
            let result = parentStr + "_" + elemStr
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
                        let c = Elem.compare x y
                        if c = 0 then compareItems xs ys else c

                compareItems itemsA itemsB

    // Map module for Path-keyed maps
    module Map =
        type t<'a> = Map<Path, 'a>
        
        let empty<'a> : t<'a> = Map.empty
        
        let exists (map : t<'a>) (predicate : 'a -> bool) : bool =
            Map.exists (fun _ value -> predicate value) map
            
        let fold (map : t<'a>) (init : 'acc) (folder : 'acc -> Path -> 'a -> 'acc) : 'acc =
            Map.fold folder init map
            
        [<RequireQualifiedAccess>]
        type DiffResult<'a> = 
            | Left of 'a 
            | Right of 'a 
            | Unequal of 'a * 'a
        
        let foldSymmetricDiff (oldMap : t<'a>) (newMap : t<'a>) (dataEqual : 'a -> 'a -> bool) (init : 'acc) (folder : 'acc -> Path * DiffResult<'a> -> 'acc) : 'acc =
            let allKeys = Set.union (oldMap |> Map.keys |> Set.ofSeq) (newMap |> Map.keys |> Set.ofSeq)
            allKeys
            |> Set.fold (fun acc key ->
                match Map.tryFind key oldMap, Map.tryFind key newMap with
                | Some oldVal, None -> folder acc (key, DiffResult.Left oldVal)
                | None, Some newVal -> folder acc (key, DiffResult.Right newVal)
                | Some oldVal, Some newVal when not (dataEqual oldVal newVal) -> folder acc (key, DiffResult.Unequal (oldVal, newVal))
                | _ -> acc
            ) init
