namespace WoofWare.Zoomies.Port

open System.Collections.Generic
open WoofWare.Incremental

module AnnotateIncr =

    module Color =
        let red50 = "#FEF2F2"
        let red100 = "#FEE2E2"
        let red200 = "#FECACA"
        let red300 = "#FCA5A5"
        let amber100 = "#FEF3C7"
        let amber200 = "#FDE68A"
        let blue100 = "#DBEAFE"
        let blue200 = "#BFDBFE"
        let blue300 = "#93C5FD"
        let emerald100 = "#D1FAE5"
        let emerald200 = "#A7F3D0"
        let pink100 = "#FCE7F3"
        let pink200 = "#FBCFE8"
        let pink300 = "#F9A8D4"

    let mutable private enabled = false

    let enable () = enabled <- true
    let disable () = enabled <- false

    let private emptyAttrs = Map.empty<string, string>

    let private attributePackedInternal (pos : string) (t : NodeCrate) =
        let pos = pos.Replace("lib/", "").Replace ("app/", "")

        let labels =
            match pos.LastIndexOf ('/') with
            | -1 -> [ pos ]
            | i ->
                let l = pos.Substring (0, i)
                let r = pos.Substring (i + 1)
                [ "bound: " + l ; "bound: " + r ]

        for label in labels do
            let labelList = [ label ]
            // Note: WoofWare.Incremental may not have exact equivalent,
            // but we simulate the interface
            ()

    [<RequireQualifiedAccess>]
    type Kind =
        | Input
        | Value
        | Result
        | Lifecycle
        | EmptyLifecycle
        | Model
        | ModelAndInput
        | AssocKey
        | AssocInput
        | AssocResults
        | AssocLifecycles
        | AssocInputs
        | Path
        | LifecycleApplyActionPair

    module Kind =
        let name =
            function
            | Kind.Input -> "input"
            | Kind.Value -> "value"
            | Kind.Result -> "result"
            | Kind.Lifecycle -> "lifecycle"
            | Kind.EmptyLifecycle -> "empty lifecycle"
            | Kind.Model -> "model"
            | Kind.ModelAndInput -> "model & input"
            | Kind.AssocKey -> "assoc key"
            | Kind.AssocInput -> "assoc input"
            | Kind.AssocResults -> "assoc result map"
            | Kind.AssocLifecycles -> "assoc lifecycle map"
            | Kind.AssocInputs -> "assoc input map"
            | Kind.Path -> "path"
            | Kind.LifecycleApplyActionPair -> "lifecycle/apply-action pair"

        let color =
            function
            | Kind.Value -> Color.red50
            | Kind.Result -> Color.red100
            | Kind.AssocResults -> Color.red200
            | Kind.Path -> Color.red300
            | Kind.Input -> Color.amber100
            | Kind.AssocInputs -> Color.amber200
            | Kind.Lifecycle -> Color.blue100
            | Kind.AssocLifecycles -> Color.blue200
            | Kind.EmptyLifecycle -> Color.blue300
            | Kind.Model -> Color.emerald100
            | Kind.LifecycleApplyActionPair -> Color.emerald200
            | Kind.ModelAndInput -> Color.pink100
            | Kind.AssocKey -> Color.pink200
            | Kind.AssocInput -> Color.pink300

    // Memoization cache for annotate_packed
    let private annotatePackedCache = Dictionary<Kind, NodeCrate -> unit> ()

    let private annotatePackedInternal (kind : Kind) (incr : NodeCrate) =
        if not (annotatePackedCache.ContainsKey (kind)) then
            annotatePackedCache.[kind] <-
                fun incr ->
                    let label = [ "kind" ; Kind.name kind ]
                    let color = Kind.color kind
                    let attrs = Map.ofList [ ("style", "filled") ; ("fillcolor", color) ]
                    // Note: WoofWare.Incremental may not have exact equivalent for graphviz annotation
                    ()

        annotatePackedCache.[kind] incr

    type Counts =
        {
            mutable Input : int
            mutable Value : int
            mutable Result : int
            mutable Lifecycle : int
            mutable EmptyLifecycle : int
            mutable Model : int
            mutable ModelAndInput : int
            mutable AssocKey : int
            mutable AssocInput : int
            mutable AssocResults : int
            mutable AssocLifecycles : int
            mutable AssocInputs : int
            mutable Path : int
            mutable LifecycleApplyActionPair : int
        }

    module Counts =
        let private globalCounts =
            {
                Input = 0
                Value = 0
                Result = 0
                Lifecycle = 0
                EmptyLifecycle = 0
                Model = 0
                ModelAndInput = 0
                AssocKey = 0
                AssocInput = 0
                AssocResults = 0
                AssocLifecycles = 0
                AssocInputs = 0
                Path = 0
                LifecycleApplyActionPair = 0
            }

        let current () =
            {
                Input = globalCounts.Input
                Value = globalCounts.Value
                Result = globalCounts.Result
                Lifecycle = globalCounts.Lifecycle
                EmptyLifecycle = globalCounts.EmptyLifecycle
                Model = globalCounts.Model
                ModelAndInput = globalCounts.ModelAndInput
                AssocKey = globalCounts.AssocKey
                AssocInput = globalCounts.AssocInput
                AssocResults = globalCounts.AssocResults
                AssocLifecycles = globalCounts.AssocLifecycles
                AssocInputs = globalCounts.AssocInputs
                Path = globalCounts.Path
                LifecycleApplyActionPair = globalCounts.LifecycleApplyActionPair
            }

        let diff (before : Counts) (after : Counts) =
            {
                Input = after.Input - before.Input
                Value = after.Value - before.Value
                Result = after.Result - before.Result
                Lifecycle = after.Lifecycle - before.Lifecycle
                EmptyLifecycle = after.EmptyLifecycle - before.EmptyLifecycle
                Model = after.Model - before.Model
                ModelAndInput = after.ModelAndInput - before.ModelAndInput
                AssocKey = after.AssocKey - before.AssocKey
                AssocInput = after.AssocInput - before.AssocInput
                AssocResults = after.AssocResults - before.AssocResults
                AssocLifecycles = after.AssocLifecycles - before.AssocLifecycles
                AssocInputs = after.AssocInputs - before.AssocInputs
                Path = after.Path - before.Path
                LifecycleApplyActionPair = after.LifecycleApplyActionPair - before.LifecycleApplyActionPair
            }

        let private incr (kind : Kind) =
            match kind with
            | Kind.Input -> globalCounts.Input <- globalCounts.Input + 1
            | Kind.Value -> globalCounts.Value <- globalCounts.Value + 1
            | Kind.Result -> globalCounts.Result <- globalCounts.Result + 1
            | Kind.Lifecycle -> globalCounts.Lifecycle <- globalCounts.Lifecycle + 1
            | Kind.EmptyLifecycle -> globalCounts.EmptyLifecycle <- globalCounts.EmptyLifecycle + 1
            | Kind.Model -> globalCounts.Model <- globalCounts.Model + 1
            | Kind.ModelAndInput -> globalCounts.ModelAndInput <- globalCounts.ModelAndInput + 1
            | Kind.AssocKey -> globalCounts.AssocKey <- globalCounts.AssocKey + 1
            | Kind.AssocInput -> globalCounts.AssocInput <- globalCounts.AssocInput + 1
            | Kind.AssocResults -> globalCounts.AssocResults <- globalCounts.AssocResults + 1
            | Kind.AssocLifecycles -> globalCounts.AssocLifecycles <- globalCounts.AssocLifecycles + 1
            | Kind.AssocInputs -> globalCounts.AssocInputs <- globalCounts.AssocInputs + 1
            | Kind.Path -> globalCounts.Path <- globalCounts.Path + 1
            | Kind.LifecycleApplyActionPair ->
                globalCounts.LifecycleApplyActionPair <- globalCounts.LifecycleApplyActionPair + 1

        let internal incrInternal = incr

    let annotatePacked (kind : Kind) (incr : NodeCrate) =
        Counts.incrInternal kind

        if enabled then
            annotatePackedInternal kind incr

    let annotate (kind : Kind) (incr : 'a Node) =
        Counts.incrInternal kind

        if enabled then
            annotatePackedInternal kind (NodeCrate.make incr)

    let attributePacked (posOpt : string option) (t : NodeCrate) =
        match enabled, posOpt with
        | true, Some pos -> attributePackedInternal pos t
        | _ -> ()

    let attribute (posOpt : string option) (incr : 'a Node) =
        if enabled then
            attributePacked posOpt (NodeCrate.make incr)
