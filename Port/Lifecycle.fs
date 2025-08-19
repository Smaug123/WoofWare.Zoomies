namespace WoofWare.Zoomies.Port

open WoofWare.Incremental

// Create incremental computation instance
module private IncrementalInstance =
    let I : Incremental = Incremental.make ()

// Component lifecycle management - handles activation, deactivation, and display events
module Lifecycle =

    // For now, represent effects as simple unit functions
    // This will need to be integrated with the actual effect system later
    type Effect = unit -> unit

    type Lifecycle =
        {
            OnActivate : Effect option
            OnDeactivate : Effect option
            AfterDisplay : Effect option
        }

    [<RequireQualifiedAccess>]
    module Collection =
        type Collection = Map<Path.Path, Lifecycle>

        let empty : Collection = Map.empty

        let hasAfterDisplay (collection : Collection) : bool =
            Map.exists (fun _path lifecycle -> Option.isSome lifecycle.AfterDisplay) collection

        let private maybeCons (head : 'a option) (tail : 'a list) : 'a list =
            match head with
            | Some a -> a :: tail
            | None -> tail

        // Compute the difference between old and new lifecycle collections
        // Returns effects to run in the proper order: deactivations, activations, after_displays
        let diff (old : Collection) (new_ : Collection) : Effect list =
            // Collect after_display effects from new collection
            let afterDisplays = new_ |> Map.toList |> List.map snd |> List.choose _.AfterDisplay

            // Collect activations and deactivations by comparing old and new
            let activations, deactivations =
                let processPath path oldLifecycle newLifecycle (activations, deactivations) =
                    match oldLifecycle, newLifecycle with
                    | Some old, None ->
                        // Component was removed - deactivate
                        (activations, maybeCons old.OnDeactivate deactivations)
                    | None, Some new_ ->
                        // Component was added - activate
                        (maybeCons new_.OnActivate activations, deactivations)
                    | Some old, Some new_ when not (obj.ReferenceEquals (old, new_)) ->
                        // Component changed - deactivate old, activate new
                        let deactivations = maybeCons old.OnDeactivate deactivations
                        let activations = maybeCons new_.OnActivate activations
                        (activations, deactivations)
                    | _ ->
                        // No change
                        (activations, deactivations)

                let allPaths = Set.union (Map.keys old |> Set.ofSeq) (Map.keys new_ |> Set.ofSeq)

                allPaths
                |> Set.fold
                    (fun (activations, deactivations) path ->
                        let oldLifecycle = Map.tryFind path old
                        let newLifecycle = Map.tryFind path new_
                        processPath path oldLifecycle newLifecycle (activations, deactivations)
                    )
                    ([], [])

            // Return effects in proper order: deactivations first, then activations, then after_displays
            let deactivationEffects = List.rev deactivations
            let activationEffects = List.rev activations

            deactivationEffects @ activationEffects @ afterDisplays

        // Merge two disjoint lifecycle collections (for incremental computation)
        let merge (a : Collection Node) (b : Collection Node) : Collection Node =
            IncrementalInstance.I.Map2
                (fun collectionA collectionB ->
                    // Simple merge - assumes collections are disjoint
                    // In a real implementation, this would need proper conflict resolution
                    Map.fold (fun acc key value -> Map.add key value acc) collectionA collectionB
                )
                a
                b
