namespace WoofWare.Zoomies.Port

open TypeEquality


/// A collection of stats about the stabilization tracker
[<RequireQualifiedAccess>]
module StabilizationStats =

    type Stats =
        {
            mutable NumStabilize : int
            mutable NumDontStabilize : int
            mutable NumStabilizeCausedByVars : int
            mutable NumPrunesRun : int
            mutable NumBranchesPruned : int
        }

    let create () =
        {
            NumStabilize = 0
            NumDontStabilize = 0
            NumStabilizeCausedByVars = 0
            NumPrunesRun = 0
            NumBranchesPruned = 0
        }

    let incrStabilize (stats : Stats) =
        stats.NumStabilize <- stats.NumStabilize + 1

    let incrDontStabilize (stats : Stats) =
        stats.NumDontStabilize <- stats.NumDontStabilize + 1

    let incrStabilizeCausedByVars (stats : Stats) =
        stats.NumStabilizeCausedByVars <- stats.NumStabilizeCausedByVars + 1

    let incrPrunesRun (stats : Stats) =
        stats.NumPrunesRun <- stats.NumPrunesRun + 1

    let incrBranchesPruned (stats : Stats) =
        stats.NumBranchesPruned <- stats.NumBranchesPruned + 1

    let display (stats : Stats) =
        printfn "Stabilization Stats:"
        printfn "  Stabilizations before actions: %d" stats.NumStabilize
        printfn "  Stabilizations caused by var changes: %d" stats.NumStabilizeCausedByVars
        printfn "  Stabilizations skipped: %d" stats.NumDontStabilize
        printfn "  Prunes run: %d" stats.NumPrunesRun
        printfn "  Branches pruned: %d" stats.NumBranchesPruned

/// Generation tracking for stabilization
[<RequireQualifiedAccess>]
module Generation =

    type Generation = int

    let initial : Generation = 0
    let next (gen : Generation) : Generation = gen + 1

/// ActionTrie module containing the complex GADT structures
[<RequireQualifiedAccess>]
module ActionTrie =

    let maxSecondsBetweenPrunes = 15
    let numGenerationsToStale = 3 * 60 * maxSecondsBetweenPrunes

    /// WithGeneration wrapper for tracking generation information
    type WithGeneration<'inner> =
        {
            mutable Generation : Generation.Generation
            mutable Inner : 'inner
        }

    /// Helper for creating empty generation wrappers
    let withEmptyGeneration inner =
        {
            Generation = -1
            Inner = inner
        }

    /// Helper for creating current generation wrappers
    let withCurrentGeneration generation inner =
        {
            Generation = generation
            Inner = inner
        }

    type Node<'inner> =
        | Unexplored
        | Terminal of TerminalCrate<'inner>
        | Sub of SubCrate<'inner>
        | Wrap of WrapCrate<'inner>
        | ModelReset of ModelResetCrate<'inner>
        | Lazy of PackedCrate * Teq<'inner, Lazy'>
        | Assoc of AssocCrate<'inner>
        | AssocOn of AssocOnCrate<'inner>
        | Switch of SwitchData * Teq<'inner, Switch>

    and TerminalCrate<'inner> =
        abstract Apply : TerminalEval<'inner, 'ret> -> 'ret

    and TerminalEval<'inner, 'ret> =
        abstract Eval<'inner2> : Teq<'inner, 'inner2 Leaf> -> 'ret

    and AssocCrate<'inner> =
        abstract Apply : AssocEval<'inner, 'ret> -> 'ret

    and AssocEval<'inner, 'ret> =
        abstract Eval<'key, 'inner2> : AssocData<'inner2> * Teq<'inner, Assoc<'key, 'inner2>> -> 'ret

    and AssocOnCrate<'inner> =
        abstract Apply : AssocOnEval<'inner, 'ret> -> 'ret

    and AssocOnEval<'inner, 'ret> =
        abstract Eval<'ioKey, 'modelKey, 'inner2> :
            AssocOnData<'inner2> * Teq<'inner, AssocOn<'ioKey, 'modelKey, 'inner2>> -> 'ret

    and SubCrate<'inner> =
        abstract Apply : SubEval<'inner, 'ret> -> 'ret

    and SubEval<'inner, 'ret> =
        abstract Eval<'from, 'into> :
            from : 'from Node WithGeneration * into : 'into Node WithGeneration * Teq<'inner, Sub<'from, 'into>> -> 'ret

    and WrapCrate<'inner> =
        abstract Apply : WrapEval<'inner, 'ret> -> 'ret

    and WrapEval<'inner, 'ret> =
        abstract Eval<'inner2, 'outer> :
            outer : unit WithGeneration * inner : 'inner2 Node WithGeneration * Teq<'inner, Wrap<'inner2, 'outer>> ->
                'ret

    and ModelResetCrate<'inner> =
        abstract Apply : ModelResetEval<'inner, 'ret> -> 'ret

    and ModelResetEval<'inner, 'ret> =
        abstract Eval<'inner2> :
            outer : unit WithGeneration * inner : 'inner2 Node WithGeneration * Teq<'inner, ModelResetter<'inner2>> ->
                'ret

    /// Packed type using crate pattern for existential types - corresponds to OCaml "packed"
    and PackedEval<'ret> =
        abstract Eval<'inner> : Node<'inner> WithGeneration * ActionId<'inner> -> 'ret

    and PackedCrate =
        abstract Apply<'ret> : PackedEval<'ret> -> 'ret

    /// Supporting data structures for the Node GADT
    and AssocData<'inner> =
        {
            mutable ByKey : Map<Keyed.Keyed, Node<'inner> WithGeneration>
        }

    and AssocOnData<'inner> =
        {
            mutable ByIoKey : Map<Keyed.Keyed, Node<'inner> WithGeneration>
        }

    and SwitchData =
        {
            mutable ByBranch : Map<int, PackedCrate>
        }

    module PackedCrate =
        let create node actionId =
            { new PackedCrate with
                member _.Apply eval = eval.Eval (node, actionId)
            }

    module Node =
        let empty () : Node<'inner> WithGeneration = withEmptyGeneration Unexplored

        let with_current_generation currentGeneration inner =
            {
                Generation = currentGeneration
                Inner = inner
            }

        let with_empty_generation inner =
            {
                Generation = -1
                Inner = inner
            }

        let rec node_of_action<'a> (currentGeneration : Generation.Generation) (action : Action<'a>) : Node<'a> =
            match action with
            | LeafStatic leafStaticCrate ->
                leafStaticCrate.Apply
                    { new LeafStaticEval<_, _> with
                        member _.Eval (teq, _) =
                            Terminal
                                { new TerminalCrate<_> with
                                    member _.Apply eval = eval.Eval teq
                                }
                    }
            | LeafDynamic leafDynamicCrate ->
                leafDynamicCrate.Apply
                    { new LeafDynamicEval<_, _> with
                        member _.Eval (teq, _) =
                            Terminal
                                { new TerminalCrate<_> with
                                    member _.Apply eval = eval.Eval teq
                                }
                    }
            | SubFrom from ->
                from.Apply
                    { new SubFromEval<_, _> with
                        member _.Eval<'from, 'into>
                            (teq : Teq<'a, Sub<'from, 'into>>, from : Action<'from>)
                            : Node<'a>
                            =
                            let from : WithGeneration<Node<'from>> =
                                with_current_generation currentGeneration (node_of_action<'from> currentGeneration from)

                            Sub
                                { new SubCrate<_> with
                                    member _.Apply eval = eval.Eval (from, empty (), teq)
                                }

                    }
            | SubInto into ->
                into.Apply
                    { new SubIntoEval<_, _> with
                        member _.Eval<'from, 'into>
                            (teq : Teq<'a, Sub<'from, 'into>>, into : Action<'into>)
                            : Node<'a>
                            =
                            let into : WithGeneration<Node<'into>> =
                                with_current_generation currentGeneration (node_of_action<'into> currentGeneration into)

                            Sub
                                { new SubCrate<_> with
                                    member _.Apply eval = eval.Eval (empty (), into, teq)
                                }

                    }
            | WrapInner inner ->
                inner.Apply
                    { new WrapInnerEval<_, _> with
                        member _.Eval<'inner, 'outer>
                            (teq : Teq<'a, Wrap<'inner, 'outer>>, inner : Action<'inner>)
                            : Node<'a>
                            =
                            let inner : WithGeneration<Node<'inner>> =
                                with_current_generation
                                    currentGeneration
                                    (node_of_action<'inner> currentGeneration inner)

                            Wrap
                                { new WrapCrate<_> with
                                    member _.Apply eval =
                                        eval.Eval (withEmptyGeneration (), inner, teq)
                                }

                    }
            | WrapOuter outer ->
                outer.Apply
                    { new WrapOuterEval<_, _> with
                        member _.Eval (teq, _) =
                            Wrap
                                { new WrapCrate<_> with
                                    member _.Apply eval =
                                        eval.Eval (withCurrentGeneration currentGeneration (), empty (), teq)
                                }
                    }
            | ModelResetInner inner ->
                inner.Apply
                    { new ModelResetInnerEval<_, _> with
                        member _.Eval<'inner>
                            (teq : Teq<'a, ModelResetter<'inner>>, inner : Action<'inner>)
                            : Node<'a>
                            =
                            let inner : WithGeneration<Node<'inner>> =
                                with_current_generation
                                    currentGeneration
                                    (node_of_action<'inner> currentGeneration inner)

                            ModelReset
                                { new ModelResetCrate<_> with
                                    member _.Apply eval =
                                        eval.Eval (withEmptyGeneration (), inner, teq)
                                }

                    }
            | ModelResetOuter outer ->
                outer.Apply
                    { new ModelResetOuterEval<_, _> with
                        member _.Eval teq =
                            ModelReset
                                { new ModelResetCrate<_> with
                                    member _.Apply eval =
                                        eval.Eval (withCurrentGeneration currentGeneration (), empty (), teq)
                                }
                    }
            | Action.Switch switchCrate ->
                switchCrate.Apply
                    { new SwitchEval<_, _> with
                        member _.Eval<'inner>
                            (teq : Teq<'a, Switch>, branch : int, action : Action<'inner>, typeId : ActionId<'inner>)
                            =
                            let inner : WithGeneration<Node<'inner>> =
                                with_current_generation
                                    currentGeneration
                                    (node_of_action<'inner> currentGeneration action)

                            let packedInner =
                                { new PackedCrate with
                                    member _.Apply eval = eval.Eval (inner, typeId)
                                }

                            Switch (
                                {
                                    ByBranch = Map.ofList [ branch, packedInner ]
                                },
                                teq
                            )
                    }
            | Action.Lazy lazyCrate ->
                lazyCrate.Apply
                    { new LazyEval<_, _> with
                        member _.Eval<'inner>
                            (teq : Teq<'a, Lazy'>, action : Action<'inner>, typeId : ActionId<'inner>)
                            =
                            let inner : WithGeneration<Node<'inner>> =
                                with_current_generation
                                    currentGeneration
                                    (node_of_action<'inner> currentGeneration action)

                            let packedInner = PackedCrate.create inner typeId

                            Lazy (packedInner, teq)
                    }
            | Action.Assoc assocCrate ->
                assocCrate.Apply
                    { new ActionAssocEval<_, _> with
                        member _.Eval<'key, 'inner when 'key : comparison>
                            (teq : Teq<'a, Assoc<'key, 'inner>>, key, action, typeId)
                            =
                            let inner : WithGeneration<Node<'inner>> =
                                with_current_generation
                                    currentGeneration
                                    (node_of_action<'inner> currentGeneration action)

                            let keyedKey = Keyed.create key

                            Assoc
                                { new AssocCrate<_> with
                                    member _.Apply eval =
                                        eval.Eval (
                                            {
                                                ByKey = Map.ofList [ keyedKey, inner ]
                                            },
                                            teq
                                        )
                                }
                    }
            | Action.AssocOn assocOnCrate ->
                assocOnCrate.Apply
                    { new ActionAssocOnEval<_, _> with
                        member _.Eval<'ioKey, 'modelKey, 'inner when 'ioKey : comparison>
                            (
                                teq : Teq<'a, AssocOn<'ioKey, 'modelKey, 'inner>>,
                                ioKey : 'ioKey,
                                modelKey : 'modelKey,
                                action : Action<'inner>,
                                ioTypeId : TypeId<'ioKey>
                            )
                            =
                            let inner : WithGeneration<Node<'inner>> =
                                with_current_generation
                                    currentGeneration
                                    (node_of_action<'inner> currentGeneration action)

                            let keyedIoKey = Keyed.create ioKey

                            AssocOn
                                { new AssocOnCrate<_> with
                                    member _.Apply eval =
                                        eval.Eval (
                                            {
                                                ByIoKey = Map.ofList [ keyedIoKey, inner ]
                                            },
                                            teq
                                        )
                                }
                    }


    /// Traverser type for walking through ActionTrie nodes
    [<Interface>]
    type Traverser<'state> =

        abstract Unexplored : 'state -> Node<'stripped> WithGeneration -> Action<'stripped> -> 'state
        abstract DynamicLeaf : 'state -> 'state
        abstract StaticLeaf : 'state -> 'state

        abstract Sub :
            'state -> Node<'from> WithGeneration -> Node<'into> WithGeneration -> Action<Sub<'from, 'into>> -> 'state

        abstract Wrap :
            'state -> Node<'inner> WithGeneration -> unit WithGeneration -> Action<Wrap<'inner, 'outer>> -> 'state

        abstract ModelReset :
            'state -> Node<'inner> WithGeneration -> unit WithGeneration -> Action<ModelResetter<'inner>> -> 'state

        abstract Lazy : 'state -> Node<'stripped> WithGeneration -> Action<'stripped> -> 'state
        abstract Assoc : 'state -> AssocData<'inner> -> Keyed.Keyed -> Action<Assoc<'key, 'inner>> -> 'state

        abstract AssocOn :
            'state -> AssocOnData<'inner> -> Keyed.Keyed -> Action<AssocOn<'ioKey, 'modelKey, 'inner>> -> 'state

        abstract Switch : 'state -> SwitchData -> int -> Action<Switch> -> 'state

/// Simplified stabilization tracker implementation
type StabilizationTracker<'action> =
    {
        mutable Stats : StabilizationStats.Stats
        mutable CurrentGeneration : Generation.Generation
        mutable AmDebuggingTest : bool
    }

/// Global flag tracking whether incremental variables are dirty
module private IncrementalState =
    let mutable dirtyIncrementalVars = false

    let markIncrementalDirty () = dirtyIncrementalVars <- true

    let markIncrementalClean () = dirtyIncrementalVars <- false

    let isDirty () = dirtyIncrementalVars

[<RequireQualifiedAccess>]
module StabilizationTracker =

    /// Create an empty stabilization tracker
    let empty () : StabilizationTracker<'action> =
        {
            Stats = StabilizationStats.create ()
            CurrentGeneration = Generation.initial
            AmDebuggingTest = false
        }

    /// Insert an action into the tracker
    let insert (tracker : StabilizationTracker<'action>) (action : Action<'action>) : unit =
        // Update the action tracking state
        // In the full implementation, this would maintain a complex trie of action paths
        // For now, we track that an action has occurred and increment generation
        tracker.CurrentGeneration <- Generation.next tracker.CurrentGeneration

    /// Check if stabilization is required before applying an action
    let requiresStabilization (tracker : StabilizationTracker<'action>) (action : Action<'action>) : bool =
        // Simplified logic - in the full version this would check complex conditions
        let requiresStabilization = IncrementalState.isDirty ()

        if requiresStabilization then
            StabilizationStats.incrStabilize tracker.Stats

            if IncrementalState.isDirty () then
                StabilizationStats.incrStabilizeCausedByVars tracker.Stats
        else
            StabilizationStats.incrDontStabilize tracker.Stats

        if tracker.AmDebuggingTest then
            if requiresStabilization then
                printfn "stabilized"
            else
                printfn "skipped stabilization"

        requiresStabilization

    /// Mark that incremental variables are dirty and require stabilization
    let markIncrementalDirty () : unit =
        IncrementalState.markIncrementalDirty ()

    /// Mark that stabilization has occurred
    let markStabilization (tracker : StabilizationTracker<'action>) : unit =
        IncrementalState.markIncrementalClean ()
        tracker.CurrentGeneration <- Generation.next tracker.CurrentGeneration

    /// Testing utilities
    [<RequireQualifiedAccess>]
    module ForTesting =

        let startDebugging (tracker : StabilizationTracker<'action>) : unit = tracker.AmDebuggingTest <- true

        let numGenerationsForPruning : int = 2700 // 3 * 60 * 15 from original

        let displayStats (tracker : StabilizationTracker<'action>) : unit =
            StabilizationStats.display tracker.Stats
