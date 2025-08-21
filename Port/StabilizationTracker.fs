namespace WoofWare.Zoomies.Port

/// A collection of stats about the stabilization tracker
[<RequireQualifiedAccess>]
module StabilizationStats =
    
    type Stats = {
        mutable NumStabilize : int
        mutable NumDontStabilize : int  
        mutable NumStabilizeCausedByVars : int
        mutable NumPrunesRun : int
        mutable NumBranchesPruned : int
    }
    
    let create () = {
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

/// Simplified stabilization tracker implementation
type StabilizationTracker<'action> = {
    mutable Stats : StabilizationStats.Stats
    mutable CurrentGeneration : Generation.Generation
    mutable AmDebuggingTest : bool
}

/// Global flag tracking whether incremental variables are dirty
module private IncrementalState =
    let mutable dirtyIncrementalVars = false
    
    let markIncrementalDirty () = 
        dirtyIncrementalVars <- true
        
    let markIncrementalClean () = 
        dirtyIncrementalVars <- false
        
    let isDirty () = dirtyIncrementalVars

[<RequireQualifiedAccess>]
module StabilizationTracker =
    
    /// Create an empty stabilization tracker
    let empty () : StabilizationTracker<'action> = {
        Stats = StabilizationStats.create ()
        CurrentGeneration = Generation.initial
        AmDebuggingTest = false
    }
    
    /// Insert an action into the tracker
    let insert (tracker : StabilizationTracker<'action>) (action : Action<'action>) : unit =
        // Simplified implementation - in the full version this would update a complex trie structure
        // For now, just increment generation to indicate an action occurred
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
        
        let startDebugging (tracker : StabilizationTracker<'action>) : unit =
            tracker.AmDebuggingTest <- true
            
        let numGenerationsForPruning : int = 2700 // 3 * 60 * 15 from original
        
        let displayStats (tracker : StabilizationTracker<'action>) : unit =
            StabilizationStats.display tracker.Stats