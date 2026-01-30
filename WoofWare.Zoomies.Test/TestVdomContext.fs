namespace WoofWare.Zoomies.Test

open System
open NUnit.Framework
open FsUnitTyped
open WoofWare.Incremental
open WoofWare.Zoomies

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestVdomContext =

    let bounds =
        {
            TopLeftX = 0
            TopLeftY = 0
            Width = 80
            Height = 24
        }

    /// Create a VdomContext from a MockTimer, returning both the context and an advance function.
    /// The advance function updates both the MockTimer's clock and the VdomContext's stabilization time.
    let empty (clock : MockTimer) : VdomContext<unit> * (TimeSpan -> DateTime) = MockTime.makeVdomContextFromTimer clock

    /// Helper to observe a bool Node from wasRecentlyActivated.
    let observeWasRecentlyActivated (key : NodeKey) (ctx : VdomContext<unit>) : bool =
        let incr = VdomContext.unsafeIncr ctx
        let node = VdomContext.wasRecentlyActivated key ctx
        let observer = incr.Observe node
        incr.Stabilize ()
        Observer.value observer

    [<Test>]
    let ``pruneExpiredActivations removes only expired entries`` () =
        let clock = MockTime.make ()
        let ctx, advance = empty clock

        // Record activations at different times
        let key1 = NodeKey.make "key1"
        let key2 = NodeKey.make "key2"
        let key3 = NodeKey.make "key3"
        let key4 = NodeKey.make "key4"

        // Record first activation
        let now1 = clock.CurrentTime ()
        VdomContext.recordActivation now1 key1 ctx

        // Advance time by 100ms and record second activation
        let now2 = advance (TimeSpan.FromMilliseconds 100.0)
        VdomContext.recordActivation now2 key2 ctx

        // Advance time by 200ms and record third activation
        let now3 = advance (TimeSpan.FromMilliseconds 200.0)
        VdomContext.recordActivation now3 key3 ctx

        // Advance time by 150ms and record fourth activation
        let now4 = advance (TimeSpan.FromMilliseconds 150.0)
        VdomContext.recordActivation now4 key4 ctx

        // At this point:
        // - key1 was activated 450ms ago
        // - key2 was activated 350ms ago
        // - key3 was activated 150ms ago
        // - key4 was activated 0ms ago (just now)

        // All should still be considered recently activated
        observeWasRecentlyActivated key1 ctx |> shouldEqual true
        observeWasRecentlyActivated key2 ctx |> shouldEqual true
        observeWasRecentlyActivated key3 ctx |> shouldEqual true
        observeWasRecentlyActivated key4 ctx |> shouldEqual true

        // Advance time by 100ms more
        // Now:
        // - key1 was activated 550ms ago (expired)
        // - key2 was activated 450ms ago (not expired)
        // - key3 was activated 250ms ago (not expired)
        // - key4 was activated 100ms ago (not expired)
        let now5 = advance (TimeSpan.FromMilliseconds 100.0)

        // Mark clean before pruning so we can verify pruning actually removed something
        VdomContext.markClean ctx

        // Prune expired activations - this should remove key1 only
        VdomContext.pruneExpiredActivations now5 ctx

        // Verify pruning actually removed an entry (not just that time elapsed)
        VdomContext.isDirty ctx |> shouldEqual true

        // Verify key1 is gone and others remain
        observeWasRecentlyActivated key1 ctx |> shouldEqual false
        observeWasRecentlyActivated key2 ctx |> shouldEqual true
        observeWasRecentlyActivated key3 ctx |> shouldEqual true
        observeWasRecentlyActivated key4 ctx |> shouldEqual true

        // Advance time by another 100ms
        // Now:
        // - key2 was activated 550ms ago (expired)
        // - key3 was activated 350ms ago (not expired)
        // - key4 was activated 200ms ago (not expired)
        let now6 = advance (TimeSpan.FromMilliseconds 100.0)

        // Mark clean before pruning so we can verify pruning actually removed something
        VdomContext.markClean ctx

        VdomContext.pruneExpiredActivations now6 ctx

        // Verify pruning actually removed an entry (not just that time elapsed)
        VdomContext.isDirty ctx |> shouldEqual true

        // Verify key2 is now gone too
        observeWasRecentlyActivated key1 ctx |> shouldEqual false
        observeWasRecentlyActivated key2 ctx |> shouldEqual false
        observeWasRecentlyActivated key3 ctx |> shouldEqual true
        observeWasRecentlyActivated key4 ctx |> shouldEqual true

    [<Test>]
    let ``pruneExpiredActivations handles removing multiple entries in one pass`` () =
        let clock = MockTime.make ()
        let ctx, advance = empty clock

        // Create many activations at the same time
        let keys = [ for i in 1..10 -> NodeKey.make $"key{i}" ]

        let now1 = clock.CurrentTime ()

        for key in keys do
            VdomContext.recordActivation now1 key ctx

        // All should be recently activated
        for key in keys do
            observeWasRecentlyActivated key ctx |> shouldEqual true

        // Advance time past expiration threshold
        let now2 = advance (TimeSpan.FromMilliseconds 600.0)

        // Mark clean before pruning so we can verify pruning actually removed entries
        VdomContext.markClean ctx

        // This should remove all entries without throwing
        // (demonstrating that Dictionary.Remove during enumeration is safe in .NET Core 3.0+)
        VdomContext.pruneExpiredActivations now2 ctx

        // Verify pruning actually removed entries (not just that time elapsed)
        VdomContext.isDirty ctx |> shouldEqual true

        // All should now be expired
        for key in keys do
            observeWasRecentlyActivated key ctx |> shouldEqual false

    [<Test>]
    let ``pruneExpiredActivations marks context dirty only when removals occur`` () =
        let clock = MockTime.make ()
        let ctx, advance = empty clock

        let key1 = NodeKey.make "key1"

        // Record an activation
        let now1 = clock.CurrentTime ()
        VdomContext.recordActivation now1 key1 ctx

        // Mark clean
        VdomContext.markClean ctx
        VdomContext.isDirty ctx |> shouldEqual false

        // Prune when nothing has expired - should not mark dirty
        VdomContext.pruneExpiredActivations now1 ctx
        VdomContext.isDirty ctx |> shouldEqual false

        // Advance time past expiration
        let now2 = advance (TimeSpan.FromMilliseconds 600.0)

        // Prune when something has expired - should mark dirty
        VdomContext.pruneExpiredActivations now2 ctx
        VdomContext.isDirty ctx |> shouldEqual true

        // Mark clean again
        VdomContext.markClean ctx

        // Prune when nothing remains - should not mark dirty
        VdomContext.pruneExpiredActivations now2 ctx
        VdomContext.isDirty ctx |> shouldEqual false
