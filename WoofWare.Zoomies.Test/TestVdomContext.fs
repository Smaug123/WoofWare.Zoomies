namespace WoofWare.Zoomies.Test

open System
open NUnit.Framework
open FsUnitTyped
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

    [<Test>]
    let ``pruneExpiredActivations removes only expired entries`` () =
        let clock = MockTime.make ()
        let ctx = VdomContext.empty<unit> clock.GetUtcNow bounds

        // Record activations at different times
        let key1 = NodeKey.make "key1"
        let key2 = NodeKey.make "key2"
        let key3 = NodeKey.make "key3"
        let key4 = NodeKey.make "key4"

        // Record first activation
        VdomContext.recordActivation key1 ctx
        let time1 = clock.GetUtcNow ()

        // Advance time by 100ms and record second activation
        clock.Advance (TimeSpan.FromMilliseconds 100.0) |> ignore<DateTime>
        VdomContext.recordActivation key2 ctx

        // Advance time by 200ms and record third activation
        clock.Advance (TimeSpan.FromMilliseconds 200.0) |> ignore<DateTime>
        VdomContext.recordActivation key3 ctx

        // Advance time by 150ms and record fourth activation
        clock.Advance (TimeSpan.FromMilliseconds 150.0) |> ignore<DateTime>
        VdomContext.recordActivation key4 ctx

        // At this point:
        // - key1 was activated 450ms ago
        // - key2 was activated 350ms ago
        // - key3 was activated 150ms ago
        // - key4 was activated 0ms ago (just now)

        // All should still be considered recently activated
        VdomContext.wasRecentlyActivated key1 ctx |> shouldEqual true
        VdomContext.wasRecentlyActivated key2 ctx |> shouldEqual true
        VdomContext.wasRecentlyActivated key3 ctx |> shouldEqual true
        VdomContext.wasRecentlyActivated key4 ctx |> shouldEqual true

        // Advance time by 100ms more
        // Now:
        // - key1 was activated 550ms ago (expired)
        // - key2 was activated 450ms ago (not expired)
        // - key3 was activated 250ms ago (not expired)
        // - key4 was activated 100ms ago (not expired)
        clock.Advance (TimeSpan.FromMilliseconds 100.0) |> ignore<DateTime>

        // Mark clean before pruning so we can verify pruning actually removed something
        VdomContext.markClean ctx

        // Prune expired activations - this should remove key1 only
        VdomContext.pruneExpiredActivations ctx

        // Verify pruning actually removed an entry (not just that time elapsed)
        VdomContext.isDirty ctx |> shouldEqual true

        // Verify key1 is gone and others remain
        VdomContext.wasRecentlyActivated key1 ctx |> shouldEqual false
        VdomContext.wasRecentlyActivated key2 ctx |> shouldEqual true
        VdomContext.wasRecentlyActivated key3 ctx |> shouldEqual true
        VdomContext.wasRecentlyActivated key4 ctx |> shouldEqual true

        // Advance time by another 100ms
        // Now:
        // - key2 was activated 550ms ago (expired)
        // - key3 was activated 350ms ago (not expired)
        // - key4 was activated 200ms ago (not expired)
        clock.Advance (TimeSpan.FromMilliseconds 100.0) |> ignore<DateTime>

        // Mark clean before pruning so we can verify pruning actually removed something
        VdomContext.markClean ctx

        VdomContext.pruneExpiredActivations ctx

        // Verify pruning actually removed an entry (not just that time elapsed)
        VdomContext.isDirty ctx |> shouldEqual true

        // Verify key2 is now gone too
        VdomContext.wasRecentlyActivated key1 ctx |> shouldEqual false
        VdomContext.wasRecentlyActivated key2 ctx |> shouldEqual false
        VdomContext.wasRecentlyActivated key3 ctx |> shouldEqual true
        VdomContext.wasRecentlyActivated key4 ctx |> shouldEqual true

    [<Test>]
    let ``pruneExpiredActivations handles removing multiple entries in one pass`` () =
        let clock = MockTime.make ()
        let ctx = VdomContext.empty<unit> clock.GetUtcNow bounds

        // Create many activations at the same time
        let keys = [ for i in 1..10 -> NodeKey.make $"key{i}" ]

        for key in keys do
            VdomContext.recordActivation key ctx

        // All should be recently activated
        for key in keys do
            VdomContext.wasRecentlyActivated key ctx |> shouldEqual true

        // Advance time past expiration threshold
        clock.Advance (TimeSpan.FromMilliseconds 600.0) |> ignore<DateTime>

        // Mark clean before pruning so we can verify pruning actually removed entries
        VdomContext.markClean ctx

        // This should remove all entries without throwing
        // (demonstrating that Dictionary.Remove during enumeration is safe in .NET Core 3.0+)
        VdomContext.pruneExpiredActivations ctx

        // Verify pruning actually removed entries (not just that time elapsed)
        VdomContext.isDirty ctx |> shouldEqual true

        // All should now be expired
        for key in keys do
            VdomContext.wasRecentlyActivated key ctx |> shouldEqual false

    [<Test>]
    let ``pruneExpiredActivations marks context dirty only when removals occur`` () =
        let clock = MockTime.make ()
        let ctx = VdomContext.empty<unit> clock.GetUtcNow bounds

        let key1 = NodeKey.make "key1"

        // Record an activation
        VdomContext.recordActivation key1 ctx

        // Mark clean
        VdomContext.markClean ctx
        VdomContext.isDirty ctx |> shouldEqual false

        // Prune when nothing has expired - should not mark dirty
        VdomContext.pruneExpiredActivations ctx
        VdomContext.isDirty ctx |> shouldEqual false

        // Advance time past expiration
        clock.Advance (TimeSpan.FromMilliseconds 600.0) |> ignore<DateTime>

        // Prune when something has expired - should mark dirty
        VdomContext.pruneExpiredActivations ctx
        VdomContext.isDirty ctx |> shouldEqual true

        // Mark clean again
        VdomContext.markClean ctx

        // Prune when nothing remains - should not mark dirty
        VdomContext.pruneExpiredActivations ctx
        VdomContext.isDirty ctx |> shouldEqual false
