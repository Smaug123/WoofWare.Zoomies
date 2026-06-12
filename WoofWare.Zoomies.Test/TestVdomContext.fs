namespace WoofWare.Zoomies.Test

open System
open FsCheck
open FsCheck.FSharp
open NUnit.Framework
open FsUnitTyped
open WoofWare.Incremental
open WoofWare.TimingWheel
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

    /// Helper to observe a bool Node from wasRecentlyActivated, releasing the observer afterwards.
    let observeWasRecentlyActivated (key : NodeKey) (ctx : VdomContext<unit>) : bool =
        let incr = VdomContext.unsafeIncr ctx
        let node = VdomContext.wasRecentlyActivated key ctx
        let observer = incr.Observe node
        incr.Stabilize ()
        let value = Observer.value observer
        Observer.disallowFutureUse observer
        incr.Stabilize ()
        value

    [<Test>]
    let ``activations expire as the clock advances, with no explicit pruning`` () =
        let clock = MockTime.make ()
        let ctx, advance = empty clock

        let key1 = NodeKey.make "key1"
        let key2 = NodeKey.make "key2"
        let key3 = NodeKey.make "key3"
        let key4 = NodeKey.make "key4"

        let now1 = clock.CurrentTime ()
        VdomContext.recordActivation now1 key1 ctx

        let now2 = advance (TimeSpan.FromMilliseconds 100.0)
        VdomContext.recordActivation now2 key2 ctx

        let now3 = advance (TimeSpan.FromMilliseconds 200.0)
        VdomContext.recordActivation now3 key3 ctx

        let now4 = advance (TimeSpan.FromMilliseconds 150.0)
        VdomContext.recordActivation now4 key4 ctx

        // At this point:
        // - key1 was activated 450ms ago
        // - key2 was activated 350ms ago
        // - key3 was activated 150ms ago
        // - key4 was activated 0ms ago (just now)

        observeWasRecentlyActivated key1 ctx |> shouldEqual true
        observeWasRecentlyActivated key2 ctx |> shouldEqual true
        observeWasRecentlyActivated key3 ctx |> shouldEqual true
        observeWasRecentlyActivated key4 ctx |> shouldEqual true

        // Advance time by 100ms more: key1 is now 550ms old (expired), the rest are not.
        advance (TimeSpan.FromMilliseconds 100.0) |> ignore<DateTime>

        observeWasRecentlyActivated key1 ctx |> shouldEqual false
        observeWasRecentlyActivated key2 ctx |> shouldEqual true
        observeWasRecentlyActivated key3 ctx |> shouldEqual true
        observeWasRecentlyActivated key4 ctx |> shouldEqual true

        // Another 100ms: key2 expires too.
        advance (TimeSpan.FromMilliseconds 100.0) |> ignore<DateTime>

        observeWasRecentlyActivated key1 ctx |> shouldEqual false
        observeWasRecentlyActivated key2 ctx |> shouldEqual false
        observeWasRecentlyActivated key3 ctx |> shouldEqual true
        observeWasRecentlyActivated key4 ctx |> shouldEqual true

    [<Test>]
    let ``re-activating a key restarts its window`` () =
        let clock = MockTime.make ()
        let ctx, advance = empty clock

        let key = NodeKey.make "key"

        VdomContext.recordActivation (clock.CurrentTime ()) key ctx

        let now2 = advance (TimeSpan.FromMilliseconds 400.0)
        observeWasRecentlyActivated key ctx |> shouldEqual true

        // Re-activate at 400ms; the window restarts.
        VdomContext.recordActivation now2 key ctx

        // 400ms later the original window would have closed, but the new one is open.
        advance (TimeSpan.FromMilliseconds 400.0) |> ignore<DateTime>
        observeWasRecentlyActivated key ctx |> shouldEqual true

        // 200ms more closes the restarted window.
        advance (TimeSpan.FromMilliseconds 200.0) |> ignore<DateTime>
        observeWasRecentlyActivated key ctx |> shouldEqual false

    [<Test>]
    let ``clearActivation removes the activation immediately`` () =
        let clock = MockTime.make ()
        let ctx, _advance = empty clock

        let key = NodeKey.make "key"

        VdomContext.recordActivation (clock.CurrentTime ()) key ctx
        observeWasRecentlyActivated key ctx |> shouldEqual true

        VdomContext.clearActivation key ctx
        observeWasRecentlyActivated key ctx |> shouldEqual false

    [<Test>]
    let ``recordActivation schedules a wake-up alarm for the expiry`` () =
        let clock = MockTime.make ()
        let ctx, _advance = empty clock
        let incr = VdomContext.unsafeIncr ctx
        let zoomiesClock = VdomContext.clock ctx

        let key = NodeKey.make "key"
        let now = clock.CurrentTime ()

        VdomContext.recordActivation now key ctx

        // The alarm only exists while the node is observed (a necessary node).
        let node = VdomContext.wasRecentlyActivated key ctx
        let observer = incr.Observe node
        incr.Stabilize ()
        Observer.value observer |> shouldEqual true

        let expiryNs =
            TimeNs.add (TimeConversion.dateTimeToNs now) VdomContextConstants.recentActivationTimeout

        match incr.Clock.NextAlarmFiresAt zoomiesClock with
        | ValueNone -> failwith "expected an alarm to be scheduled for the activation expiry"
        | ValueSome alarmAt ->
            // The alarm must not be later than the expiry plus one alarm-precision tick.
            let precision = TimeNs.Span.toInt64Ns (incr.Clock.AlarmPrecision zoomiesClock)

            (TimeNs.toInt64NsSinceEpoch alarmAt
             <= TimeNs.toInt64NsSinceEpoch expiryNs + precision)
            |> shouldEqual true

        Observer.disallowFutureUse observer
        incr.Stabilize ()

    [<Test>]
    let ``expired activations leave no alarms behind`` () =
        let clock = MockTime.make ()
        let ctx, advance = empty clock
        let incr = VdomContext.unsafeIncr ctx
        let zoomiesClock = VdomContext.clock ctx

        let key = NodeKey.make "key"

        VdomContext.recordActivation (clock.CurrentTime ()) key ctx

        let node = VdomContext.wasRecentlyActivated key ctx
        let observer = incr.Observe node
        incr.Stabilize ()

        // Expire the activation, then query once more (which re-records nothing): the
        // clock should be quiet again.
        advance (TimeSpan.FromMilliseconds 600.0) |> ignore<DateTime>
        Observer.value observer |> shouldEqual false

        Observer.disallowFutureUse observer
        incr.Stabilize ()

        incr.Clock.NextAlarmFiresAt zoomiesClock |> shouldEqual ValueNone

    // ============================================================
    // Model-based property: the activation mechanism agrees with the
    // reference predicate "last activation was strictly less than
    // 500ms ago".
    // ============================================================

    /// Operations for the model-based test.
    type private ActivationOp =
        /// Advance the mock clock by this many milliseconds.
        | Advance of ms : int
        /// Record an activation for the key with this index.
        | Activate of keyIndex : int
        /// Clear the activation for the key with this index.
        | Clear of keyIndex : int

    [<Test>]
    let ``wasRecentlyActivated agrees with the reference model under arbitrary interleavings`` () =
        let keyCount = 3

        let opGen =
            Gen.frequency
                [
                    // Advances skew small so that several ops land inside one window, but
                    // include jumps well past the 500ms timeout.
                    3, Gen.choose (0, 250) |> Gen.map Advance
                    1, Gen.choose (251, 1200) |> Gen.map Advance
                    3, Gen.choose (0, keyCount - 1) |> Gen.map Activate
                    1, Gen.choose (0, keyCount - 1) |> Gen.map Clear
                ]

        let arb = Arb.fromGen (Gen.listOf opGen)

        let prop (ops : ActivationOp list) =
            let clock = MockTime.make ()
            let ctx, advance = empty clock
            let incr = VdomContext.unsafeIncr ctx

            let keys = Array.init keyCount (fun i -> NodeKey.make $"key{i}")

            // Persistent observers, as a real app would hold them via its vdom.
            let observers =
                keys
                |> Array.map (fun key -> incr.Observe (VdomContext.wasRecentlyActivated key ctx))

            incr.Stabilize ()

            // Reference model: last activation time per key.
            let model : DateTime option[] = Array.create keyCount None

            for op in ops do
                match op with
                | Advance ms -> advance (TimeSpan.FromMilliseconds (float ms)) |> ignore<DateTime>
                | Activate i ->
                    let now = clock.CurrentTime ()
                    VdomContext.recordActivation now keys.[i] ctx
                    model.[i] <- Some now
                    incr.Stabilize ()
                | Clear i ->
                    VdomContext.clearActivation keys.[i] ctx
                    model.[i] <- None
                    incr.Stabilize ()

                let now = clock.CurrentTime ()

                for i in 0 .. keyCount - 1 do
                    let expected =
                        match model.[i] with
                        | None -> false
                        | Some t -> (now - t).TotalMilliseconds < VdomContextConstants.RECENT_ACTIVATION_TIMEOUT_MS

                    Observer.value observers.[i] |> shouldEqual expected

        Check.One (propConfig, Prop.forAll arb prop)
