module WoofWare.PlayFetch.Tests.TimingWheel

open System
open Expecto
open WoofWare.PlayFetch

// Test helpers
let epochNs = TimeNs.epoch
let gibiNanos (float : float) =
    let gibi = 2. ** 30.
    int64 (float * gibi) |> TimeSpan.FromTicks

let createConfig extendToMaxNumBits levelBits alarmPrecision () =
    let alarmPrecision = AlarmPrecision.ofSpanFloorPow2Ns alarmPrecision
    let levelBits =
        levelBits
        |> Option.map (fun bits ->
            LevelBits.createThrowing extendToMaxNumBits bits)
    Config.create levelBits alarmPrecision ()

let createUnit extendToMaxNumBits levelBits start alarmPrecision () =
    let config = createConfig extendToMaxNumBits levelBits alarmPrecision ()
    TimingWheel.create config start

let advanceClockToIntervalNum t toNum handleFired =
    let toTime = TimingWheel.intervalNumStart t toNum
    TimingWheel.advanceClock t toTime handleFired

[<Tests>]
let tests =
    testList "TimingWheel" [
        testList "AlarmPrecision" [
            testCase "constants" <| fun () ->
                let checkSpan precision expectedNs =
                    let actual = AlarmPrecision.toSpan precision |> TimeNs.Span.toInt64Ns
                    Expect.equal actual expectedNs "Alarm precision span should match"

                checkSpan AlarmPrecision.aboutOneDay 70_368_744_177_664L
                checkSpan AlarmPrecision.aboutOneSecond 1_073_741_824L
                checkSpan AlarmPrecision.aboutOneMicrosecond 1_024L
                checkSpan AlarmPrecision.aboutOneMillisecond 1_048_576L
                checkSpan AlarmPrecision.oneNanosecond 1L

            testCase "div" <| fun () ->
                let results =
                    [-3..3]
                    |> List.map (fun pow2 ->
                        AlarmPrecision.div AlarmPrecision.aboutOneSecond pow2
                        |> AlarmPrecision.toSpan
                        |> TimeNs.Span.toInt64Ns)

                let expected = [
                    8_589_934_592L
                    4_294_967_296L
                    2_147_483_648L
                    1_073_741_824L
                    536_870_912L
                    268_435_456L
                    134_217_728L
                ]

                Expect.equal results expected "div results should match"

            testCase "mul" <| fun () ->
                let results =
                    [-3..3]
                    |> List.map (fun pow2 ->
                        AlarmPrecision.mul AlarmPrecision.aboutOneSecond pow2
                        |> AlarmPrecision.toSpan
                        |> TimeNs.Span.toInt64Ns)

                let expected = [
                    134_217_728L
                    268_435_456L
                    536_870_912L
                    1_073_741_824L
                    2_147_483_648L
                    4_294_967_296L
                    8_589_934_592L
                ]

                Expect.equal results expected "mul results should match"

            testCase "ofSpanFloorPow2Ns" <| fun () ->
                let precisions = [
                    AlarmPrecision.aboutOneDay
                    AlarmPrecision.aboutOneSecond
                    AlarmPrecision.aboutOneMillisecond
                    AlarmPrecision.aboutOneMicrosecond
                    AlarmPrecision.oneNanosecond
                ]

                for p in precisions do
                    let span = AlarmPrecision.toSpan p
                    let roundTrip = AlarmPrecision.ofSpanFloorPow2Ns span
                    Expect.equal p roundTrip "Round trip should preserve precision"
        ]

        testList "LevelBits" [
            testCase "max num bits" <| fun () ->
                Expect.equal LevelBits.maxNumBits 62 "Max num bits should be 62"

            testCase "invalid level bits" <| fun () ->
                let testInvalid bits expectedMsg =
                    Expect.throwsT<Exception>
                        (fun () -> LevelBits.createThrowing bits |> ignore)
                        expectedMsg

                testInvalid [] "Empty list should fail"
                testInvalid [0] "Zero bits should fail"
                testInvalid [-1] "Negative bits should fail"
                testInvalid [2; 0; 1] "Zero in middle should fail"
                testInvalid [LevelBits.maxNumBits + 1] "Too many bits should fail"

            testCase "num bits" <| fun () ->
                let test bits expected =
                    let levelBits = LevelBits.createThrowing bits
                    Expect.equal (LevelBits.numBits levelBits) expected "Num bits should match"

                test [1] 1
                test [1; 1] 2
                test [1; 2; 3] 6
        ]

        testList "Config" [
            testCase "create with negative alarm precision" <| fun () ->
                Expect.throwsT<Exception>
                    (fun () -> createConfig (gibiNanos -1.) () |> ignore)
                    "Negative alarm precision should fail"

            testCase "create with zero alarm precision" <| fun () ->
                Expect.throwsT<Exception>
                    (fun () -> createConfig (gibiNanos 0.) () |> ignore)
                    "Zero alarm precision should fail"

            testCase "microsecond precision" <| fun () ->
                let config = Config.microsecondPrecision()
                let precision = Config.alarmPrecision config |> TimeNs.Span.toInt64Ns
                Expect.equal precision 1024L "Microsecond precision should be 1024ns"

                let durations = Config.durations config |> List.map TimeNs.Span.toInt64Ns
                let expectedDurations = [
                    1_048_576L           // ~1ms
                    1_073_741_824L       // ~1s
                    68_719_476_736L      // ~1m
                    4_398_046_511_104L   // ~1h
                    140_737_488_355_328L // ~1d
                ]
                Expect.equal durations expectedDurations "Durations should match"
        ]

        testList "TimingWheel creation and basic operations" [
            testCase "empty timing wheel" <| fun () ->
                let t = createUnit ()
                Expect.isTrue (TimingWheel.isEmpty t) "New timing wheel should be empty"
                Expect.equal (TimingWheel.length t) 0 "New timing wheel should have length 0"

            testCase "add and remove" <| fun () ->
                let t = createUnit () [1]
                let a1 = TimingWheel.addAtIntervalNum t IntervalNum.zero ()
                let a2 = TimingWheel.addAtIntervalNum t IntervalNum.zero ()

                Expect.equal (TimingWheel.length t) 2 "Should have 2 alarms"
                Expect.isFalse (TimingWheel.isEmpty t) "Should not be empty"
                Expect.isTrue (TimingWheel.mem t a1) "Should contain a1"
                Expect.isTrue (TimingWheel.mem t a2) "Should contain a2"

                TimingWheel.remove t a1
                Expect.equal (TimingWheel.length t) 1 "Should have 1 alarm"
                Expect.isFalse (TimingWheel.mem t a1) "Should not contain a1"
                Expect.isTrue (TimingWheel.mem t a2) "Should contain a2"

            testCase "interval num calculations" <| fun () ->
                let t = createUnit ()
                let start = TimingWheel.start t

                let test afterSeconds =
                    let time = TimeNs.add start (gibiNanos afterSeconds)
                    let intervalNum = TimingWheel.intervalNum t time
                    let intervalNumStart = TimingWheel.intervalNumStart t intervalNum
                    let intervalStart = TimingWheel.intervalStart t time

                    Expect.equal intervalNumStart intervalStart "Interval starts should match"

                test 0.0
                test 0.1
                test 0.99
                test 1.0
                test 1.5
                test 1.99
                test 2.0

            testCase "advance clock" <| fun () ->
                let t = createUnit ()
                let initialNow = TimingWheel.now t
                let toTime = TimeNs.add initialNow (gibiNanos 1.)

                TimingWheel.advanceClock t toTime ignore

                let newNow = TimingWheel.now t
                Expect.equal newNow toTime "Now should be updated"
        ]

        testList "alarm scheduling" [
            testCase "add failures" <| fun () ->
                let t = createUnit () [1]

                // Add alarms at all valid interval nums
                let minAllowed = TimingWheel.minAllowedAlarmIntervalNum t
                let maxAllowed = TimingWheel.maxAllowedAlarmIntervalNum t

                for i = IntervalNum.toIntThrowing minAllowed to IntervalNum.toIntThrowing maxAllowed do
                    TimingWheel.addAtIntervalNum t (IntervalNum.ofInt i) () |> ignore

                // Test invalid adds
                let testInvalidAdd at =
                    Expect.throwsT<Exception>
                        (fun () -> TimingWheel.addAtIntervalNum t at () |> ignore)
                        "Invalid add should fail"

                testInvalidAdd IntervalNum.minValue
                testInvalidAdd (IntervalNum.pred minAllowed)
                testInvalidAdd (IntervalNum.succ maxAllowed)
                testInvalidAdd IntervalNum.maxValue

            testCase "clear" <| fun () ->
                let t = createUnit () [1; 1]
                TimingWheel.addAtIntervalNum t IntervalNum.zero () |> ignore
                TimingWheel.addAtIntervalNum t (IntervalNum.ofInt 2) () |> ignore

                Expect.equal (TimingWheel.length t) 2 "Should have 2 alarms"

                TimingWheel.clear t

                Expect.equal (TimingWheel.length t) 0 "Should be empty after clear"
                Expect.isTrue (TimingWheel.isEmpty t) "Should be empty after clear"

            testCase "reschedule" <| fun () ->
                let epochPlus nSeconds = TimeNs.add TimeNs.epoch (gibiNanos nSeconds)
                let config = createConfig (gibiNanos 1.) [10] ()
                let t = TimingWheel.create config (epochPlus 0.)

                let alarm1 = TimingWheel.add t (epochPlus 5.) ()
                let alarm2 = TimingWheel.add t (epochPlus 10.) ()

                // Initial state
                Expect.equal (TimingWheel.Alarm.at t alarm1) (epochPlus 5.) "alarm1 initial time"
                Expect.equal (TimingWheel.Alarm.at t alarm2) (epochPlus 10.) "alarm2 initial time"

                // Reschedule alarm1 after alarm2
                TimingWheel.reschedule t alarm1 (epochPlus 15.)
                Expect.equal (TimingWheel.Alarm.at t alarm1) (epochPlus 15.) "alarm1 after reschedule"

                // Advance past alarm1's original time - nothing should fire
                let mutable fired = false
                TimingWheel.advanceClock t (epochPlus 7.) (fun _ -> fired <- true)
                Expect.isFalse fired "Nothing should fire"

                // Reschedule alarm1 before alarm2 again
                TimingWheel.reschedule t alarm1 (epochPlus 8.)
                Expect.equal (TimingWheel.Alarm.at t alarm1) (epochPlus 8.) "alarm1 after second reschedule"

                // Cannot reschedule before current time
                Expect.throwsT<Exception>
                    (fun () -> TimingWheel.reschedule t alarm2 (epochPlus 6.))
                    "Cannot reschedule before current time"
        ]

        testList "alarm firing" [
            testCase "fire past alarms" <| fun () ->
                let start = TimeNs.epoch
                let at sec = TimeNs.add start (gibiNanos sec)
                let config = createConfig (gibiNanos 60.) ()
                let t = TimingWheel.create start config

                let mutable firedCount = 0

                // Add alarms at t=1s and t=2s
                let alarm1 = TimingWheel.add t (at 1.) ()
                let alarm2 = TimingWheel.add t (at 2.) ()

                // Both should be in interval 0
                Expect.equal (TimingWheel.Alarm.intervalNum t alarm1) IntervalNum.zero "alarm1 interval"
                Expect.equal (TimingWheel.Alarm.intervalNum t alarm2) IntervalNum.zero "alarm2 interval"

                // Advance to t=1s, then fire past alarms
                TimingWheel.advanceClock t (at 1.) (fun _ -> failwith "Should not fire during advance")
                TimingWheel.firePastAlarms t (fun a ->
                    if TimeNs.equal (TimingWheel.Alarm.at t a) (at 1.) then
                        firedCount <- firedCount + 1
                    else
                        failwith "Wrong alarm fired")

                Expect.equal firedCount 1 "Only alarm at t=1s should fire"

            testCase "advance clock fires alarms in order" <| fun () ->
                let t = createUnit ()
                let mutable firedOrder = []

                // Add alarms in reverse order
                for i in [5..-1..0] do
                    let at = TimeNs.add (TimingWheel.now t) (TimeNs.Span.ofSec 1.)
                    TimingWheel.add t at i |> ignore

                // Advance clock to fire all alarms
                TimingWheel.advanceClock t (TimeNs.add (TimingWheel.now t) (TimeNs.Span.ofSec 2.))
                    (fun a ->
                        firedOrder <- (TimingWheel.Alarm.value t a) :: firedOrder)

                // Alarms should fire in insertion order (0 to 5)
                Expect.equal (List.rev firedOrder) [0..5] "Alarms should fire in insertion order"

            testCase "next alarm fires at" <| fun () ->
                let t = createUnit () [10]

                Expect.isNone (TimingWheel.nextAlarmFiresAt t) "Empty wheel should have no next alarm"

                // Add alarm at 2 seconds
                TimingWheel.add t (TimeNs.add TimeNs.epoch (gibiNanos 2.)) () |> ignore

                match TimingWheel.nextAlarmFiresAt t with
                | Some next ->
                    let expectedNext = TimeNs.add TimeNs.epoch (gibiNanos 3.221225472)
                    let diff = TimeNs.diff next expectedNext |> TimeNs.Span.toInt64Ns |> abs
                    Expect.isLessThan diff 1000L "Next alarm time should be close to expected"
                | None -> failtest "Should have next alarm"
        ]

        testList "edge cases" [
            testCase "max allowed alarm time" <| fun () ->
                let t = createUnit () [1]
                let maxAllowed = TimingWheel.maxAllowedAlarmTime t

                // Should be able to add at max time
                TimingWheel.add t maxAllowed () |> ignore

                // Should not be able to add beyond max time
                let beyondMax = TimeNs.add maxAllowed TimeNs.Span.nanosecond
                Expect.throwsT<Exception>
                    (fun () -> TimingWheel.add t beyondMax () |> ignore)
                    "Cannot add beyond max allowed time"

            testCase "min and max alarm functions" <| fun () ->
                let t = createUnit () [1; 1]

                Expect.isNone (TimingWheel.minAlarmIntervalNum t) "Empty wheel should have no min"

                // Add alarms
                for i in 0..10 do
                    TimingWheel.addAtIntervalNum t (IntervalNum.ofInt i) () |> ignore

                Expect.equal (TimingWheel.minAlarmIntervalNum t) (Some IntervalNum.zero) "Min should be 0"

                // Check min/max alarm time in min interval
                match TimingWheel.maxAlarmTimeInMinInterval t with
                | Some maxTime ->
                    Expect.isGreaterThanOrEqual maxTime TimeNs.epoch "Max time should be valid"
                | None -> failtest "Should have max alarm time"

            testCase "iter" <| fun () ->
                let t = createUnit () [1; 1; 1; 1]
                let mutable count = 0

                TimingWheel.iter t (fun _ -> count <- count + 1)
                Expect.equal count 0 "Empty wheel should not iterate"

                // Add 10 alarms
                for i in 0..9 do
                    TimingWheel.addAtIntervalNum t (IntervalNum.ofInt i) () |> ignore

                count <- 0
                TimingWheel.iter t (fun _ -> count <- count + 1)
                Expect.equal count 10 "Should iterate over all alarms"

                // Remove one alarm by advancing clock
                advanceClockToIntervalNum t (IntervalNum.ofInt 1) ignore

                count <- 0
                TimingWheel.iter t (fun _ -> count <- count + 1)
                Expect.equal count 9 "Should have one less alarm"
        ]
    ]
