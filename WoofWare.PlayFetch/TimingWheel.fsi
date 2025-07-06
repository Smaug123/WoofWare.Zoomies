namespace WoofWare.PlayFetch

[<Struct>]
type IntervalNum = { Span: int64 }

[<RequireQualifiedAccess>]
module internal IntervalNum =
    type Span

    [<RequireQualifiedAccess>]
    module Span =
        val max: Span -> Span -> Span
        val zero: Span
        val one: Span
        val maxValue: Span
        val ofInt64: int64 -> Span
        val toInt64: Span -> int64
        val ofInt: int -> Span
        val toIntThrowing: Span -> int
        val pred: Span -> Span
        val succ: Span -> Span
        val add: Span -> Span -> Span
        val scale: Span -> int -> Span

    val max: IntervalNum -> IntervalNum -> IntervalNum
    val min: IntervalNum -> IntervalNum -> IntervalNum
    val zero: IntervalNum
    val one: IntervalNum
    val minValue: IntervalNum
    val maxValue: IntervalNum
    val ofInt64: int64 -> IntervalNum
    val toInt64: IntervalNum -> int64
    val ofInt: int -> IntervalNum
    val toIntThrowing: IntervalNum -> int
    val pred: IntervalNum -> IntervalNum
    val succ: IntervalNum -> IntervalNum
    val add: IntervalNum -> IntervalNum -> IntervalNum
    val sub: IntervalNum -> IntervalNum -> IntervalNum
    val diff: IntervalNum -> IntervalNum -> IntervalNum
    val rem: IntervalNum -> Span -> Span

type AlarmPrecision

[<RequireQualifiedAccess>]
module AlarmPrecision =
    val ofSpanFloorPow2Ns: TimeNs.Span -> AlarmPrecision
    val toSpan: AlarmPrecision -> TimeNs.Span
    val oneNanosecond: AlarmPrecision
    ///  ~19.5 h
    val aboutOneDay: AlarmPrecision
    /// 1.024 us
    val aboutOneMicrosecond: AlarmPrecision

    /// ~1.05 ms
    val aboutOneMillisecond: AlarmPrecision

    /// ~1.07 s
    val aboutOneSecond: AlarmPrecision

    /// [mul t ~pow2] is [t * 2^pow2]. [pow2] may be negative, but [mul] does not check for
    /// overflow or underflow.
    val mul: AlarmPrecision -> pow2: int -> AlarmPrecision

    /// [div t ~pow2] is [t / 2^pow2]. [pow2] may be negative, but [div] does not check for
    /// overflow or underflow.
    val div: AlarmPrecision -> pow2: int -> AlarmPrecision

type TimingWheel<'a>
