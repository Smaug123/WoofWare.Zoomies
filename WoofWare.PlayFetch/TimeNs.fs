namespace WoofWare.PlayFetch

open System

[<RequireQualifiedAccess>]
module Int64 =
    let floor_log2 (n : int64) =
        if n <= 0L then
            failwith "floor_log2 of non-positive number"
        let mutable result = 0
        let mutable n = n
        if n >= (1L <<< 32) then
            n <- n >>> 32
            result <- result + 32
        if n >= (1L <<< 16) then
            n <- n >>> 16
            result <- result + 16
        if n >= (1L <<< 8) then
            n <- n >>> 8
            result <- result + 8
        if n >= (1L <<< 4) then
            n <- n >>> 4
            result <- result + 4
        if n >= (1L <<< 2) then
            n <- n >>> 2
            result <- result + 2
        if n >= (1L <<< 1) then
            result <- result + 1
        result

[<Struct>]
type TimeNs =
    | TimeNs of int64

[<RequireQualifiedAccess>]
module TimeNs =

    [<Struct>]
    type Span =
        | Span of int64

    [<RequireQualifiedAccess>]
    module Span =
        let zero = Span 0L

        let nanosecond = Span 1L

        let ofInt64Ns (ns : int64) = Span ns

        let of_int63_ns = ofInt64Ns

        let toInt64Ns (Span ns) = ns

        let to_int63_ns = toInt64Ns

        let ofInt (i : int) = Span (int64 i)

        let toInt (Span ns) = int ns

        let toIntThrowing (Span ns) =
            if ns > int64 Int32.MaxValue || ns < int64 Int32.MinValue then
                failwithf "Span %d nanoseconds cannot be represented as int" ns
            int ns

        let add (Span a) (Span b) = Span (a + b)

        let sub (Span a) (Span b) = Span (a - b)

        let scale (Span ns) (factor : int) = Span (ns * int64 factor)

        let pred (Span ns) = Span (ns - 1L)

        let succ (Span ns) = Span (ns + 1L)

        let max (Span a) (Span b) = Span (max a b)

        let min (Span a) (Span b) = Span (min a b)

        let maxValue = Span Int64.MaxValue

        let minValue = Span Int64.MinValue

        let compare (Span a) (Span b) = compare a b

        let ( + ) = add
        let ( - ) = sub

        let ofTimeSpan (ts : TimeSpan) =
            let ticks = ts.Ticks
            let nanoseconds = ticks * 100L
            Span nanoseconds

        let toTimeSpan (Span ns) =
            let ticks = ns / 100L
            TimeSpan(ticks)

    let epoch = TimeNs 0L

    let minTime = epoch

    let maxValueRepresentable = TimeNs Int64.MaxValue

    let max_value_representable = maxValueRepresentable

    let minValueFor1usRounding = TimeNs Int64.MinValue

    let min_value_for_1us_rounding = minValueFor1usRounding

    let ofInt64NsSinceEpoch (ns : int64) = TimeNs ns

    let of_int63_ns_since_epoch = ofInt64NsSinceEpoch

    let toInt64NsSinceEpoch (TimeNs ns) = ns

    let to_int63_ns_since_epoch = toInt64NsSinceEpoch

    let add (TimeNs t) (Span.Span s) = TimeNs (t + s)

    let sub (TimeNs t) (Span.Span s) = TimeNs (t - s)

    let diff (TimeNs a) (TimeNs b) = Span.Span (a - b)

    let min (TimeNs a) (TimeNs b) = TimeNs (min a b)

    let max (TimeNs a) (TimeNs b) = TimeNs (max a b)

    let compare (TimeNs a) (TimeNs b) = compare a b

    let equal (TimeNs a) (TimeNs b) = a = b
