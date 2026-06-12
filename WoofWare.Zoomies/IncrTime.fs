namespace WoofWare.Zoomies

open System
open WoofWare.Incremental
open WoofWare.TimingWheel

/// Helpers for creating time-based incremental nodes.
[<RequireQualifiedAccess>]
module IncrTime =

    /// Nanoseconds per second.
    let private nsPerSecond = 1_000_000_000L

    /// The frame index at a given time, for a frame lasting intervalNs.
    let private frameAt (intervalNs : int64) (frameCount : int) (timeNs : TimeNs) : int =
        let ns = TimeNs.toInt64NsSinceEpoch timeNs

        if intervalNs = 0L then
            0
        else
            int ((ns / intervalNs) % int64 frameCount)

    /// The tick count at a given time, for a tick lasting intervalNs.
    let private tickAt (intervalNs : int64) (timeNs : TimeNs) : int64 =
        let ns = TimeNs.toInt64NsSinceEpoch timeNs
        if intervalNs = 0L then 0L else ns / intervalNs

    /// A node yielding the current time, which additionally keeps an alarm scheduled in the
    /// clock's timing wheel every intervalNs, so that the event loop knows when the value
    /// next needs recomputing. The timing wheel refuses intervals below its alarm precision,
    /// so the alarm interval is clamped (values are still computed from the true time).
    let private timeWithAlarmsNode (incr : Incremental) (clock : Clock) (intervalNs : int64) : TimeNs Node =
        let alarmIntervalNs =
            max intervalNs (TimeNs.Span.toInt64Ns (incr.Clock.AlarmPrecision clock))

        let intervalsNode =
            incr.Clock.AtIntervals clock (TimeNs.Span.ofInt64Ns alarmIntervalNs)

        let timeNode = incr.Clock.WatchNow clock
        incr.Map2 (fun () (timeNs : TimeNs) -> timeNs) intervalsNode timeNode

    /// Create a Node yielding the current spinner frame index (0 to frameCount-1).
    /// The value is a pure function of the given time node; no alarms are scheduled, so the
    /// caller is responsible for waking up often enough to advance the animation.
    let spinnerFrameNodeFromTimeNode
        (incr : IncrView)
        (timeNode : int64<timeNs> Node)
        (frameCount : int)
        (fps : float)
        : int Node
        =
        // Gracefully handle invalid inputs by using sensible defaults
        let frameCount = max 1 frameCount
        let intervalNs = if fps <= 0.0 then 0L else int64 (float nsPerSecond / fps)

        // Map to frame index based on time
        // Default cutoff (polyEqual) will prevent unnecessary propagation when frame doesn't change
        incr.Map (frameAt intervalNs frameCount) timeNode

    /// Create a Node yielding the current spinner frame index (0 to frameCount-1).
    /// While the node is alive, an alarm is kept scheduled in the clock's timing wheel at
    /// each frame boundary, so the event loop knows when to wake up for the next frame.
    let spinnerFrameNode (incr : Incremental) (clock : Clock) (frameCount : int) (fps : float) : int Node =
        let frameCount' = max 1 frameCount
        let intervalNs = if fps <= 0.0 then 0L else int64 (float nsPerSecond / fps)

        if intervalNs <= 0L then
            // Degenerate fps: the frame is constant, so schedule no alarms.
            incr.Return 0
        else
            timeWithAlarmsNode incr clock intervalNs
            |> incr.Map (frameAt intervalNs frameCount')

    /// Create a Node that yields a tick count incrementing at the given interval.
    /// The value is a pure function of the given time node; no alarms are scheduled, so the
    /// caller is responsible for waking up often enough to observe the ticks.
    let periodicTickNodeFromTimeNode
        (incr : IncrView)
        (timeNode : int64<timeNs> Node)
        (interval : TimeSpan)
        : int64 Node
        =
        // Ticks are 100ns units; multiply by 100 to get nanoseconds without floating-point truncation.
        // Gracefully handle non-positive intervals (same pattern as spinnerFrameNodeFromTimeNode).
        let intervalNs = if interval.Ticks <= 0L then 0L else interval.Ticks * 100L

        incr.Map (tickAt intervalNs) timeNode

    /// Create a Node that yields a tick count incrementing at the given interval.
    /// While the node is alive, an alarm is kept scheduled in the clock's timing wheel at
    /// each tick boundary, so the event loop knows when to wake up for the next tick.
    let periodicTickNode (incr : Incremental) (clock : Clock) (interval : TimeSpan) : int64 Node =
        let intervalNs = if interval.Ticks <= 0L then 0L else interval.Ticks * 100L

        if intervalNs <= 0L then
            // Degenerate interval: the tick count is constant, so schedule no alarms.
            incr.Return 0L
        else
            timeWithAlarmsNode incr clock intervalNs |> incr.Map (tickAt intervalNs)
