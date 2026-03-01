namespace WoofWare.Zoomies

open System
open WoofWare.Incremental
open WoofWare.TimingWheel

/// Helpers for creating time-based incremental nodes.
[<RequireQualifiedAccess>]
module IncrTime =

    /// Nanoseconds per second.
    let private nsPerSecond = 1_000_000_000L

    /// Create a Node yielding the current spinner frame index (0 to frameCount-1).
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
        incr.Map
            (fun (timeNs : int64<timeNs>) ->
                let ns = TimeNs.toInt64NsSinceEpoch timeNs

                if intervalNs = 0L then
                    0
                else
                    int ((ns / intervalNs) % int64 frameCount)
            )
            timeNode

    let spinnerFrameNode (incr : Incremental) (clock : Clock) (frameCount : int) (fps : float) : int Node =
        let timeNode = incr.Clock.WatchNow clock
        spinnerFrameNodeFromTimeNode (IncrView incr) timeNode frameCount fps

    /// Create a Node that yields a tick count incrementing at the given interval.
    let periodicTickNodeFromTimeNode
        (incr : IncrView)
        (timeNode : int64<timeNs> Node)
        (interval : TimeSpan)
        : int64 Node
        =
        // Ticks are 100ns units; multiply by 100 to get nanoseconds without floating-point truncation.
        // Gracefully handle non-positive intervals (same pattern as spinnerFrameNodeFromTimeNode).
        let intervalNs = if interval.Ticks <= 0L then 0L else interval.Ticks * 100L

        incr.Map
            (fun (timeNs : int64<timeNs>) ->
                let ns = TimeNs.toInt64NsSinceEpoch timeNs
                if intervalNs = 0L then 0L else ns / intervalNs
            )
            timeNode

    let periodicTickNode (incr : Incremental) (clock : Clock) (interval : TimeSpan) : int64 Node =
        let timeNode = incr.Clock.WatchNow clock

        periodicTickNodeFromTimeNode (IncrView incr) timeNode interval
