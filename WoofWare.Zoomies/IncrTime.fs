namespace WoofWare.Zoomies

open System
open WoofWare.Incremental
open WoofWare.TimingWheel

/// Helpers for creating time-based incremental nodes.
[<RequireQualifiedAccess>]
module IncrTime =

    /// Nanoseconds per second.
    let private nsPerSecond = 1_000_000_000L

    /// Create a Node that yields the current spinner frame index.
    /// The spinner has `frameCount` frames and runs at `fps` frames per second.
    /// The node updates when the clock advances past a frame boundary.
    /// Uses default cutoff (polyEqual) so only propagates when frame index changes.
    let spinnerFrameNode (incr : Incremental) (clock : Clock) (frameCount : int) (fps : float) : int Node =
        if frameCount <= 0 then
            invalidArg (nameof frameCount) "frameCount must be positive"

        if fps <= 0.0 then
            invalidArg (nameof fps) "fps must be positive"

        // Calculate interval in nanoseconds
        let intervalNs = int64 (float nsPerSecond / fps)

        // Watch the current time
        let timeNode = incr.Clock.WatchNow clock

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

    /// Create a Node that yields a tick count that increments at the given interval.
    /// This can be used to trigger periodic updates.
    /// Uses default cutoff (polyEqual) so only propagates when tick count changes.
    let periodicTickNode (incr : Incremental) (clock : Clock) (interval : TimeSpan) : int64 Node =
        let intervalNs = int64 interval.TotalMilliseconds * 1_000_000L
        let timeNode = incr.Clock.WatchNow clock

        incr.Map
            (fun (timeNs : int64<timeNs>) ->
                let ns = TimeNs.toInt64NsSinceEpoch timeNs
                if intervalNs = 0L then 0L else ns / intervalNs
            )
            timeNode
