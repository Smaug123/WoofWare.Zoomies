namespace WoofWare.Zoomies

open System.Diagnostics

/// An abstraction over System.Diagnostics.Stopwatch.
type IStopwatch =
    /// Current elapsed number of ticks
    abstract GetTimestamp : unit -> int64
    /// Number of ticks per second
    abstract Frequency : int64

[<RequireQualifiedAccess>]
module Stopwatch =

    /// An IStopwatch that calls through to System.Diagnostics builtins.
    let system =
        { new IStopwatch with
            member _.GetTimestamp () = Stopwatch.GetTimestamp ()
            member _.Frequency = Stopwatch.Frequency
        }
