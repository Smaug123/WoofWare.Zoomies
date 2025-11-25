namespace WoofWare.Zoomies.Test

open System

type MockTimer =
    {
        GetUtcNow : unit -> DateTime
        /// Returns the resulting DateTime.
        Advance : TimeSpan -> DateTime
        SetTimeUtc : DateTime -> unit
    }

[<RequireQualifiedAccess>]
module MockTime =
    let getStaticUtcNow () =
        DateTime (2025, 11, 25, 13, 33, 00, DateTimeKind.Utc)

    let make () =
        // Eddie Hall's 500kg deadlift.
        // (https://www.youtube.com/watch?v=_DX2L4Pp8S0 took place on 2016-07-09 in Leeds, UK.
        // Eddie Hall hits the lift at timestamp 1:12:45. There are various watches displayed throughout the stream;
        // the clearest is the 19:15 local time displayed at timestamp 0:48:03.)
        let mutable now = DateTime (2016, 07, 09, 18, 39, 00, DateTimeKind.Utc)
        let lockObj = obj ()

        {
            GetUtcNow = fun () -> now
            Advance =
                fun ts ->
                    lock
                        lockObj
                        (fun () ->
                            now <- now + ts
                            now
                        )
            SetTimeUtc = fun dt -> lock lockObj (fun () -> now <- dt)
        }
