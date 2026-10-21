namespace WoofWare.Zoomies.Test

open WoofWare.Zoomies

/// Test utilities for WorldProcessor
[<RequireQualifiedAccess>]
module WorldProcessor =

    /// A WorldProcessor that ignores all events and returns the state unchanged.
    /// Generic only on state; assumes unit for app events and post-layout events (the overwhelmingly common case).
    let passthrough<'state> : WorldProcessor<unit, unit, 'state> =
        { new WorldProcessor<unit, unit, 'state> with
            member _.ProcessWorld (_, _, state) = ProcessWorldResult.make state
            member _.ProcessPostLayoutEvents (_, _, state) = state
        }

type MockWorld =
    {
        /// Deliver a key into the attached WorldFreezer, as the platform input thread would.
        /// Synchronous: when this returns, the key is visible to the freezer's Changes.
        SendKey : System.ConsoleKeyInfo -> unit
    }

[<RequireQualifiedAccess>]
module MockWorld =

    /// Create a MockWorld that delivers keystrokes synchronously into the given freezer.
    let attach (freezer : WorldFreezer<'appEvent>) : MockWorld =
        {
            SendKey = freezer.DeliverKeystroke
        }
