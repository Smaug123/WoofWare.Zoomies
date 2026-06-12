namespace WoofWare.Zoomies

open System
open WoofWare.Incremental
open WoofWare.TimingWheel

/// Helpers for converting between DateTime and nanoseconds since epoch.
[<RequireQualifiedAccess>]
module internal TimeConversion =
    /// Ticks per nanosecond (1 tick = 100 ns, so 1 ns = 0.01 ticks)
    let private ticksPerNanosecond = 0.01

    /// Nanoseconds per tick (1 tick = 100 ns)
    let private nanosecondsPerTick = 100L

    /// Unix epoch as DateTime
    let unixEpoch = DateTime (1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)

    /// Convert DateTime to nanoseconds since Unix epoch.
    let dateTimeToNs (dt : DateTime) : int64<timeNs> =
        let ticksSinceEpoch = dt.ToUniversalTime().Ticks - unixEpoch.Ticks
        TimeNs.ofInt64NsSinceEpoch (ticksSinceEpoch * nanosecondsPerTick)

    /// Convert nanoseconds since Unix epoch to DateTime.
    let nsToDateTime (ns : int64<timeNs>) : DateTime =
        let ticksSinceEpoch = TimeNs.toInt64NsSinceEpoch ns / nanosecondsPerTick
        DateTime (unixEpoch.Ticks + ticksSinceEpoch, DateTimeKind.Utc)

/// State encapsulating the Incremental computation graph for reactive UI updates.
/// This holds the core Incremental instance, the clock for time-based reactivity,
/// and the primary input variables (terminal bounds, focus).
/// User state is managed separately, in a Var owned by the app loop.
type IncrementalState =
    {
        /// The Incremental instance managing the dependency graph.
        Incr : Incremental

        /// Clock for time-based reactivity (spinners, animations, etc.).
        Clock : Clock

        /// The terminal bounds, updated when the terminal is resized.
        TerminalBoundsVar : Rectangle Var

        /// The currently focused NodeKey, updated when focus changes.
        FocusedKeyVar : NodeKey option Var

        /// Cached clock time as DateTime (each Incr.Map call creates a new node).
        ClockDateTimeNode : DateTime Node
    }

[<RequireQualifiedAccess>]
module IncrementalState =

    /// Create a new IncrementalState with the given initial values.
    let make (initialBounds : Rectangle) (initialFocusedKey : NodeKey option) : IncrementalState =
        let incr = Incremental.make ()
        let epochNs = TimeConversion.dateTimeToNs TimeConversion.unixEpoch
        let clock = incr.Clock.Create epochNs
        let clockNsNode = incr.Clock.WatchNow clock
        let clockDateTimeNode = incr.Map TimeConversion.nsToDateTime clockNsNode

        {
            Incr = incr
            Clock = clock
            TerminalBoundsVar = incr.Var.Create initialBounds
            FocusedKeyVar = incr.Var.Create initialFocusedKey
            ClockDateTimeNode = clockDateTimeNode
        }

    /// Get the terminal bounds as a Node for incremental computations.
    let boundsNode (s : IncrementalState) : Rectangle Node = s.Incr.Var.Watch s.TerminalBoundsVar

    /// Get the focused key as a Node for incremental computations.
    let focusedKeyNode (s : IncrementalState) : NodeKey option Node = s.Incr.Var.Watch s.FocusedKeyVar

    /// Update the terminal bounds.
    let setBounds (newBounds : Rectangle) (s : IncrementalState) : unit =
        s.Incr.Var.Set s.TerminalBoundsVar newBounds

    /// Update the focused key.
    let setFocusedKey (key : NodeKey option) (s : IncrementalState) : unit = s.Incr.Var.Set s.FocusedKeyVar key

    /// Get the clock time as an incremental Node.
    /// This allows components to depend on time incrementally.
    let clockTimeNode (s : IncrementalState) : int64<WoofWare.TimingWheel.timeNs> Node = s.Incr.Clock.WatchNow s.Clock

    /// Get the clock time as a DateTime Node (cached).
    let clockDateTimeNode (s : IncrementalState) : DateTime Node = s.ClockDateTimeNode

    /// Advance the clock to the given time and stabilize.
    /// Advancing is a no-op unless time has strictly advanced (AdvanceClock guarantees this
    /// upstream), but stabilization happens unconditionally so pending var sets always propagate.
    let advanceClockAndStabilize (time : DateTime) (s : IncrementalState) : unit =
        s.Incr.Clock.AdvanceClock s.Clock (TimeConversion.dateTimeToNs time)
        s.Incr.Stabilize ()

    /// Stabilize the computation graph without advancing the clock.
    let stabilize (s : IncrementalState) : unit = s.Incr.Stabilize ()
