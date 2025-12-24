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
    let private unixEpoch = DateTime (1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)

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
/// and the primary input variables (state, terminal bounds, focus).
type IncrementalState<'userState> =
    {
        /// The Incremental instance managing the dependency graph.
        Incr : Incremental

        /// Clock for time-based reactivity (spinners, animations, etc.).
        Clock : Clock

        /// The application's user state, as a mutable variable that can be updated.
        StateVar : 'userState Var

        /// The terminal bounds, updated when the terminal is resized.
        TerminalBoundsVar : Rectangle Var

        /// The currently focused NodeKey, updated when focus changes.
        FocusedKeyVar : NodeKey option Var
    }

[<RequireQualifiedAccess>]
module IncrementalState =

    /// Create a new IncrementalState with the given initial values.
    let make<'userState>
        (initialState : 'userState)
        (initialBounds : Rectangle)
        (initialFocusedKey : NodeKey option)
        : IncrementalState<'userState>
        =
        let incr = Incremental.make ()
        let nowNs = TimeConversion.dateTimeToNs DateTime.UtcNow

        {
            Incr = incr
            Clock = incr.Clock.Create nowNs
            StateVar = incr.Var.Create initialState
            TerminalBoundsVar = incr.Var.Create initialBounds
            FocusedKeyVar = incr.Var.Create initialFocusedKey
        }

    /// Get the user state as a Node for incremental computations.
    let stateNode (s : IncrementalState<'userState>) : 'userState Node = s.Incr.Var.Watch s.StateVar

    /// Get the terminal bounds as a Node for incremental computations.
    let boundsNode (s : IncrementalState<'userState>) : Rectangle Node = s.Incr.Var.Watch s.TerminalBoundsVar

    /// Get the focused key as a Node for incremental computations.
    let focusedKeyNode (s : IncrementalState<'userState>) : NodeKey option Node = s.Incr.Var.Watch s.FocusedKeyVar

    /// Update the user state.
    let setState (newState : 'userState) (s : IncrementalState<'userState>) : unit = s.Incr.Var.Set s.StateVar newState

    /// Update the terminal bounds.
    let setBounds (newBounds : Rectangle) (s : IncrementalState<'userState>) : unit =
        s.Incr.Var.Set s.TerminalBoundsVar newBounds

    /// Update the focused key.
    let setFocusedKey (key : NodeKey option) (s : IncrementalState<'userState>) : unit =
        s.Incr.Var.Set s.FocusedKeyVar key

    /// Get the clock time as an incremental Node.
    /// This allows components to depend on time incrementally.
    let clockTimeNode (s : IncrementalState<'userState>) : int64<WoofWare.TimingWheel.timeNs> Node =
        s.Incr.Clock.WatchNow s.Clock

    /// Get the clock time as a DateTime Node for convenience.
    let clockDateTimeNode (s : IncrementalState<'userState>) : DateTime Node =
        s.Incr.Map TimeConversion.nsToDateTime (clockTimeNode s)

    /// Advance the clock to the given time and stabilize the computation graph.
    let advanceClockAndStabilize (time : DateTime) (s : IncrementalState<'userState>) : unit =
        s.Incr.Clock.AdvanceClock s.Clock (TimeConversion.dateTimeToNs time)
        s.Incr.Stabilize ()

    /// Stabilize the computation graph without advancing the clock.
    let stabilize (s : IncrementalState<'userState>) : unit = s.Incr.Stabilize ()
