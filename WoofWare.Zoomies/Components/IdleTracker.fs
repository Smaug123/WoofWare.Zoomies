namespace WoofWare.Zoomies.Components

open System
open WoofWare.Zoomies

/// Helper for tracking user input timing and idle duration.
/// Embed this State in your application state to track when the user last interacted.
[<RequireQualifiedAccess>]
module IdleTracker =

    /// State for tracking user input timing.
    /// Embed this in your application state.
    type State =
        {
            /// The time of the most recent user input, if any.
            LastInputTime : DateTime voption
        }

        /// Initial state with no recorded input.
        static member Initial =
            {
                LastInputTime = ValueNone
            }

        /// Record that user input occurred at the given time.
        /// Call this from ProcessWorld when handling Keystroke, MouseEvent, or Paste events.
        member this.RecordInput (now : DateTime) : State =
            {
                LastInputTime = ValueSome now
            }

        /// Get the duration since the last user input.
        /// Returns TimeSpan.Zero if no input has been recorded yet.
        member this.IdleDuration (now : DateTime) : TimeSpan =
            match this.LastInputTime with
            | ValueSome t -> now - t
            | ValueNone -> TimeSpan.Zero

    /// Returns true if the given WorldStateChange represents user input
    /// (Keystroke, MouseEvent, or Paste).
    let isUserInput<'appEvent> (change : WorldStateChange<'appEvent>) : bool =
        match change with
        | WorldStateChange.Keystroke _
        | WorldStateChange.MouseEvent _
        | WorldStateChange.Paste _ -> true
        | WorldStateChange.ApplicationEvent _
        | WorldStateChange.ApplicationEventException _ -> false
