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

    /// State for tracking user input timing with animation frame tracking.
    /// Use this when you have an idle animation that needs to trigger re-renders
    /// at specific frame intervals.
    type AnimatedState =
        {
            /// The time of the most recent user input, if any.
            LastInputTime : DateTime voption
            /// The last animation frame index that was rendered.
            LastPulseFrame : int
        }

        /// Initial state with no recorded input and frame 0.
        static member Initial =
            {
                LastInputTime = ValueNone
                LastPulseFrame = 0
            }

        /// Record that user input occurred at the given time.
        /// Resets the animation frame to 0.
        member this.RecordInput (now : DateTime) : AnimatedState =
            {
                LastInputTime = ValueSome now
                LastPulseFrame = 0
            }

        /// Get the duration since the last user input.
        /// Returns TimeSpan.Zero if no input has been recorded yet.
        member this.IdleDuration (now : DateTime) : TimeSpan =
            match this.LastInputTime with
            | ValueSome t -> now - t
            | ValueNone -> TimeSpan.Zero

        /// Advance the pulse animation based on current time.
        /// Returns (newState, shouldRerender) where shouldRerender is true if the frame changed.
        /// Use this in ProcessWorld to determine when to request a re-render.
        ///
        /// Parameters:
        /// - now: Current time
        /// - idleThresholdSeconds: Seconds of idle time before animation starts
        /// - pulseSpeedCharsPerSec: Animation speed in characters per second
        /// - textLength: Length of the text being animated (for wrapping)
        member this.AdvancePulse
            (now : DateTime)
            (idleThresholdSeconds : float)
            (pulseSpeedCharsPerSec : float)
            (textLength : int)
            : struct (AnimatedState * bool)
            =
            let idleDuration = this.IdleDuration now

            if idleDuration.TotalSeconds < idleThresholdSeconds then
                struct (this, false) // Not idle enough, no pulse
            else if textLength <= 0 then
                struct (this, false) // No text to animate
            else
                let elapsed = idleDuration.TotalSeconds - idleThresholdSeconds
                let currentFrame = int (elapsed * pulseSpeedCharsPerSec) % textLength

                if currentFrame <> this.LastPulseFrame then
                    struct ({ this with
                                LastPulseFrame = currentFrame
                            },
                            true)
                else
                    struct (this, false)
