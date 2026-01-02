namespace WoofWare.Zoomies

open System.Collections.Concurrent
open WoofWare.Incremental

/// A state machine that manages state updates via events flowing through the Incremental graph.
/// Events are queued and processed on stabilization, folding them into state using the transition function.
type StateMachine<'state, 'event> =
    {
        /// The current state as an Incremental node.
        /// Dependents will be recomputed when state changes.
        StateNode : 'state Node

        /// Inject an event to be processed on the next stabilization.
        /// The event is queued and will be folded into state when Stabilize is called.
        Inject : 'event -> unit

        /// Read the current state synchronously.
        /// This returns the state as of the last stabilization (or initial state if not yet stabilized),
        /// plus any events that have been processed during the current stabilization.
        CurrentState : unit -> 'state

        /// Set the state directly, bypassing the event queue.
        /// This is useful for post-layout events or other cases where immediate state update is needed.
        /// The node will be marked stale and recompute on next stabilization.
        SetState : 'state -> unit
    }

[<RequireQualifiedAccess>]
module StateMachine =

    /// Create a new state machine with the given initial state and transition function.
    ///
    /// The transition function is called for each event in the order they were injected,
    /// folding them into the state: newState = fold transition initial events.
    ///
    /// Events are batched between stabilizations - multiple Inject calls before a Stabilize
    /// will all be processed in a single recomputation.
    let create<'state, 'event>
        (state : State)
        (initial : 'state)
        (transition : 'state -> 'event -> 'state)
        : StateMachine<'state, 'event>
        =
        let eventsQueue = ConcurrentQueue<'event> ()
        let mutable currentState = initial

        let expertNode =
            Expert1Node.create
                state
                None // onObservabilityChange
                (fun () ->
                    // Drain the event queue and fold events into state
                    let mutable ev = Unchecked.defaultof<'event>

                    while eventsQueue.TryDequeue (&ev) do
                        currentState <- transition currentState ev

                    currentState
                )

        let inject (ev : 'event) : unit =
            eventsQueue.Enqueue ev
            Expert1Node.makeStale expertNode

        let setState (newState : 'state) : unit =
            currentState <- newState
            Expert1Node.makeStale expertNode

        {
            StateNode = Expert1Node.watch expertNode
            Inject = inject
            CurrentState = fun () -> currentState
            SetState = setState
        }
