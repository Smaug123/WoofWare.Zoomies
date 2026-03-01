namespace WoofWare.Zoomies

open System.Collections.Concurrent
open WoofWare.Incremental

/// A state machine that manages state updates via events flowing through the Incremental graph.
/// Events are queued and processed on stabilization, folding them into state using the transition function.
///
/// Two usage patterns:
/// - Simple queue-fold: call `Inject` to enqueue events, then stabilize. The machine drains the queue
///   and folds events into state automatically.
/// - External batch fold: call `SetState` directly after performing your own event routing and folding
///   (e.g. when the caller needs complex pre-routing such as activation resolvers or Tab handling,
///   as App.fs does).
type StateMachine<'state, 'event> =
    {
        /// The current state as an Incremental node.
        /// Dependents will be recomputed when state changes.
        StateNode : 'state Node

        /// Inject an event to be processed on the next stabilization.
        /// The machine drains the queue and folds events in injection order.
        /// Use this for simple event processing without pre-routing.
        Inject : 'event -> unit

        /// Read the current state synchronously.
        CurrentState : unit -> 'state

        /// Set the state directly, bypassing the event queue.
        /// This is the primary API when callers perform their own event routing and batch fold
        /// (as App.fs does for activation resolvers and Tab handling).
        SetState : 'state -> unit
    }

[<RequireQualifiedAccess>]
module StateMachine =

    /// Create a new state machine with the given initial state and transition function.
    /// Events are folded in injection order on each stabilization.
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
