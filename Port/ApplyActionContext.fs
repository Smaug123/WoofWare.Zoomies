namespace WoofWare.Zoomies.Port

// This module handles the context for applying actions, including effect injection and event scheduling.
// In the original OCaml, this uses Ui_effect.t which we'll represent as a generic effect type.
module ApplyActionContext =

    // For now, we'll define a simple effect type that represents a unit-returning computation
    // This may need to be refined as we understand more about the effect system requirements
    type Effect<'a> = unit -> 'a

    type ApplyActionContext<'action> =
        {
            Inject : 'action -> Effect<unit>
            ScheduleEvent : Effect<unit> -> unit
        }

    let inject (context : ApplyActionContext<'action>) (action : 'action) = context.Inject action

    let scheduleEvent (context : ApplyActionContext<_>) (event : Effect<unit>) = context.ScheduleEvent event

    let create (inject : 'action -> Effect<unit>) (scheduleEvent : Effect<unit> -> unit) =
        {
            Inject = inject
            ScheduleEvent = scheduleEvent
        }
