namespace WoofWare.Zoomies

open System
open WoofWare.Incremental
open WoofWare.Zoomies
open WoofWare.Zoomies.Components

/// Events from user input
type AppEvent =
    | ButtonClicked
    | CounterUp
    | CounterDown

/// Events posted during rendering
type PostLayoutEvent = | NoOp

type State =
    {
        /// Counter that increments on each user input event (for idle tracking)
        InputEventCount : int
        /// Simple counter to show interactivity
        Counter : int
        /// Number of button clicks
        Clicks : int
    }

    static member Initial =
        {
            InputEventCount = 0
            Counter = 0
            Clicks = 0
        }

module RainbowDemo =
    let buttonKey = NodeKey.make "button"
    let counterUpKey = NodeKey.make "counter-up"
    let counterDownKey = NodeKey.make "counter-down"

    let processWorld =
        { new WorldProcessor<AppEvent, PostLayoutEvent, State> with
            member _.ProcessWorld (changes, _prevVdom, state) =
                let mutable inputEventCount = state.InputEventCount
                let mutable counter = state.Counter
                let mutable clicks = state.Clicks

                for change in changes do
                    match change with
                    | WorldStateChange.MouseEvent _
                    | WorldStateChange.Keystroke _
                    | WorldStateChange.Paste _ ->
                        // Increment counter to signal input happened (incremental view will capture time)
                        inputEventCount <- inputEventCount + 1

                    | WorldStateChange.ApplicationEvent ButtonClicked -> clicks <- clicks + 1

                    | WorldStateChange.ApplicationEvent CounterUp -> counter <- counter + 1

                    | WorldStateChange.ApplicationEvent CounterDown -> counter <- counter - 1

                    | WorldStateChange.ApplicationEventException _ -> ()

                ProcessWorldResult.make
                    {
                        InputEventCount = inputEventCount
                        Counter = counter
                        Clicks = clicks
                    }

            member _.ProcessPostLayoutEvents (_events, _ctx, state) = state
        }

    let rainbowConfig =
        { RainbowText.Config.Default with
            IdleThresholdSeconds = 2.0
            PulseWidthChars = 5
            PulseSpeedCharsPerSec = 8.0
        }

    /// Incremental view function - uses the Incremental infrastructure for proper animation
    let incrView (ctx : VdomContext<PostLayoutEvent>) (stateNode : State Node) : Vdom<DesiredBounds> Node =
        let incr = VdomContext.incr ctx
        // VdomContext implements IVdomContext, so we can upcast it
        let typedCtx : IVdomContext<PostLayoutEvent> = ctx :> _

        // Get the clock time as DateTime for time-based animations
        let clockDateTimeNode = VdomContext.clockDateTimeNode ctx

        // Track input event count and capture clock time when it changes
        let inputEventCountNode = incr.Map (fun s -> s.InputEventCount) stateNode

        // Capture the clock time when input happens (using local mutable state for the "sample and hold")
        let mutable lastSeenCount = 0
        let mutable capturedInputTime : DateTime voption = ValueNone

        let lastInputTimeNode : DateTime voption Node =
            incr.Map2
                (fun count currentTime ->
                    if count > lastSeenCount then
                        // Input happened - capture the current clock time
                        lastSeenCount <- count
                        capturedInputTime <- ValueSome currentTime

                    capturedInputTime
                )
                inputEventCountNode
                clockDateTimeNode

        // Create the rainbow text nodes using the incremental API
        let headerRainbowNode =
            RainbowText.makeIncr
                incr
                clockDateTimeNode
                lastInputTimeNode
                "Welcome to WoofWare.Zoomies Rainbow Demo!"
                rainbowConfig

        let secondRainbowNode =
            RainbowText.makeIncr
                incr
                clockDateTimeNode
                lastInputTimeNode
                "The quick brown fox jumps over the lazy dog"
                rainbowConfig

        // Create the idle status text as a Node
        let idleStatusNode =
            incr.Map2
                (fun currentTime lastInputTime ->
                    let idleDuration =
                        match lastInputTime with
                        | ValueSome t -> currentTime - t
                        | ValueNone -> TimeSpan.Zero

                    let idleSeconds = idleDuration.TotalSeconds

                    if idleSeconds < rainbowConfig.IdleThresholdSeconds then
                        sprintf "Idle: %.1fs (pulse starts at %.1fs)" idleSeconds rainbowConfig.IdleThresholdSeconds
                    else
                        sprintf "Idle: %.1fs - PULSING!" idleSeconds
                )
                clockDateTimeNode
                lastInputTimeNode

        // Combine all the nodes into the final Vdom
        incr.Map
            (fun
                ((((state, headerRainbow : Vdom<DesiredBounds>), secondRainbow : Vdom<DesiredBounds>), idleStatus),
                 _bounds) ->
                // Interactive elements (these don't need Incremental since they're not animated)
                let counterDisplay = Vdom.textContent (sprintf "Counter: %d" state.Counter)

                let counterButtons =
                    let upButton = Button.make (typedCtx, counterUpKey, "[+]")
                    let downButton = Button.make (typedCtx, counterDownKey, "[-]")
                    Vdom.panelSplitAuto (SplitDirection.Vertical, upButton, downButton)

                let counterRow =
                    Vdom.panelSplitAuto (SplitDirection.Vertical, counterDisplay, counterButtons)

                let clickButton =
                    Button.make (typedCtx, buttonKey, sprintf "Click me! (clicked %d times)" state.Clicks)

                let instructions =
                    Vdom.textContent
                        "Press Tab to cycle focus, Enter/Space to activate buttons.\nStop interacting to see the rainbow pulse!"

                let idleStatusVdom = Vdom.textContent idleStatus

                // Layout: stack everything vertically
                // Add type annotations to help inference with overloaded panelSplitAbsolute
                let headerRow : Vdom<DesiredBounds> =
                    Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 1, headerRainbow, Vdom.empty)

                let secondRainbowRow : Vdom<DesiredBounds> =
                    Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 1, secondRainbow, Vdom.empty)

                let instructionsRow : Vdom<DesiredBounds> =
                    Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 2, instructions, secondRainbowRow)

                let buttonRow : Vdom<DesiredBounds> =
                    Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 1, clickButton, instructionsRow)

                let counterRowLayout : Vdom<DesiredBounds> =
                    Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 1, counterRow, buttonRow)

                let statusRow : Vdom<DesiredBounds> =
                    Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 1, idleStatusVdom, counterRowLayout)

                let content : Vdom<DesiredBounds> =
                    Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 1, headerRow, statusRow)

                Vdom.bordered content
            )
            (incr.Map2
                (fun a b -> a, b)
                (incr.Map2
                    (fun a b -> a, b)
                    (incr.Map2
                        (fun a b -> a, b)
                        (incr.Map2 (fun a b -> a, b) stateNode headerRainbowNode)
                        secondRainbowNode)
                    idleStatusNode)
                (VdomContext.boundsNode ctx))

    let resolver : ActivationResolver<AppEvent, State> =
        ActivationResolver.combine
            [
                ActivationResolver.button buttonKey ButtonClicked
                ActivationResolver.button counterUpKey CounterUp
                ActivationResolver.button counterDownKey CounterDown
            ]

    let run (getEnv : string -> string option) =
        App.run getEnv State.Initial (fun _ -> true) (fun _ -> processWorld) incrView resolver

module Program =
    let getEnv (varName : string) : string option =
        match Environment.GetEnvironmentVariable varName with
        | null -> None
        | value -> Some value

    [<EntryPoint>]
    let main _argv =
        (RainbowDemo.run getEnv).Finished.Wait ()
        0
