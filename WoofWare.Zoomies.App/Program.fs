namespace WoofWare.Zoomies.App

open System
open WoofWare.Zoomies

type FocusedElement =
    | Option1
    | Option2
    | Option3
    | ShowDetails
    | CompactMode
    | DebugInfo

type TestState =
    {
        mutable FocusedElement : FocusedElement
        mutable SelectedOption : int
        mutable ShowDetails : bool
        mutable CompactMode : bool
        mutable DebugInfo : bool
    }

    static member Initial () =
        {
            SelectedOption = 0
            ShowDetails = false
            CompactMode = false
            DebugInfo = true
            FocusedElement = FocusedElement.Option1
        }

module Program =

    /// Creates a VDOM that will stress-test the constraint system
    let vdom (state : TestState) : Vdom =

        // Header with title - needs reasonable width
        let header =
            Vdom.textContent None "Constraint Test Application - Resize terminal to test!"
            |> Vdom.bordered

        // Left panel with options that degrade gracefully
        let optionsPanel =
            let option1 =
                Vdom.labelledCheckbox
                    (fun () -> state.FocusedElement <- FocusedElement.Option1)
                    state.FocusedElement.IsOption1
                    (state.SelectedOption = 0)
                    "First Option"

            let option2 =
                Vdom.labelledCheckbox
                    (fun () -> state.FocusedElement <- FocusedElement.Option2)
                    state.FocusedElement.IsOption2
                    (state.SelectedOption = 1)
                    "Second Option"

            let option3 =
                Vdom.labelledCheckbox
                    (fun () -> state.FocusedElement <- FocusedElement.Option3)
                    state.FocusedElement.IsOption3
                    (state.SelectedOption = 2)
                    "Third Option"

            // Stack options vertically
            option3
            |> Vdom.panelSplitProportion Direction.Horizontal 0.33 option2
            |> Vdom.panelSplitProportion Direction.Horizontal 0.5 option1
            |> Vdom.bordered

        // Center panel with long text that needs wrapping
        let contentPanel =
            let content =
                if state.ShowDetails then
                    """This is a long piece of text that will need to wrap when the terminal width is constrained.
It contains multiple lines and should gracefully handle being rendered in a small space.
The constraint solver should ensure that even if this panel gets very small, it will still render something.
Lines will wrap at word boundaries when possible.
If the space gets really tight, we might only see a few characters per line, but it should never crash."""
                else
                    "Toggle 'Show Details' to see more content. This shorter text should fit in most reasonable terminal sizes."

            Vdom.textContent None content |> Vdom.bordered

        // Right panel with nested borders and controls
        let controlPanel =
            let showDetailsToggle =
                Vdom.labelledCheckbox
                    (fun () -> state.FocusedElement <- FocusedElement.ShowDetails)
                    state.FocusedElement.IsShowDetails
                    state.ShowDetails
                    "Show Details"

            let compactModeToggle =
                Vdom.labelledCheckbox
                    (fun () -> state.FocusedElement <- FocusedElement.CompactMode)
                    state.FocusedElement.IsCompactMode
                    state.CompactMode
                    "Compact Mode"

            let debugToggle =
                Vdom.labelledCheckbox
                    (fun () -> state.FocusedElement <- FocusedElement.DebugInfo)
                    state.FocusedElement.IsDebugInfo
                    state.DebugInfo
                    "Debug Info"

            // Nested borders to test constraint propagation
            showDetailsToggle
            |> Vdom.panelSplitProportion Direction.Horizontal 0.33 compactModeToggle
            |> Vdom.panelSplitProportion Direction.Horizontal 0.5 debugToggle
            |> Vdom.bordered
            |> Vdom.bordered // Double border!

        // Status bar at bottom
        let statusBar =
            let statusText =
                if state.DebugInfo then
                    sprintf
                        "Option: %d | Details: %b | Compact: %b | Press TAB to navigate, SPACE to toggle"
                        state.SelectedOption
                        state.ShowDetails
                        state.CompactMode
                else
                    "Press TAB to navigate, SPACE to toggle"

            Vdom.textContent None statusText

        // Build the complete layout
        let mainContent =
            if state.CompactMode then
                // In compact mode, just show options and content side-by-side
                optionsPanel |> Vdom.panelSplitProportion Direction.Vertical 0.3 contentPanel
            else
                // Normal mode with all three panels
                let leftAndCenter =
                    optionsPanel |> Vdom.panelSplitProportion Direction.Vertical 0.25 contentPanel

                Vdom.panelSplitProportion Direction.Vertical 0.8 leftAndCenter controlPanel

        // Combine with header and status
        let mainWindow =
            header |> Vdom.panelSplitProportion Direction.Horizontal 0.5 mainContent

        Vdom.panelSplitAbsolute Direction.Horizontal -1 mainWindow statusBar

    let processInput (changes : WorldStateChange seq) (state : TestState) =
        for change in changes do
            match change with
            | Keystroke key when key.Key = ConsoleKey.Spacebar ->
                match state.FocusedElement with
                | FocusedElement.Option1 -> state.SelectedOption <- 1
                | FocusedElement.Option2 -> state.SelectedOption <- 2
                | FocusedElement.Option3 -> state.SelectedOption <- 3
                | FocusedElement.ShowDetails -> state.ShowDetails <- not state.ShowDetails
                | FocusedElement.CompactMode -> state.CompactMode <- not state.CompactMode
                | FocusedElement.DebugInfo -> state.DebugInfo <- not state.DebugInfo
            | Keystroke key when key.KeyChar = 'd' || key.KeyChar = 'D' -> state.ShowDetails <- not state.ShowDetails
            | Keystroke key when key.KeyChar = 'c' || key.KeyChar = 'C' -> state.CompactMode <- not state.CompactMode
            | Keystroke key when key.KeyChar = 'i' || key.KeyChar = 'I' -> state.DebugInfo <- not state.DebugInfo
            | _ -> ()

    [<EntryPoint>]
    let main argv =

        App.run (TestState.Initial ()) (fun _ -> true) processInput vdom |> _.Wait()

        0
