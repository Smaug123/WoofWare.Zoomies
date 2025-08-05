namespace WoofWare.Zoomies.Test

open System
open NUnit.Framework
open WoofWare.Expect
open WoofWare.Zoomies

/// Test application that exercises the constraint system
[<TestFixture>]
module ConstraintTestApp =

    type TestState =
        {
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
            }

    /// Creates a VDOM that will stress-test the constraint system
    let createTestVdom (state : TestState) : Vdom =

        // Header with title - needs reasonable width
        let header =
            Vdom.textContent None "Constraint Test Application - Resize terminal to test!"
            |> Vdom.bordered

        // Left panel with options that degrade gracefully
        let optionsPanel =
            let option1 =
                Vdom.labelledCheckbox
                    (fun () -> state.SelectedOption <- 0)
                    (state.SelectedOption = 0)
                    (state.SelectedOption = 0)
                    "First Option"

            let option2 =
                Vdom.labelledCheckbox
                    (fun () -> state.SelectedOption <- 1)
                    (state.SelectedOption = 1)
                    (state.SelectedOption = 1)
                    "Second Option"

            let option3 =
                Vdom.labelledCheckbox
                    (fun () -> state.SelectedOption <- 2)
                    (state.SelectedOption = 2)
                    (state.SelectedOption = 2)
                    "Third Option"

            // Stack options vertically
            option1
            |> Vdom.panelSplitProportion Direction.Horizontal 0.33 option2
            |> Vdom.panelSplitProportion Direction.Horizontal 0.5 option3
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
                    (fun () -> state.ShowDetails <- not state.ShowDetails)
                    false
                    state.ShowDetails
                    "Show Details"

            let compactModeToggle =
                Vdom.labelledCheckbox
                    (fun () -> state.CompactMode <- not state.CompactMode)
                    false
                    state.CompactMode
                    "Compact Mode"

            let debugToggle =
                Vdom.labelledCheckbox
                    (fun () -> state.DebugInfo <- not state.DebugInfo)
                    false
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

                leftAndCenter |> Vdom.panelSplitProportion Direction.Vertical 0.7 controlPanel

        // Combine with header and status
        header
        |> Vdom.panelSplitAbsolute Direction.Horizontal 3 mainContent
        |> Vdom.panelSplitAbsolute Direction.Horizontal -1 statusBar

    /// Process keyboard input
    let processInput (changes : WorldStateChange seq) (state : TestState) =
        for change in changes do
            match change with
            | Keystroke key when key.Key = ConsoleKey.D1 -> state.SelectedOption <- 0
            | Keystroke key when key.Key = ConsoleKey.D2 -> state.SelectedOption <- 1
            | Keystroke key when key.Key = ConsoleKey.D3 -> state.SelectedOption <- 2
            | Keystroke key when key.KeyChar = 'd' || key.KeyChar = 'D' -> state.ShowDetails <- not state.ShowDetails
            | Keystroke key when key.KeyChar = 'c' || key.KeyChar = 'C' -> state.CompactMode <- not state.CompactMode
            | Keystroke key when key.KeyChar = 'i' || key.KeyChar = 'I' -> state.DebugInfo <- not state.DebugInfo
            | _ -> ()

    [<Test>]
    let ``Standard terminal`` () =
        let console, harness = ConsoleHarness.make' (fun () -> 80) (fun () -> 24)

        let keyAvailable, readKey, sendKey = WorldFreezerInputs.make ()

        use worldFreezer = WorldFreezer.listen' keyAvailable readKey

        let state = TestState.Initial ()

        let renderState = RenderState.make' console

        App.pumpOnce worldFreezer state (fun _ -> true) renderState processInput createTestVdom

        expect' {
            snapshot
                @"
Option: 0 | Details: false | Compact: false | Press TAB to navigate, SPACE to to|
ggle                                                                            |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
                                                                                |
┌──────────────────────────────────────────────────────┐┌────┐┌────────────────┐|
│┌────────────────────────────────────────────────────┐││Togg││   Third Option │|
││ ☑ Debug Info                                       │││le '││ ☐              │|
││ ☐ Compact Mode                                     │││Show││ ☐ Second Option│|
││ ☐ Show Details                                     │││ Det││   First Option │|
│└────────────────────────────────────────────────────┘││ails││[☑]             │|
└──────────────────────────────────────────────────────┘└────┘└────────────────┘|
┌──────────────────────────────────────────────────────────────────────────────┐|
│Constraint Test Application - Resize terminal to test!                        │|
└──────────────────────────────────────────────────────────────────────────────┘|
"

            return ConsoleHarness.toString harness
        }

(*
/// Example test harness for different terminal sizes
module ConstraintTestHarness =
    open ConstraintTestApp

    let testSizes = [
        (80, 24), "Standard terminal"
        (120, 40), "Large terminal"
        (60, 20), "Small terminal"
        (40, 15), "Tiny terminal"
        (30, 10), "Micro terminal"
        (20, 8), "Absolutely minimal"
        (200, 50), "Ultra-wide"
        (15, 50), "Tall and narrow"
    ]

    /// Run the test with a specific size
    let runTest (width: int, height: int) (description: string) =
        printfn "\n=== Testing %s (%dx%d) ===" description width height

        // Create a fake console with the specified size
        let console =
            { IConsole.defaultForTests with
                WindowWidth = fun () -> width
                WindowHeight = fun () -> height
                Execute = fun op -> () // Could collect and display
            }

        let state = TestState.Initial
        let renderState = RenderState.make' console
        let vdom = createTestVdom state

        // Create buffer
        let buffer = Array2D.zeroCreate height width
        let bounds =
            {
                TopLeftX = 0
                TopLeftY = 0
                Width = width
                Height = height
            }

        // Try to layout with constraints
        match Render.layoutWithConstraints buffer None bounds vdom with
        | Ok rendered ->
            printfn "✓ Layout successful"
            // Could print the buffer contents here if desired
        | Error reason ->
            printfn "✗ Layout failed: %s" reason

    /// Run all test sizes
    let runAllTests () =
        printfn "Running constraint system tests..."
        testSizes |> List.iter (fun (size, desc) -> runTest size desc)


*)
