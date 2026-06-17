# Checkbox Toggle Demo
# Demonstrates keyboard navigation and toggling of checkboxes

Output output/demos/checkbox-toggle.gif

Set Shell "bash"
Set FontSize 16
Set Width 600
Set Height 300
Set Padding 10

# Launch the demo TUI
Hide
Type "dotnet run --project WoofWare.Zoomies.Demos -c Release -- checkbox-toggle"
Enter
Sleep 1s
Show

# Initial state: first checkbox is focused (Option A unchecked, Option B checked)
Sleep 500ms

# Toggle the first checkbox (Option A)
Space
Sleep 500ms

# Tab to the second checkbox
Tab
Sleep 500ms

# Toggle the second checkbox (Option B)
Space
Sleep 500ms

# Tab back to first
Tab
Sleep 500ms

# Toggle it again
Space
Sleep 500ms

# Final pause before exit
Sleep 1s

# Exit the TUI
Ctrl+C
