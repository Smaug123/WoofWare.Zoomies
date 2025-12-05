# TextBox Widget Design Plan

## Overview

This document describes the design for adding an interactive TextBox widget to WoofWare.Zoomies. The TextBox uses the Activation mechanism to handle text input, cursor movement, and editing operations.

## Design Goals

- **Stateless component**: TextBox renders based on provided state (content + cursor position)
- **Type safety**: All text operations represented as typed discriminated union
- **Composable primitives**: Clean layering with component-specific types in their own files
- **Consistent with existing components**: Follows Button/Checkbox patterns
- **Zero layout jitter**: Cursor rendering maintains constant width across focus states
- **Clear visual feedback**: Focused state is visually distinct

## Key Design Decisions (Post-Review)

This design has been refined based on code review to address:

1. **Layering**: `TextBoxAction` lives in standalone file to keep resolver generic and composable
2. **Shift modifier handling**: Allows uppercase and punctuation (matches existing `textInput`)
3. **Tab handling limitation**: Framework focus intercepts Tab before resolvers see it; tab-insert requires manual-focus mode
4. **Zero layout jitter**: Reserved-width cursor maintains constant width
5. **Focus indication**: Uses `CellStyle.inverted` for focused state (matches other controls)

---

## Architecture Overview

The TextBox follows the established component pattern:

| Concern | Responsibility |
|---------|----------------|
| **Application state** | Stores text content and cursor position |
| **TextBoxAction type** | Standalone DU in its own file (maintains composability) |
| **ActivationResolver** | Transforms keystrokes into typed TextBoxAction events |
| **Component** | Renders text with cursor indicator (purely visual) |
| **Framework** | Handles focus tracking and event routing |

---

## 1. State Management

The application is responsible for tracking TextBox state in its user state:

```fsharp
type State = {
    TextBoxContent: string
    TextBoxCursor: int  // Character index, 0 <= cursor <= content.Length
}
```

**Key principle**: The TextBox component is **stateless and immediate-mode**. It receives state and renders accordingly, just like:
- Button receives `isPressed` and renders accordingly
- Checkbox receives `isChecked` and renders accordingly
- TextBox receives `content` and `cursorPos` and renders accordingly

---

## 2. Event Types

### File: WoofWare.Zoomies/TextBoxAction.fs

Create a standalone file for the TextBoxAction type to maintain clean layering (component-specific types separate from generic resolver primitives):

```fsharp
namespace WoofWare.Zoomies

/// Actions that can be performed on a TextBox.
/// This type is defined standalone to maintain composability:
/// component-specific types stay separate from the generic ActivationResolver module.
type TextBoxAction =
    | InsertChar of char
    | Backspace
    | Delete
    | MoveLeft
    | MoveRight
    | Home
    | End
```

**Why standalone file?**
- Keeps `ActivationResolver.fs` generic and primitive (like Button)
- Component code (`TextBox.fs`) and component types (`TextBoxAction`) stay in the component layer
- Better composability: clear separation between primitives and higher-level abstractions

Application events might look like:

```fsharp
type AppEvent =
    | UsernameAction of TextBoxAction
    | PasswordAction of TextBoxAction
    | SubmitClicked
```

---

## 3. ActivationResolver Extension

### File: WoofWare.Zoomies/ActivationResolver.fs

Extend with a `textBox` function:

```fsharp
[<RequireQualifiedAccess>]
module ActivationResolver =
    // ... existing functions ...

    /// Create a resolver for a text box that handles standard text editing keys.
    ///
    /// IMPORTANT: This resolver is only consulted when framework focus handling is enabled.
    /// Tab is intercepted for focus cycling BEFORE reaching the resolver (see App.fs:131-143).
    ///
    /// If you need tab-insertion while using framework focus, this is currently not supported
    /// without modifying App.fs. Workaround: use manual-focus mode and handle Tab in ProcessWorld.
    ///
    /// In manual-focus mode (haveFrameworkHandleFocus = false), all keystrokes including Tab
    /// are passed directly to ProcessWorld without consulting resolvers.
    let textBox
        (key: NodeKey)
        (makeEvent: TextBoxAction -> 'e)
        : ActivationResolver<'e, 's> =
        ActivationResolver(fun k keystroke _ ->
            if k <> key then None
            else
                match keystroke.Key with
                | ConsoleKey.Backspace when keystroke.Modifiers = ConsoleModifiers.None ->
                    Some (makeEvent Backspace)
                | ConsoleKey.Delete when keystroke.Modifiers = ConsoleModifiers.None ->
                    Some (makeEvent Delete)
                | ConsoleKey.LeftArrow when keystroke.Modifiers = ConsoleModifiers.None ->
                    Some (makeEvent MoveLeft)
                | ConsoleKey.RightArrow when keystroke.Modifiers = ConsoleModifiers.None ->
                    Some (makeEvent MoveRight)
                | ConsoleKey.Home when keystroke.Modifiers = ConsoleModifiers.None ->
                    Some (makeEvent Home)
                | ConsoleKey.End when keystroke.Modifiers = ConsoleModifiers.None ->
                    Some (makeEvent End)
                | _ when keystroke.KeyChar <> '\000' && not (System.Char.IsControl keystroke.KeyChar) ->
                    // Accept any printable character without checking modifiers.
                    // The console gives us the modified character in KeyChar:
                    //   - Shift+A → KeyChar = 'A'
                    //   - Shift+1 → KeyChar = '!'
                    // Reject control characters (Enter, Esc, Tab, Backspace with modifiers, etc) to
                    // avoid inserting them as literal text.
                    Some (makeEvent (InsertChar keystroke.KeyChar))
                | _ -> None
        )
```

### Tab Handling Limitation

**Current behavior:**
- **Framework focus enabled**: Tab is intercepted for focus cycling (App.fs:131-143) *before* resolvers see it
- **Manual focus mode**: Resolvers are not consulted at all; keystrokes go directly to ProcessWorld

**Implication**: There is currently **no way to insert tab characters** while using framework focus and ActivationResolver.

**Workarounds:**
1. Use manual-focus mode (`haveFrameworkHandleFocus = false`) and handle Tab in ProcessWorld
2. Future enhancement: Modify App.fs to run resolvers in both focus modes, add `interceptTab` parameter

**Design philosophy**: This limitation is acceptable for v1 because:
- Tab-for-focus-cycling is the primary use case in forms
- Applications needing tab-insert can use manual-focus mode (drops to low-level control)
- Maintains framework simplicity without complicating the event pipeline

---

## 4. Component Implementation

### File: WoofWare.Zoomies/Components/TextBox.fs

```fsharp
namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

[<RequireQualifiedAccess>]
type TextBox =
    /// <summary>Low-level TextBox rendering without framework integration.</summary>
    /// <remarks>Shows the text content with a cursor indicator at the specified position.</remarks>
    /// <param name="content">The text content to display.</param>
    /// <param name="cursorPos">The cursor position (character index, 0 to content.Length inclusive).</param>
    /// <param name="isFocused">Whether the textbox should render as focused (with cursor visible and inverted style).</param>
    static member make'
        (content: string, cursorPos: int, isFocused: bool)
        : Vdom<DesiredBounds, Unkeyed> =
        // Reserved-width cursor: always allocate space for cursor marker
        // This maintains constant width across focus states (zero layout jitter)
        let displayText, style =
            if cursorPos >= 0 && cursorPos <= content.Length then
                if isFocused then
                    let before = content.Substring(0, cursorPos)
                    let after = content.Substring(cursorPos)
                    // Show cursor as | with inverted style for clear focus indication.
                    // The entire textbox content is inverted while focused (mirrors other controls'
                    // focus styling). If you need a caret-only inversion, render the caret as its
                    // own styled segment instead.
                    ($"{before}|{after}", CellStyle.inverted)
                else
                    // Reserve cursor space at the end (maintains width without visual oddity)
                    ($"{content} ", CellStyle.none)
            else
                // Invalid cursor position: just show content
                (content, CellStyle.none)

        Vdom.styledText (displayText, style)
        |> Vdom.withTag "textbox"

    /// <summary>Framework-integrated TextBox with automatic focus handling.</summary>
    /// <param name="ctx">The VdomContext for checking focus state.</param>
    /// <param name="key">The NodeKey identifying this textbox. You must also register an ActivationResolver for this key.</param>
    /// <param name="content">The current text content.</param>
    /// <param name="cursorPos">The current cursor position.</param>
    /// <param name="isFirstToFocus">
    /// Set to `true` to put this element first in the focus order, when using automatic focus tracking.
    /// </param>
    /// <param name="isInitiallyFocused">
    /// Set to `true` to have this element be focused from the very first tick.
    /// </param>
    /// <remarks>
    /// This component automatically handles focus visual state by consulting the VdomContext.
    /// You must also provide an ActivationResolver to `App.run` to handle text editing events.
    /// </remarks>
    static member make
        (ctx: VdomContext,
         key: NodeKey,
         content: string,
         cursorPos: int,
         ?isFirstToFocus: bool,
         ?isInitiallyFocused: bool)
        : Vdom<DesiredBounds, Unkeyed> =
        let isFocused = VdomContext.focusedKey ctx = Some key
        let textbox = TextBox.make' (content, cursorPos, isFocused) |> Vdom.withKey key
        Vdom.withFocusTracking (textbox, ?isFirstToFocus = isFirstToFocus, ?isInitiallyFocused = isInitiallyFocused)
```

### Cursor Rendering Strategy

**Problem**: Inline cursor markers change text width, causing layout jitter when focus toggles.

**Solution**: Reserved-width rendering with space at the end
- **Focused**: `"He|llo"` with inverted style (6 chars: 5 content + 1 cursor)
- **Unfocused**: `"Hello "` with normal style (6 chars: 5 content + 1 trailing space)

When unfocused, the reserved space moves to the end of the string rather than staying at the cursor position. This avoids the visual oddity of having a space in the middle of the text (e.g., "He llo" looks broken).

**Why this works:**
- Zero layout jitter: width is constant (content.Length + 1 in both states)
- Clear focus indication: inverted style + cursor visible when focused
- No visual oddity when unfocused: trailing space is invisible
- TextContent measures by string length (Layout.fs:596), so constant string length = stable layout
- Matches Button/Toggle pattern of constant width across states

**Alternative considered (rejected)**: Non-inline cursor indicator below text
- Pros: Truly zero width change, clear cursor position
- Cons: Requires multi-line layout (2+ rows minimum), more complex component structure
- Decision: Reserved-width is simpler and "good enough" for v1

---

## 5. Helper Function for State Updates

### File: WoofWare.Zoomies/Components/TextBoxHelpers.fs (or inline in app code)

```fsharp
namespace WoofWare.Zoomies.Components

open WoofWare.Zoomies

[<RequireQualifiedAccess>]
module TextBoxHelpers =
    /// Apply a TextBoxAction to the current content and cursor position.
    /// Returns the new (content, cursorPos) tuple.
    /// Clamps invalid cursor positions to valid bounds before applying the action.
    let applyAction (content: string) (cursor: int) (action: TextBoxAction) : string * int =
        // Clamp cursor to valid range first to prevent Substring exceptions
        let cursor = max 0 (min cursor content.Length)

        match action with
        | InsertChar c ->
            let before = content.Substring(0, cursor)
            let after = content.Substring(cursor)
            (before + string c + after, cursor + 1)

        | Backspace ->
            if cursor > 0 then
                let before = content.Substring(0, cursor - 1)
                let after = content.Substring(cursor)
                (before + after, cursor - 1)
            else
                (content, cursor)

        | Delete ->
            if cursor < content.Length then
                let before = content.Substring(0, cursor)
                let after = content.Substring(cursor + 1)
                (before + after, cursor)
            else
                (content, cursor)

        | MoveLeft -> (content, max 0 (cursor - 1))
        | MoveRight -> (content, min content.Length (cursor + 1))
        | Home -> (content, 0)
        | End -> (content, content.Length)
```

---

## 6. Test Cases

All tests in `WoofWare.Zoomies.Test/TestTextBox.fs`

### Test 1: Tab moves focus in framework focus mode

```fsharp
[<Test>]
let ``tab moves focus from textbox to button when framework focus enabled`` () =
    task {
        // Setup:
        //   - TextBox with isInitiallyFocused=true
        //   - Button next in focus order
        //   - haveFrameworkHandleFocus returns true

        // Action: Press Tab

        // Assert:
        //   - Focus moves to button (framework intercepts Tab)
        //   - Textbox content unchanged
        //   - Textbox no longer shows inverted style
    }
```

### Test 2: All keystrokes pass to ProcessWorld in manual-focus mode

```fsharp
[<Test>]
let ``all keystrokes including tab pass to ProcessWorld when framework focus disabled`` () =
    task {
        // Setup:
        //   - haveFrameworkHandleFocus returns false
        //   - Textbox manually focused
        //   - ProcessWorld records all received keystrokes

        // Action: Press Tab, then 'a', then Backspace

        // Assert:
        //   - ProcessWorld receives all three keystrokes
        //   - ActivationResolver never consulted
        //   - Application can handle Tab manually if desired
    }
```

### Test 3: Basic text editing with uppercase and punctuation

```fsharp
[<Test>]
let ``typing uppercase and punctuation updates text correctly`` () =
    task {
        // Setup: Empty textbox, cursor at 0, framework focus enabled

        // Actions:
        //   - Type 'H' (Shift+h)
        //   - Type 'e'
        //   - Type 'l'
        //   - Type 'l'
        //   - Type 'o'
        //   - Type '!' (Shift+1)

        // Assert after each action:
        //   - Content progresses: "" → "H" → "He" → "Hel" → "Hell" → "Hello" → "Hello!"
        //   - Cursor progresses: 0 → 1 → 2 → 3 → 4 → 5 → 6
        //   - Uppercase and punctuation work (Shift modifier doesn't block)
    }
```

### Test 4: Backspace and Delete

```fsharp
[<Test>]
let ``backspace and delete edit text correctly`` () =
    task {
        // Setup: TextBox with "Hello", cursor at 5 (end)

        // Actions and assertions:
        //   - Press Backspace → content = "Hell", cursor = 4
        //   - Move cursor to 1 (between 'H' and 'e')
        //   - Press Delete → content = "Hll", cursor = 1
        //   - Press Backspace → content = "ll", cursor = 0
        //   - Press Backspace → content = "ll", cursor = 0 (can't backspace past start)
    }
```

### Test 5: Cursor movement

```fsharp
[<Test>]
let ``arrow keys and home end move cursor without changing content`` () =
    task {
        // Setup: TextBox with "Hello", cursor at 5 (end)

        // Actions and asserts:
        //   - Press Left → cursor = 4, content = "Hello"
        //   - Press Left → cursor = 3, content = "Hello"
        //   - Press Home → cursor = 0, content = "Hello"
        //   - Press Right → cursor = 1, content = "Hello"
        //   - Press End → cursor = 5, content = "Hello"
        //   - Press Right → cursor = 5, content = "Hello" (can't move past end)
    }
```

### Test 6: Cursor rendering maintains constant width

```fsharp
[<Test>]
let ``cursor rendering maintains constant width across focus states`` () =
    task {
        // Setup: TextBox with "Hello", cursor at 2

        // Focused state:
        //   - Rendered output: "He|llo" (6 chars: 5 content + 1 cursor)
        //   - Style: inverted

        // Unfocused state:
        //   - Rendered output: "Hello " (6 chars: 5 content + 1 trailing space)
        //   - Style: none

        // Assert: Both states have same measured width (zero layout jitter)
        // Assert: Unfocused state shows text without space in the middle (no visual oddity)
    }
```

### Test 7: Focus visual indication

```fsharp
[<Test>]
let ``focused textbox uses inverted style for clear visual feedback`` () =
    task {
        // Setup: Two textboxes, one focused, one not

        // Assert:
        //   - Focused textbox renders with CellStyle.inverted
        //   - Unfocused textbox renders with CellStyle.none
        //   - Focus state is visually distinct even when cursor is at end
    }
```

---

## 7. Complete Usage Example

```fsharp
module LoginForm =
    open WoofWare.Zoomies
    open WoofWare.Zoomies.Components

    type AppEvent =
        | UsernameEdit of TextBoxAction
        | PasswordEdit of TextBoxAction
        | Submit

    type State = {
        Username: string
        UsernameCursor: int
        Password: string
        PasswordCursor: int
    }

    let usernameKey = NodeKey.make "username"
    let passwordKey = NodeKey.make "password"
    let submitKey = NodeKey.make "submit"

    let resolver =
        ActivationResolver.combine [
            ActivationResolver.textBox usernameKey UsernameEdit
            ActivationResolver.textBox passwordKey PasswordEdit
            ActivationResolver.button submitKey Submit
        ]

    let view (ctx: VdomContext) (state: State) =
        let usernameLabel = Vdom.textContent false "Username:"
        let username =
            TextBox.make (ctx, usernameKey, state.Username, state.UsernameCursor, isFirstToFocus=true)

        let passwordLabel = Vdom.textContent false "Password:"
        let password =
            TextBox.make (ctx, passwordKey, state.Password, state.PasswordCursor)

        let submit = Button.make (ctx, submitKey, "Login")

        // Layout: labels and fields in rows
        Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 1,
            Vdom.panelSplitAbsolute (SplitDirection.Vertical, 10, usernameLabel, username),
            Vdom.panelSplitAbsolute (SplitDirection.Horizontal, 1,
                Vdom.panelSplitAbsolute (SplitDirection.Vertical, 10, passwordLabel, password),
                submit))

    let processWorld =
        { new WorldProcessor<AppEvent, State> with
            member _.ProcessWorld(events, ctx, state) =
                let mutable newState = state

                for evt in events do
                    match evt with
                    | WorldStateChange.ApplicationEvent (UsernameEdit action) ->
                        let content, cursor =
                            TextBoxHelpers.applyAction state.Username state.UsernameCursor action
                        newState <- { newState with Username = content; UsernameCursor = cursor }

                    | WorldStateChange.ApplicationEvent (PasswordEdit action) ->
                        let content, cursor =
                            TextBoxHelpers.applyAction state.Password state.PasswordCursor action
                        newState <- { newState with Password = content; PasswordCursor = cursor }

                    | WorldStateChange.ApplicationEvent Submit ->
                        printfn "Login: %s / %s" newState.Username newState.Password

                    | _ -> ()

                ProcessWorldResult.make newState
        }

    let run () =
        App.run
            System.Environment.GetEnvironmentVariable
            { Username = ""; UsernameCursor = 0; Password = ""; PasswordCursor = 0 }
            (fun _ -> true)  // Enable framework focus handling (Tab cycles focus)
            processWorld
            view
            resolver
```

### Example: Tab Insertion in Manual-Focus Mode

For applications that need tab-insertion, use manual focus mode:

```fsharp
let processWorld =
    { new WorldProcessor<AppEvent, State> with
        member _.ProcessWorld(events, ctx, state) =
            let mutable newState = state

            for evt in events do
                match evt with
                | WorldStateChange.Keystroke k ->
                    match VdomContext.focusedKey ctx with
                    | Some key when key = usernameKey ->
                        // Handle keystrokes manually, including Tab
                        if k.Key = ConsoleKey.Tab then
                            // Insert tab character
                            let content, cursor =
                                TextBoxHelpers.applyAction
                                    state.Username
                                    state.UsernameCursor
                                    (InsertChar '\t')
                            newState <- { newState with Username = content; UsernameCursor = cursor }
                        elif k.KeyChar <> '\000' && not (System.Char.IsControl k.KeyChar) then
                            // Regular character (skip control chars like Enter/Escape here unless
                            // you choose to handle them explicitly)
                            let content, cursor =
                                TextBoxHelpers.applyAction
                                    state.Username
                                    state.UsernameCursor
                                    (InsertChar k.KeyChar)
                            newState <- { newState with Username = content; UsernameCursor = cursor }
                        // ... handle other keys ...

                    | _ -> ()

                | _ -> ()

            ProcessWorldResult.make newState
    }

let run () =
    App.run
        System.Environment.GetEnvironmentVariable
        initialState
        (fun _ -> false)  // Disable framework focus - handle everything manually
        processWorld
        view
        ActivationResolver.none  // No resolver in manual mode
```

---

## 8. Edge Cases and Invariants

### Cursor position bounds

- Must always satisfy: `0 <= cursorPos <= content.Length`
- Insert operations advance cursor by 1
- Delete keeps cursor in same position
- Backspace moves cursor back by 1 (unless at start)
- Movement operations clamp to valid range

### Invalid cursor positions

If the application provides an invalid cursor position, the component should gracefully handle it by showing the content without cursor markers.

### Empty textbox

- Empty content: `content = ""`
- Cursor at start: `cursorPos = 0`
- Render focused: `"|"` with inverted style (1 character)
- Render unfocused: `" "` with normal style (1 character: trailing space)
- Both states have the same width (reserved-width cursor maintains constant layout)

### Special characters

- Tab character (`'\t'`): Currently not insertable with framework focus (see Tab Handling Limitation)
- Newline (`'\n'`): Not handled in single-line textbox; filtered out by `Char.IsControl` in the resolver
- Control characters like Enter (`'\r'`), Escape (`'\u001b'`), Tab (`'\t'`), and Backspace with modifiers
  have non-zero `KeyChar` values; the resolver explicitly excludes all control characters before
  emitting `InsertChar`

---

## 9. Future Enhancements

### Tab insertion with framework focus

Modify `App.processChanges` to:
1. Run ActivationResolver in both framework-focus and manual-focus modes
2. Add `interceptTab` parameter to `textBox` resolver
3. If resolver handles Tab (returns Some), don't advance focus

Benefits: Best of both worlds - framework focus cycling + tab insertion where desired

### Selection and clipboard operations

```fsharp
type TextBoxAction =
    // ... existing cases ...
    | StartSelection
    | ExtendSelectionLeft
    | ExtendSelectionRight
    | SelectAll
    | Cut
    | Copy
    | Paste of string
```

### Multi-line text editing

- Handle newlines in content
- Vertical cursor movement (Up/Down arrows)
- Line wrapping logic
- Vertical scroll support

### Validation and masking

```fsharp
type TextBox =
    static member make
        (ctx: VdomContext,
         key: NodeKey,
         content: string,
         cursorPos: int,
         ?validator: char -> bool,  // Filter input characters
         ?mask: char,                // Display character (e.g., '*' for password)
         ...)
```

### Non-inline cursor (zero jitter v2)

Multi-line layout with separate cursor indicator row:
```
Row 1: "Hello World"  (text content)
Row 2: "     ^      "  (cursor indicator)
```

Pros: Truly zero layout jitter, clear cursor position
Requires: Multi-line component architecture, more complex layout logic

---

## 10. Implementation Order

1. **Create `TextBoxAction.fs`** before `ActivationResolver.fs` in compilation order
   - Defines the action DU
   - Keeps component types separate from primitives (clean layering)

2. **Update `ActivationResolver.fs`**
   - Add `textBox` function
   - Fix Shift modifier handling (don't check modifiers for character input)
   - Document Tab handling limitation

3. **Create `Components/TextBox.fs`**
   - Implement `make'` with reserved-width cursor
   - Implement `make` with framework integration
   - Use `CellStyle.inverted` for focused state

4. **Create `Components/TextBoxHelpers.fs`** (or inline in app code)
   - Implement `applyAction` helper function

5. **Update `.fsproj` compilation order**
   ```xml
   <!-- Core types -->
   <Compile Include="ConsoleModifiers.fs" />
   <Compile Include="Nursery.fs" />
   <Compile Include="Vdom.fs" />

   <!-- TextBox support (before ActivationResolver) -->
   <Compile Include="TextBoxAction.fs" />
   <Compile Include="ActivationResolver.fs" />

   <!-- ... other files ... -->

   <!-- Components -->
   <Compile Include="Components/TextBox.fs" />
   <Compile Include="Components/TextBoxHelpers.fs" />
   ```

6. **Write tests** in `TestTextBox.fs`
   - Test 1: Tab cycles focus
   - Test 2: Manual-focus mode passes all keystrokes to ProcessWorld
   - Test 3: Uppercase and punctuation work
   - Test 4: Backspace and Delete
   - Test 5: Cursor movement
   - Test 6: Zero layout jitter (constant width)
   - Test 7: Focus visual indication

7. **Run formatter**: `dotnet fantomas .`

8. **Run analyzers**: `./analyzers/run.sh`

---

## 11. Files to Create/Modify

### New Files
- `WoofWare.Zoomies/TextBoxAction.fs` - Action DU (standalone for clean layering)
- `WoofWare.Zoomies/Components/TextBox.fs` - Component implementation
- `WoofWare.Zoomies/Components/TextBoxHelpers.fs` - Helper utilities
- `WoofWare.Zoomies.Test/TestTextBox.fs` - Test suite

### Modified Files
- `WoofWare.Zoomies/ActivationResolver.fs` - Add `textBox` function
- `WoofWare.Zoomies/WoofWare.Zoomies.fsproj` - Add new files in correct order
- `WoofWare.Zoomies.Test/WoofWare.Zoomies.Test.fsproj` - Add test file

### Compilation Order

**Critical**: `TextBoxAction.fs` must come before `ActivationResolver.fs`:

```xml
<ItemGroup>
  <!-- Primitives -->
  <Compile Include="ConsoleModifiers.fs" />
  <Compile Include="Nursery.fs" />
  <Compile Include="Vdom.fs" />
  <Compile Include="CtrlCHandler.fs" />
  <Compile Include="ConsoleColor.fs" />
  <Compile Include="Terminal.fs" />
  <Compile Include="Console.fs" />
  <Compile Include="IStopwatch.fs" />
  <Compile Include="CellStyle.fs" />

  <!-- TextBox action type (before resolver) -->
  <Compile Include="TextBoxAction.fs" />
  <Compile Include="ActivationResolver.fs" />

  <Compile Include="WorldFreezer.fs" />
  <Compile Include="Layout.fs" />
  <Compile Include="Render.fs" />

  <!-- Components -->
  <Compile Include="Components/Toggle.fs" />
  <Compile Include="Components/Checkbox.fs" />
  <Compile Include="Components/LabelledCheckbox.fs" />
  <Compile Include="Components/Button.fs" />
  <Compile Include="Components/TextBox.fs" />
  <Compile Include="Components/TextBoxHelpers.fs" />

  <Compile Include="App.fs" />
</ItemGroup>
```

---

## 12. Summary

### Design Principles

1. **Clean layering**: Component-specific types (`TextBoxAction`) in standalone files, separate from generic primitives (`ActivationResolver`)
2. **Zero layout jitter**: Reserved-width cursor maintains constant text length
3. **Clear focus indication**: `CellStyle.inverted` provides visual feedback
4. **Stateless component**: Application owns all state (content + cursor)
5. **Type-safe actions**: `TextBoxAction` DU for all text operations
6. **Composable with framework**: Works seamlessly with automatic focus tracking

### Key Trade-offs

1. **Tab limitation**: Can't insert tabs with framework focus enabled (architectural limitation of App.fs)
   - Workaround: Use manual-focus mode for tab-insertion
   - Future: Modify App.fs to run resolvers in both focus modes

2. **Single-line only**: Multi-line editing requires future enhancement
   - Current design handles horizontal cursor movement only
   - Vertical movement (Up/Down) and line wrapping are out of scope

3. **Reserved-width cursor**: Always allocates 1 extra character
   - Pro: Zero layout jitter
   - Con: TextBox always 1 character wider than content
   - Alternative (future): Multi-line layout with separate cursor row

### Success Criteria

- ✅ Uppercase and punctuation work (Shift modifier handled correctly)
- ✅ Zero layout jitter (constant width across focus states)
- ✅ Clear visual feedback (inverted style when focused)
- ✅ Clean layering (TextBoxAction separate from ActivationResolver)
- ✅ Composable (works with framework focus, can drop to manual mode)
- ✅ Type-safe (TextBoxAction DU, no string parsing)

This design maintains the WoofWare.Zoomies philosophy: **small, composable primitives with perfect compositional properties**, enabling ergonomic high-level libraries while allowing seamless drop-down to low-level customization.
