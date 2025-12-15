namespace WoofWare.Zoomies

open System

type ColorMode =
    | NoColor
    | Color

[<Struct>]
type TerminalCell =
    {
        Char : char
        BackgroundColor : ConsoleColor voption
        TextColor : ConsoleColor voption
    }

    static member OfChar (c : char) =
        {
            Char = c
            BackgroundColor = ValueNone
            TextColor = ValueNone
        }

[<RequireQualifiedAccess>]
module Terminal =

    let clearLine () = Console.Write "\x1B[2K"
    let saveCursorPosition () = Console.Write "\x1B[s"
    let restoreCursorPosition () = Console.Write "\x1B[u"

type TerminalOp =
    | MoveCursor of x : int * y : int
    /// Write a run of characters with uniform styling.
    /// The text should not contain control characters or newlines.
    | WriteRun of text : string * backgroundColor : ConsoleColor voption * textColor : ConsoleColor voption
    | SetCursorVisibility of toVisible : bool
    | ClearScreen
    | EnterAlternateScreen
    | ExitAlternateScreen
    | RegisterMouseMode
    | UnregisterMouseMode
    | RegisterBracketedPaste
    | UnregisterBracketedPaste

[<RequireQualifiedAccess>]
module TerminalOp =
    /// Execute a terminal operation, returning the new current colors.
    /// Colors are only emitted when they differ from the current state.
    let execute
        (colorMode : ColorMode)
        (currentBackground : ConsoleColor)
        (currentForeground : ConsoleColor)
        (consoleWrite : string -> unit)
        (o : TerminalOp)
        : struct (ConsoleColor * ConsoleColor)
        =
        match o with
        | TerminalOp.WriteRun (text, backgroundColor, textColor) ->
            let newBackground =
                match colorMode, backgroundColor with
                | ColorMode.Color, ValueSome bg when bg <> currentBackground ->
                    consoleWrite (ConsoleColor.toBackgroundEscapeCode bg)
                    bg
                | ColorMode.Color, ValueSome bg -> bg
                | _ -> currentBackground

            let newForeground =
                match colorMode, textColor with
                | ColorMode.Color, ValueSome fg when fg <> currentForeground ->
                    consoleWrite (ConsoleColor.toForegroundEscapeCode fg)
                    fg
                | ColorMode.Color, ValueSome fg -> fg
                | _ -> currentForeground

            consoleWrite text
            struct (newBackground, newForeground)
        | TerminalOp.MoveCursor (x, y) ->
            consoleWrite $"\x1B[%d{y + 1};%d{x + 1}H"
            struct (currentBackground, currentForeground)
        | TerminalOp.SetCursorVisibility visible ->
            if visible then
                consoleWrite "\x1B[?25h"
            else
                consoleWrite "\x1B[?25l"

            struct (currentBackground, currentForeground)
        | TerminalOp.ClearScreen ->
            consoleWrite "\x1B[2J"
            struct (currentBackground, currentForeground)
        | TerminalOp.EnterAlternateScreen ->
            consoleWrite "\x1B[?1049h"
            struct (currentBackground, currentForeground)
        | TerminalOp.ExitAlternateScreen ->
            consoleWrite "\x1B[?1049l"
            struct (currentBackground, currentForeground)
        | TerminalOp.RegisterMouseMode ->
            consoleWrite "\u001b[?1000;1006h"
            struct (currentBackground, currentForeground)
        | TerminalOp.UnregisterMouseMode ->
            consoleWrite "\u001b[?1000;1006l"
            struct (currentBackground, currentForeground)
        | TerminalOp.RegisterBracketedPaste ->
            consoleWrite "\u001b[?2004h"
            struct (currentBackground, currentForeground)
        | TerminalOp.UnregisterBracketedPaste ->
            consoleWrite "\u001b[?2004l"
            struct (currentBackground, currentForeground)
