namespace WoofWare.Zoomies

open System

/// Represents a terminal color.
/// This type supports the full range of terminal color capabilities:
/// - Default: use the terminal's default color (SGR 39 for foreground, SGR 49 for background)
/// - Palette: 0-255 indexed colors (0-15 are standard ANSI, 16-255 are extended)
/// - Rgb: 24-bit true color
[<Struct>]
type Color =
    /// Terminal default color (SGR 39 for foreground, SGR 49 for background).
    | Default
    /// Indexed palette color (0-15 = standard ANSI 16 colors, 16-255 = extended palette).
    | Palette of index : byte
    /// 24-bit true color (RGB).
    | Rgb of r : byte * g : byte * b : byte

[<RequireQualifiedAccess>]
module Color =

    /// Convert a System.ConsoleColor to a Color.
    /// Maps to the standard ANSI palette indices 0-15.
    let fromConsoleColor (c : ConsoleColor) : Color =
        match c with
        | ConsoleColor.Black -> Palette 0uy
        | ConsoleColor.DarkRed -> Palette 1uy
        | ConsoleColor.DarkGreen -> Palette 2uy
        | ConsoleColor.DarkYellow -> Palette 3uy
        | ConsoleColor.DarkBlue -> Palette 4uy
        | ConsoleColor.DarkMagenta -> Palette 5uy
        | ConsoleColor.DarkCyan -> Palette 6uy
        | ConsoleColor.Gray -> Palette 7uy
        | ConsoleColor.DarkGray -> Palette 8uy
        | ConsoleColor.Red -> Palette 9uy
        | ConsoleColor.Green -> Palette 10uy
        | ConsoleColor.Yellow -> Palette 11uy
        | ConsoleColor.Blue -> Palette 12uy
        | ConsoleColor.Magenta -> Palette 13uy
        | ConsoleColor.Cyan -> Palette 14uy
        | ConsoleColor.White -> Palette 15uy
        | _ -> Default

    /// Create an RGB color.
    let rgb (r : byte) (g : byte) (b : byte) : Color = Rgb (r, g, b)

    /// Create a palette color.
    let palette (index : byte) : Color = Palette index

    /// Get the ANSI escape code for setting this color as the foreground.
    let toForegroundEscapeCode (c : Color) : string =
        match c with
        | Default -> "\u001b[39m"
        | Palette index -> $"\u001b[38;5;%d{int index}m"
        | Rgb (r, g, b) -> $"\u001b[38;2;%d{int r};%d{int g};%d{int b}m"

    /// Get the ANSI escape code for setting this color as the background.
    let toBackgroundEscapeCode (c : Color) : string =
        match c with
        | Default -> "\u001b[49m"
        | Palette index -> $"\u001b[48;5;%d{int index}m"
        | Rgb (r, g, b) -> $"\u001b[48;2;%d{int r};%d{int g};%d{int b}m"
