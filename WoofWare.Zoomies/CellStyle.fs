namespace WoofWare.Zoomies

open System

/// Visual styling for a terminal cell.
[<Struct>]
type CellStyle =
    {
        /// Foreground (text) colour. None means terminal default.
        Foreground : ConsoleColor voption
        /// Background colour. None means terminal default.
        Background : ConsoleColor voption
    }

[<RequireQualifiedAccess>]
module CellStyle =
    /// No explicit styling; uses terminal defaults.
    let none : CellStyle =
        {
            Foreground = ValueNone
            Background = ValueNone
        }

    /// Inverted colours (light text on dark background or vice versa).
    /// Note: actual effect depends on terminal configuration.
    let inverted : CellStyle =
        {
            Foreground = ValueSome ConsoleColor.Black
            Background = ValueSome ConsoleColor.White
        }

    /// Set foreground colour only.
    let withForeground (c : ConsoleColor) (s : CellStyle) : CellStyle =
        { s with
            Foreground = ValueSome c
        }

    /// Set background colour only.
    let withBackground (c : ConsoleColor) (s : CellStyle) : CellStyle =
        { s with
            Background = ValueSome c
        }
