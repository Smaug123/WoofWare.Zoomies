namespace WoofWare.Zoomies

/// Visual styling for a terminal cell.
[<Struct>]
type CellStyle =
    {
        /// Foreground (text) colour. Default means terminal default.
        Foreground : Color
        /// Background colour. Default means terminal default.
        Background : Color
        /// Whether the text is bold.
        Bold : bool
    }

[<RequireQualifiedAccess>]
module CellStyle =
    /// No explicit styling; uses terminal defaults.
    let none : CellStyle =
        {
            Foreground = Color.Default
            Background = Color.Default
            Bold = false
        }

    /// Inverted colours (light text on dark background or vice versa).
    /// Note: actual effect depends on terminal configuration.
    let inverted : CellStyle =
        {
            Foreground = Color.Palette 0uy // Black
            Background = Color.Palette 15uy // White
            Bold = false
        }

    /// Set foreground colour only.
    let withForeground (c : Color) (s : CellStyle) : CellStyle =
        { s with
            Foreground = c
        }

    /// Set background colour only.
    let withBackground (c : Color) (s : CellStyle) : CellStyle =
        { s with
            Background = c
        }

    /// Set bold attribute.
    let withBold (bold : bool) (s : CellStyle) : CellStyle =
        { s with
            Bold = bold
        }
