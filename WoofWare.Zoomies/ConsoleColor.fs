namespace WoofWare.Zoomies

open System

// Microsoft, probably: "Don't worry your pretty little head about this, Microsoft's gonna take care of you"

[<RequireQualifiedAccess>]
module ConsoleColor =

    // https://github.com/dotnet/runtime/blob/5ed4079b4d9db02fbf137c898c08b3bc205d9849/src/libraries/Microsoft.Extensions.Logging.Console/src/AnsiParser.cs#L133
    // if only they saw fit to make this public
    let toForegroundEscapeCode (c : ConsoleColor) : string =
        match c with
        | ConsoleColor.Black -> "\u001b[30m"
        | ConsoleColor.DarkRed -> "\u001b[31m"
        | ConsoleColor.DarkGreen -> "\u001b[32m"
        | ConsoleColor.DarkYellow -> "\u001b[33m"
        | ConsoleColor.DarkBlue -> "\u001b[34m"
        | ConsoleColor.DarkMagenta -> "\u001b[35m"
        | ConsoleColor.DarkCyan -> "\u001b[36m"
        | ConsoleColor.Gray -> "\u001b[37m"
        | ConsoleColor.Red -> "\u001b[1m\u001b[31m"
        | ConsoleColor.Green -> "\u001b[1m\u001b[32m"
        | ConsoleColor.Yellow -> "\u001b[1m\u001b[33m"
        | ConsoleColor.Blue -> "\u001b[1m\u001b[34m"
        | ConsoleColor.Magenta -> "\u001b[1m\u001b[35m"
        | ConsoleColor.Cyan -> "\u001b[1m\u001b[36m"
        | ConsoleColor.White -> "\u001b[1m\u001b[37m"
        | _ -> "\u001b[39m\u001b[22m" // default foreground

    let toBackgroundEscapeCode (c : ConsoleColor) : string =
        match c with
        | ConsoleColor.Black -> "\u001b[40m"
        | ConsoleColor.DarkRed -> "\u001b[41m"
        | ConsoleColor.DarkGreen -> "\u001b[42m"
        | ConsoleColor.DarkYellow -> "\u001b[43m"
        | ConsoleColor.DarkBlue -> "\u001b[44m"
        | ConsoleColor.DarkMagenta -> "\u001b[45m"
        | ConsoleColor.DarkCyan -> "\u001b[46m"
        | ConsoleColor.Gray -> "\u001b[47m"
        | _ -> "\u001b[49m" // default background
