namespace WoofWare.Zoomies

open System

// Microsoft, probably: "Don't worry your pretty little head about this, Microsoft's gonna take care of you"

[<RequireQualifiedAccess>]
module ConsoleColor =

    // https://github.com/dotnet/runtime/blob/5ed4079b4d9db02fbf137c898c08b3bc205d9849/src/libraries/Microsoft.Extensions.Logging.Console/src/AnsiParser.cs#L133
    // if only they saw fit to make this public
    let toForegroundEscapeCode (c : ConsoleColor) : string =
        match c with
        | ConsoleColor.Black -> "\e[30m"
        | ConsoleColor.DarkRed -> "\e[31m"
        | ConsoleColor.DarkGreen -> "\e[32m"
        | ConsoleColor.DarkYellow -> "\e[33m"
        | ConsoleColor.DarkBlue -> "\e[34m"
        | ConsoleColor.DarkMagenta -> "\e[35m"
        | ConsoleColor.DarkCyan -> "\e[36m"
        | ConsoleColor.Gray -> "\e[37m"
        | ConsoleColor.Red -> "\e[1m\e[31m"
        | ConsoleColor.Green -> "\e[1m\e[32m"
        | ConsoleColor.Yellow -> "\e[1m\e[33m"
        | ConsoleColor.Blue -> "\e[1m\e[34m"
        | ConsoleColor.Magenta -> "\e[1m\e[35m"
        | ConsoleColor.Cyan -> "\e[1m\e[36m"
        | ConsoleColor.White -> "\e[1m\e[37m"
        | _ -> "\e[39m\e[22m" // default foreground

    let toBackgroundEscapeCode (c : ConsoleColor) : string =
        match c with
        | ConsoleColor.Black -> "\e[40m"
        | ConsoleColor.DarkRed -> "\e[41m"
        | ConsoleColor.DarkGreen -> "\e[42m"
        | ConsoleColor.DarkYellow -> "\e[43m"
        | ConsoleColor.DarkBlue -> "\e[44m"
        | ConsoleColor.DarkMagenta -> "\e[45m"
        | ConsoleColor.DarkCyan -> "\e[46m"
        | ConsoleColor.Gray -> "\e[47m"
        | _ -> "\e[49m" // default background
