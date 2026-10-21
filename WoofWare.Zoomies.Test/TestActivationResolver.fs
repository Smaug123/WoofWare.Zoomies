namespace WoofWare.Zoomies.Test

open System
open NUnit.Framework
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open WoofWare.Zoomies

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestActivationResolver =

    type private TextInputEvent =
        | Char of char
        | Backspace

    let private key = NodeKey.make "text-input"

    let private resolver : ActivationResolver<TextInputEvent, unit> =
        ActivationResolver.textInput key Char Backspace

    let private keystroke (c : char) (consoleKey : ConsoleKey) (modifiers : ConsoleModifiers) =
        ConsoleKeyInfo (
            c,
            consoleKey,
            modifiers.HasFlag ConsoleModifiers.Shift,
            modifiers.HasFlag ConsoleModifiers.Alt,
            modifiers.HasFlag ConsoleModifiers.Control
        )

    [<Test>]
    let ``textInput never inserts a control character`` () =
        let gen =
            gen {
                let! c = Gen.elements [ yield! [ '\001' .. '\031' ] ; '\127' ]
                // Backspace is the one control key with dedicated handling.
                let! consoleKey =
                    Gen.elements (Enum.GetValues<ConsoleKey> () |> Array.filter ((<>) ConsoleKey.Backspace))

                let! modifiers =
                    Gen.elements [ ConsoleModifiers.None ; ConsoleModifiers.Shift ; ConsoleModifiers.Control ]

                return keystroke c consoleKey modifiers
            }

        let prop (k : ConsoleKeyInfo) =
            resolver.Invoke (key, k, ()) |> shouldEqual None

        Check.One (propConfig, Prop.forAll (Arb.fromGen gen) prop)

    [<Test>]
    let ``textInput inserts printable characters`` () =
        let gen =
            gen {
                let! c =
                    ArbMap.defaults
                    |> ArbMap.generate<char>
                    |> Gen.filter (fun c -> not (Char.IsControl c))

                let! consoleKey =
                    Gen.elements (Enum.GetValues<ConsoleKey> () |> Array.filter ((<>) ConsoleKey.Backspace))

                return keystroke c consoleKey ConsoleModifiers.None
            }

        let prop (k : ConsoleKeyInfo) =
            resolver.Invoke (key, k, ()) |> shouldEqual (Some (Char k.KeyChar))

        Check.One (propConfig, Prop.forAll (Arb.fromGen gen) prop)

    [<Test>]
    let ``textInput handles Enter and Escape without inserting them`` () =
        resolver.Invoke (key, keystroke '\r' ConsoleKey.Enter ConsoleModifiers.None, ())
        |> shouldEqual None

        resolver.Invoke (key, keystroke '\027' ConsoleKey.Escape ConsoleModifiers.None, ())
        |> shouldEqual None
