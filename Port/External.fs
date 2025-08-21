namespace WoofWare.Zoomies.Port

/// External dependencies from Jane Street libraries that need to be implemented
module External =
    /// Equivalent to OCaml's Fn.const
    let const' x _ = x

    /// Equivalent to OCaml's Fn.id
    let id x = x

    /// Equivalent to OCaml's phys_same but for F#
    /// Note: Only works correctly for reference types
    let physSame (x : 'a) (y : 'a) = obj.ReferenceEquals (x, y)

/// FakeUnit type for existential type encodings
[<Struct>]
type FakeUnit = FakeUnit

/// Effect type - placeholder for actual effect system
/// In the real Bonsai, this would be more sophisticated
type Effect<'a> = unit -> 'a
