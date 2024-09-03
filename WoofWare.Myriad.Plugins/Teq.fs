namespace WoofWare.Myriad.Plugins

// Extracted from https://github.com/G-Research/TypeEquality
// which is Apache-2.0 licenced. See `TeqLicence.txt`.
// We inline this code because Myriad doesn't seem to reliably load package references in the generator.
// I have reformatted a little, and stripped out all the code I don't use.

type internal Teq<'a, 'b> = private Teq of ('a -> 'b) * ('b -> 'a)

[<RequireQualifiedAccess>]
module internal Teq =

    let refl<'a> : Teq<'a, 'a> = Teq (id, id)
    let cast (Teq (f, _)) a = f a

    [<RequireQualifiedAccess>]
    module Cong =
        let believeMe<'a, 'b, 'a2, 'b2> (_ : Teq<'a, 'b>) : Teq<'a2, 'b2> =
            unbox <| (refl : Teq<'a2, 'a2>)
