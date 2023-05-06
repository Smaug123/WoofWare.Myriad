# fsharp-arguments

Some helpers in [Myriad](https://github.com/MoiraeSoftware/myriad/) which might be useful for someone writing an argument parser.

## `RemoveOptions`

Takes a record like this:

```fsharp
type Foo =
    {
        A : int option
        B : string
        C : float list
    }
```

and stamps out a record like this:

```fsharp
[<RequireQualifiedAccess>]
module Foo =
    type Short =
        {
            A : int
            B : string
            C : float list
        }
```

(This is a proof of concept. It would be better to somehow disambiguate the module name.)
