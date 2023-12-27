namespace MyriadPlugin.Test

open FsCheck
open ConsumePlugin
open NUnit.Framework
open FsUnitTyped

module TestRemoveOptions =

    let shortenProperty (f : RecordType) =
        let g = RecordType.shorten f

        g.B |> shouldEqual f.B
        g.C |> shouldEqual f.C

        match f.A with
        | None -> g.A |> shouldEqual (RecordType.DefaultA ())
        | Some a -> g.A |> shouldEqual a

        true

    [<Test>]
    let ``shorten works`` () =
        Check.QuickThrowOnFailure shortenProperty
