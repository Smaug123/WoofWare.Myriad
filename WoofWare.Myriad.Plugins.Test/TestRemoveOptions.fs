namespace WoofWare.Myriad.Plugins.Test

open FsCheck
open ConsumePlugin
open NUnit.Framework
open FsUnitTyped

module TestRemoveOptions =

    /// `` ``d thing`` `` and `` ``e thing`` `` have names which need backticks. The generator
    /// synthesizes the `Default`-prefixed member name it calls for the optional one, so that name
    /// has to be re-backticked on the way out; the required one is here to pin the sites which reuse
    /// the user's own `Ident` (the field declaration, the accessor, and the record label), which
    /// were already correct and must stay so.
    let shortenProperty (f : RecordType) =
        let g = RecordType.shorten f

        g.B |> shouldEqual f.B
        g.C |> shouldEqual f.C
        g.``e thing`` |> shouldEqual f.``e thing``

        match f.A with
        | None -> g.A |> shouldEqual (RecordType.DefaultA ())
        | Some a -> g.A |> shouldEqual a

        match f.``d thing`` with
        | None -> g.``d thing`` |> shouldEqual (RecordType.``Defaultd thing`` ())
        | Some d -> g.``d thing`` |> shouldEqual d

        true

    [<Test>]
    let ``shorten works`` () =
        Check.QuickThrowOnFailure shortenProperty
