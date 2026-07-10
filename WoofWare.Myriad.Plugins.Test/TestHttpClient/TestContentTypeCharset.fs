namespace WoofWare.Myriad.Plugins.Test

open FsCheck
open FsCheck.FSharp
open Fantomas.FCS.Syntax
open NUnit.Framework
open FsUnitTyped
open WoofWare.Myriad.Plugins
open WoofWare.Whippet.Fantomas

[<TestFixture>]
module TestContentTypeCharset =

    /// The tests below inspect the rewritten constant, so unwrap it.
    let private constStr (e : SynExpr) : string =
        match e with
        | SynExpr.Const (SynConst.String (s, _, _), _) -> s
        | e -> failwith $"expected a string constant, got %+A{e}"

    let private rewrite (s : string) : string =
        HttpClientGenerator.withStringContentCharset (SynExpr.CreateConst s) |> constStr

    [<Test>]
    let ``a Content-Type without a charset declares the UTF-8 we actually send`` () : unit =
        rewrite "application/json" |> shouldEqual "application/json; charset=utf-8"

        rewrite "application/merge-patch+json; profile=custom"
        |> shouldEqual "application/merge-patch+json; profile=custom; charset=utf-8"

    [<TestCase "text/plain; charset=utf-8">]
    [<TestCase "text/plain; charset=UTF-8">]
    [<TestCase "text/plain; charset=utf8">]
    [<TestCase "text/plain;charset=\"utf-8\"">]
    let ``a UTF-8 charset declaration passes through untouched`` (declared : string) : unit =
        rewrite declared |> shouldEqual declared

    [<Test>]
    let ``a quoted parameter containing a semicolon is not mistaken for a charset`` () : unit =
        // Naive splitting on ';' would read a charset out of the quoted profile value.
        rewrite "text/plain; profile=\"a;charset=iso-8859-1\""
        |> shouldEqual "text/plain; profile=\"a;charset=iso-8859-1\"; charset=utf-8"

    [<TestCase "not-a-media-type">]
    [<TestCase "utter garbage">]
    let ``an unparseable Content-Type constant is rejected at generation time`` (declared : string) : unit =
        // Better to fail generation than emit code whose MediaTypeHeaderValue.Parse call
        // throws on every request.
        Assert.Throws<exn> (fun () ->
            HttpClientGenerator.withStringContentCharset (SynExpr.CreateConst declared)
            |> ignore<SynExpr>
        )
        |> ignore<exn>

    [<TestCase "text/plain; charset=iso-8859-1">]
    [<TestCase "application/json; charset=UTF-16">]
    let ``a non-UTF-8 charset is refused rather than mislabelled`` (declared : string) : unit =
        // StringContent bodies are UTF-8-encoded; declaring another charset would label the
        // bytes wrongly, so generation must fail loudly instead.
        let e =
            Assert.Throws<exn> (fun () ->
                HttpClientGenerator.withStringContentCharset (SynExpr.CreateConst declared)
                |> ignore<SynExpr>
            )

        e.Message.Contains "charset" |> shouldEqual true

    [<Test>]
    let ``a computed Content-Type passes through untouched`` () : unit =
        // We can only inspect compile-time constants; anything else is the user's problem.
        // (SynExpr has no structural equality, but pass-through returns the same object.)
        let computed = SynExpr.createIdent "someHeaderValue"

        obj.ReferenceEquals (HttpClientGenerator.withStringContentCharset computed, computed)
        |> shouldEqual true

    [<Test>]
    let ``charset defaulting is idempotent`` () : unit =
        // Applying the rewrite twice must equal applying it once: the first pass pins a
        // charset, which the second pass then honours.
        let mediaTypeGen : Gen<string> =
            let segment =
                Gen.elements [ "application/json" ; "text/plain" ; "application/vnd.api+json" ]

            let param =
                Gen.elements [ "" ; "; profile=custom" ; "; version=1" ; "; charset=utf-8" ]

            Gen.map2 (+) segment param

        let property (mediaType : string) : bool =
            let once = rewrite mediaType
            let twice = rewrite once
            twice = once

        Prop.forAll (Arb.fromGen mediaTypeGen) property |> Check.QuickThrowOnFailure
