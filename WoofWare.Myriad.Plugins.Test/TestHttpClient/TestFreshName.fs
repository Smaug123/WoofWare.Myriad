namespace WoofWare.Myriad.Plugins.Test

open FsCheck
open NUnit.Framework
open WoofWare.Myriad.Plugins

/// `HttpClientGenerator.freshName` is what stops a generated binding (e.g. the serialised
/// `queryString`) from capturing a method parameter of the same name. These pin down the two
/// invariants the generator relies on: the allocated name is never one that's already taken,
/// and a name that doesn't collide is returned untouched (so adding a parameter can't silently
/// move the generated output of any endpoint that doesn't actually clash).
[<TestFixture>]
module TestFreshName =

    // FsCheck's default string generator yields null; a taken-name set is never null in practice.
    let private clean (s : string) : string = if isNull s then "" else s

    [<Test>]
    let ``freshName never returns a taken name`` () =
        let property (desired : string) (taken : string list) : bool =
            let desired = clean desired
            let taken = taken |> List.map clean |> Set.ofList
            let result = HttpClientGenerator.freshName desired taken
            not (taken.Contains result)

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``freshName leaves a free name unchanged`` () =
        let property (desired : string) (taken : string list) : bool =
            let desired = clean desired
            let taken = taken |> List.map clean |> Set.ofList

            if taken.Contains desired then
                true // vacuous: this property only speaks to the collision-free case
            else
                HttpClientGenerator.freshName desired taken = desired

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``freshName only ever appends to the desired name`` () =
        // We never mangle the requested name, so the generated identifier stays recognisable.
        let property (desired : string) (taken : string list) : bool =
            let desired = clean desired
            let taken = taken |> List.map clean |> Set.ofList
            (HttpClientGenerator.freshName desired taken).StartsWith desired

        Check.QuickThrowOnFailure property
