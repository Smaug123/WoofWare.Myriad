namespace WoofWare.Myriad.Plugins.Test

open NUnit.Framework
open WoofWare.Myriad.Plugins
open ApiSurface

[<TestFixture>]
module TestSurface =
    let assembly = typeof<RemoveOptionsGenerator>.Assembly

    [<Test>]
    let ``Ensure API surface has not been modified`` () = ApiSurface.assertIdentical assembly

    [<Test>]
    // https://github.com/nunit/nunit3-vs-adapter/issues/876
    let CheckVersionAgainstRemote () =
        MonotonicVersion.validate assembly "WoofWare.Myriad.Plugins"

    [<Test ; Explicit>]
    let ``Update API surface`` () =
        ApiSurface.writeAssemblyBaseline assembly

    [<Test>]
    let ``Ensure public API is fully documented`` () =
        DocCoverage.assertFullyDocumented assembly
