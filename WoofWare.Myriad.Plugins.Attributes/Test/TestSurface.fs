namespace WoofWare.Myriad.Plugins.Attributes.Test

open NUnit.Framework
open WoofWare.Myriad.Plugins
open ApiSurface

[<TestFixture>]
module TestSurface =
    let assembly = typeof<RemoveOptionsAttribute>.Assembly

    [<Test>]
    let ``Ensure API surface has not been modified`` () = ApiSurface.assertIdentical assembly

    [<Test>]
    let ``Check version against remote`` () =
        MonotonicVersion.validate assembly "WoofWare.Myriad.Plugins.Attributes"

    [<Test ; Explicit>]
    let ``Update API surface`` () =
        ApiSurface.writeAssemblyBaseline assembly

    [<Test>]
    let ``Ensure public API is fully documented`` () =
        DocCoverage.assertFullyDocumented assembly
