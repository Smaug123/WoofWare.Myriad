namespace MyriadPlugin.Test

open NUnit.Framework
open MyriadPlugin
open ApiSurface

[<TestFixture>]
module TestSurface =
    let assembly = typeof<RemoveOptionsAttribute>.Assembly

    [<Test>]
    let ``Ensure API surface has not been modified`` () = ApiSurface.assertIdentical assembly

    [<Test ; Explicit>]
    let ``Update API surface`` () =
        ApiSurface.writeAssemblyBaseline assembly

    [<Test>]
    let ``Ensure public API is fully documented`` () =
        DocCoverage.assertFullyDocumented assembly
