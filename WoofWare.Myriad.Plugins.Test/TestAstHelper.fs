namespace WoofWare.Myriad.Plugins.Test

open NUnit.Framework
open FsUnitTyped
open Fantomas.FCS.Syntax
open WoofWare.Whippet.Fantomas
open WoofWare.Myriad.Plugins

[<TestFixture>]
module TestAstHelper =

    /// Render an open target as its dotted path, for easy comparison.
    let private renderOpen (target : SynOpenDeclTarget) : string =
        match target with
        | SynOpenDeclTarget.ModuleOrNamespace (SynLongIdent.SynLongIdent (ident, _, _), _) ->
            ident |> List.map _.idText |> String.concat "."
        | SynOpenDeclTarget.Type _ -> failwith "open type not expected in these tests"

    let private opensFor (ns : string) (source : string) : string list =
        let ast = Ast.parse source
        let nsIdent = ns.Split '.' |> Seq.map Ident.create |> List.ofSeq
        AstHelper.extractOpensForNamespace nsIdent ast |> List.map renderOpen

    let private source : string =
        """namespace First

open System.IO

module Helpers =
    let x = 1

namespace First

open Helpers

namespace First.Unrelated

open System.Text

namespace Second

open System.Collections.Generic
"""

    [<Test>]
    let ``Exactly-matching blocks contribute their opens, in file order`` () =
        // Both blocks named First match: same-named blocks share a resolution context.
        opensFor "First" source |> shouldEqual [ "System.IO" ; "Helpers" ]

    [<Test>]
    let ``Opens from unrelated blocks do not leak`` () =
        opensFor "Second" source |> shouldEqual [ "System.Collections.Generic" ]

    [<Test>]
    let ``Matching is by whole name, not by name prefix`` () =
        // A separate top-level block `namespace First` is not lexically around a block
        // `namespace First.Unrelated`, so its opens are not in scope there.
        opensFor "First.Unrelated" source |> shouldEqual [ "System.Text" ]
        opensFor "First.M" source |> shouldEqual []

    // ----------------------------------------------------------------------------------------
    // CataGenerator's type discovery descends into nested modules, so it cannot look opens up by
    // namespace name; it tracks the lexically-enclosing opens during the descent instead.

    let private nestedSource : string =
        """namespace Outer

open System.IO

module Inner =
    open System.Text

    type Tree =
        | Leaf
        | Node of Tree * Tree

namespace Outer.Sibling

open System.Collections.Generic

type Unrelated = { Field : int }
"""

    [<Test>]
    let ``Cata type discovery carries the lexically enclosing opens`` () =
        let groups =
            CataGenerator.groupedTypeDefns (Ast.parse nestedSource)
            |> List.map (fun (ns, opens, types) ->
                ns |> List.map _.idText |> String.concat ".", opens |> List.map renderOpen, types |> List.length
            )

        groups
        |> shouldEqual
            [
                "Outer.Inner", [ "System.IO" ; "System.Text" ], 1
                "Outer.Sibling", [ "System.Collections.Generic" ], 1
            ]
