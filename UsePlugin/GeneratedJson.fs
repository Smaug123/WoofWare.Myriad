//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------

namespace UsePlugin

/// Module containing JSON parsing methods for the InnerType type
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module InnerType =
    /// Parse from a JSON node.
    let jsonParse (node : System.Text.Json.Nodes.JsonNode) : InnerType =
        let Thing = node.["Thing"].AsValue().GetValue<string> ()

        {
            Thing = Thing
        }
namespace UsePlugin

/// Module containing JSON parsing methods for the JsonRecordType type
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonRecordType =
    /// Parse from a JSON node.
    let jsonParse (node : System.Text.Json.Nodes.JsonNode) : JsonRecordType =
        let D = InnerType.jsonParse node.["D"]

        let C =
            node.["C"].AsArray () |> Seq.map (fun elt -> elt.GetValue<int> ()) |> List.ofSeq

        let B = node.["B"].AsValue().GetValue<string> ()
        let A = node.["A"].AsValue().GetValue<int> ()

        {
            A = A
            B = B
            C = C
            D = D
        }
