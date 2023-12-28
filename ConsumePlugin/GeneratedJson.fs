//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------

namespace ConsumePlugin

/// Module containing JSON parsing methods for the InnerType type
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module InnerType =
    /// Parse from a JSON node.
    let jsonParse (node : System.Text.Json.Nodes.JsonNode) : InnerType =
        let Thing = node.[(Literals.something)].AsValue().GetValue<string> ()

        {
            Thing = Thing
        }
namespace ConsumePlugin

/// Module containing JSON parsing methods for the JsonRecordType type
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonRecordType =
    /// Parse from a JSON node.
    let jsonParse (node : System.Text.Json.Nodes.JsonNode) : JsonRecordType =
        let F =
            node.["f"].AsArray ()
            |> Seq.map (fun elt -> elt.AsValue().GetValue<int> ())
            |> Array.ofSeq

        let E =
            node.["e"].AsArray ()
            |> Seq.map (fun elt -> elt.AsValue().GetValue<string> ())
            |> Array.ofSeq

        let D = InnerType.jsonParse node.["d"]

        let C =
            node.["hi"].AsArray ()
            |> Seq.map (fun elt -> elt.AsValue().GetValue<int> ())
            |> List.ofSeq

        let B = node.["another-thing"].AsValue().GetValue<string> ()
        let A = node.["a"].AsValue().GetValue<int> ()

        {
            A = A
            B = B
            C = C
            D = D
            E = E
            F = F
        }