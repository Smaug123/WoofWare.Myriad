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
        let Thing =
            (match node.[(Literals.something)] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ((Literals.something))
                     )
                 )
             | v -> v)
                .AsValue()
                .GetValue<string> ()

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
            (match node.["f"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("f")
                     )
                 )
             | v -> v)
                .AsArray ()
            |> Seq.map (fun elt -> elt.AsValue().GetValue<int> ())
            |> Array.ofSeq

        let E =
            (match node.["e"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("e")
                     )
                 )
             | v -> v)
                .AsArray ()
            |> Seq.map (fun elt -> elt.AsValue().GetValue<string> ())
            |> Array.ofSeq

        let D = InnerType.jsonParse node.["d"]

        let C =
            (match node.["hi"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("hi")
                     )
                 )
             | v -> v)
                .AsArray ()
            |> Seq.map (fun elt -> elt.AsValue().GetValue<int> ())
            |> List.ofSeq

        let B =
            (match node.["another-thing"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("another-thing")
                     )
                 )
             | v -> v)
                .AsValue()
                .GetValue<string> ()

        let A =
            (match node.["a"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("a")
                     )
                 )
             | v -> v)
                .AsValue()
                .GetValue<int> ()

        {
            A = A
            B = B
            C = C
            D = D
            E = E
            F = F
        }
namespace ConsumePlugin

/// Module containing JSON parsing extension members for the ToGetExtensionMethod type
[<AutoOpen>]
module ToGetExtensionMethodJsonParseExtension =
    ///Extension methods for JSON parsing
    type ToGetExtensionMethod with

        /// Parse from a JSON node.
        static member jsonParse (node : System.Text.Json.Nodes.JsonNode) : ToGetExtensionMethod =
            let Sailor =
                (match node.["sailor"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("sailor")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<float> ()

            let Soldier =
                (match node.["soldier"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("soldier")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<string> ()
                |> System.Uri

            let Tailor =
                (match node.["tailor"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("tailor")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<int> ()

            let Tinker =
                (match node.["tinker"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("tinker")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<string> ()

            {
                Tinker = Tinker
                Tailor = Tailor
                Soldier = Soldier
                Sailor = Sailor
            }
