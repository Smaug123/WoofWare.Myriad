//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------
namespace ConsumePlugin

/// Module containing an option-truncated version of the RecordType type
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RecordType =
    /// My whatnot
    type Short =
        {
            /// A thing!
            A: int
            /// Another thing!
            B: string
            /// Yet another thing!
            C: float list
        }

    /// Remove the optional members of the input.
    let shorten (input: RecordType) : Short =
        { A = input.A |> Option.defaultWith RecordType.DefaultA
          B = input.B
          C = input.C }






