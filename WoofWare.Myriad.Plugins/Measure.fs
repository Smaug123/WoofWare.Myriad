namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open WoofWare.Whippet.Fantomas

[<RequireQualifiedAccess>]
module internal Measure =

    let getLanguagePrimitivesMeasure (typeName : LongIdent) : SynExpr =
        match typeName |> List.map _.idText with
        | [ "System" ; "Single" ] -> [ "LanguagePrimitives" ; "Float32WithMeasure" ]
        | [ "System" ; "Double" ] -> [ "LanguagePrimitives" ; "FloatWithMeasure" ]
        | [ "System" ; "Byte" ] -> [ "LanguagePrimitives" ; "ByteWithMeasure" ]
        | [ "System" ; "SByte" ] -> [ "LanguagePrimitives" ; "SByteWithMeasure" ]
        | [ "System" ; "Int16" ] -> [ "LanguagePrimitives" ; "Int16WithMeasure" ]
        | [ "System" ; "Int32" ] -> [ "LanguagePrimitives" ; "Int32WithMeasure" ]
        | [ "System" ; "Int64" ] -> [ "LanguagePrimitives" ; "Int64WithMeasure" ]
        | [ "System" ; "UInt16" ] -> [ "LanguagePrimitives" ; "UInt16WithMeasure" ]
        | [ "System" ; "UInt32" ] -> [ "LanguagePrimitives" ; "UInt32WithMeasure" ]
        | [ "System" ; "UInt64" ] -> [ "LanguagePrimitives" ; "UInt64WithMeasure" ]
        | l ->
            let l = String.concat "." l
            failwith $"unrecognised type for measure: %s{l}"
        |> SynExpr.createLongIdent
