namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text.Range
open Fantomas.FCS.Xml
open Myriad.Core.AstExtensions

[<RequireQualifiedAccess>]
module internal AstHelper =

    let constructRecord (fields : (RecordFieldName * SynExpr option) list) : SynExpr =
        let fields =
            fields
            |> List.map (fun (rfn, synExpr) -> SynExprRecordField (rfn, Some range0, synExpr, None))

        SynExpr.Record (None, None, fields, range0)

    let private createRecordType
        (
            name : Ident,
            repr : SynTypeDefnRepr,
            members : SynMemberDefns,
            xmldoc : PreXmlDoc
        )
        : SynTypeDefn
        =
        let name = SynComponentInfo.Create ([ name ], xmldoc = xmldoc)

        let trivia : SynTypeDefnTrivia =
            {
                LeadingKeyword = SynTypeDefnLeadingKeyword.Type range0
                EqualsRange = Some range0
                WithKeyword = Some range0
            }

        SynTypeDefn (name, repr, members, None, range0, trivia)

    let defineRecordType
        (
            name : Ident,
            fields : SynField seq,
            members : SynMemberDefns option,
            xmldoc : PreXmlDoc option
        )
        : SynTypeDefn
        =
        let repr =
            SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (None, Seq.toList fields, range0), range0)

        createRecordType (name, repr, defaultArg members SynMemberDefns.Empty, defaultArg xmldoc PreXmlDoc.Empty)

    let isOptionIdent (ident : SynLongIdent) : bool =
        match ident.LongIdent with
        | [ i ] when System.String.Equals (i.idText, "option", System.StringComparison.OrdinalIgnoreCase) -> true
        // TODO: consider Microsoft.FSharp.Option or whatever it is
        | _ -> false

    let isListIdent (ident : SynLongIdent) : bool =
        match ident.LongIdent with
        | [ i ] when System.String.Equals (i.idText, "list", System.StringComparison.OrdinalIgnoreCase) -> true
        // TODO: consider FSharpList or whatever it is
        | _ -> false

    let isArrayIdent (ident : SynLongIdent) : bool =
        match ident.LongIdent with
        | [ i ] when
            System.String.Equals (i.idText, "array", System.StringComparison.OrdinalIgnoreCase)
            || System.String.Equals (i.idText, "[]", System.StringComparison.Ordinal)
            ->
            true
        // TODO: consider FSharpList or whatever it is
        | [ i ] ->
            printfn $"Not array: %s{i.idText}"
            false
        | _ -> false

[<AutoOpen>]
module internal SynTypePatterns =
    let (|OptionType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ innerType ], _, _, _, _) when AstHelper.isOptionIdent ident ->
            Some innerType
        | _ -> None

    let (|ListType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ innerType ], _, _, _, _) when AstHelper.isListIdent ident ->
            Some innerType
        | _ -> None

    let (|ArrayType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ innerType ], _, _, _, _) when AstHelper.isArrayIdent ident ->
            Some innerType
        | SynType.Array (1, innerType, _) -> Some innerType
        | _ -> None

    /// Returns the string name of the type.
    let (|PrimitiveType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent with
            | [ i ] -> [ "string" ; "float" ; "int" ; "bool" ] |> List.tryFind (fun s -> s = i.idText)
            | _ -> None
        | _ -> None

    let (|NumberType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent with
            | [ i ] -> [ "string" ; "float" ; "int" ; "bool" ] |> List.tryFind (fun s -> s = i.idText)
            | _ -> None
        | _ -> None

    let (|DateOnly|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.LongIdent (SynLongIdent.SynLongIdent (ident, _, _)) ->
            match ident |> List.map (fun i -> i.idText) with
            | [ "System" ; "DateOnly" ]
            | [ "DateOnly" ] -> Some ()
            | _ -> None
        | _ -> None

    let (|DateTime|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.LongIdent (SynLongIdent.SynLongIdent (ident, _, _)) ->
            match ident |> List.map (fun i -> i.idText) with
            | [ "System" ; "DateTime" ]
            | [ "DateTime" ] -> Some ()
            | _ -> None
        | _ -> None

    let (|Task|_|) (fieldType : SynType) : SynType option =
        match fieldType with
        | SynType.App (SynType.LongIdent (SynLongIdent.SynLongIdent (ident, _, _)), _, args, _, _, _, _) ->
            match ident |> List.map (fun i -> i.idText) with
            | [ "Task" ]
            | [ "Tasks" ; "Task" ]
            | [ "Threading" ; "Tasks" ; "Task" ]
            | [ "System" ; "Threading" ; "Tasks" ; "Task" ] ->
                match args with
                | [ arg ] -> Some arg
                | _ -> failwithf "Expected Task to be applied to exactly one arg, but got: %+A" args
            | _ -> None
        | _ -> None
