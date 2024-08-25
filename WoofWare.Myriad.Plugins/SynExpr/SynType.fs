namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal SynType =
    let rec stripOptionalParen (ty : SynType) : SynType =
        match ty with
        | SynType.Paren (ty, _) -> stripOptionalParen ty
        | ty -> ty

    let inline createLongIdent (ident : LongIdent) : SynType =
        SynType.LongIdent (SynLongIdent.create ident)

    let inline createLongIdent' (ident : string list) : SynType =
        SynType.LongIdent (SynLongIdent.createS' ident)

    let inline named (name : string) = createLongIdent' [ name ]

    let inline app' (name : SynType) (args : SynType list) : SynType =
        if args.IsEmpty then
            failwith "Type cannot be applied to no arguments"

        SynType.App (name, Some range0, args, List.replicate (args.Length - 1) range0, Some range0, false, range0)

    let inline app (name : string) (args : SynType list) : SynType = app' (named name) args

    let inline appPostfix (name : string) (arg : SynType) : SynType =
        SynType.App (named name, None, [ arg ], [], None, true, range0)

    let inline appPostfix' (name : string list) (arg : SynType) : SynType =
        SynType.App (createLongIdent' name, None, [ arg ], [], None, true, range0)

    let inline funFromDomain (domain : SynType) (range : SynType) : SynType =
        SynType.Fun (
            domain,
            range,
            range0,
            {
                ArrowRange = range0
            }
        )

    let inline signatureParamOfType (ty : SynType) (name : Ident option) : SynType =
        SynType.SignatureParameter ([], false, name, ty, range0)

    let inline var (ty : SynTypar) : SynType = SynType.Var (ty, range0)

    let unit : SynType = named "unit"
    let int : SynType = named "int"

    let string : SynType = named "string"

    /// Given ['a1, 'a2] and 'ret, returns 'a1 -> 'a2 -> 'ret.
    let toFun (inputs : SynType list) (ret : SynType) : SynType =
        (ret, List.rev inputs) ||> List.fold (fun ty input -> funFromDomain input ty)

[<AutoOpen>]
module internal SynTypePatterns =
    let (|OptionType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ innerType ], _, _, _, _) when SynLongIdent.isOption ident ->
            Some innerType
        | _ -> None

    let (|NullableType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ innerType ], _, _, _, _) when SynLongIdent.isNullable ident ->
            Some innerType
        | _ -> None

    let (|UnitType|_|) (fieldType : SynType) : unit option =
        match fieldType with
        | SynType.LongIdent ident when SynLongIdent.isUnit ident -> Some ()
        | _ -> None

    let (|ListType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ innerType ], _, _, _, _) when SynLongIdent.isList ident ->
            Some innerType
        | _ -> None

    let (|ArrayType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ innerType ], _, _, _, _) when SynLongIdent.isArray ident ->
            Some innerType
        | SynType.Array (1, innerType, _) -> Some innerType
        | _ -> None

    let (|RestEaseResponseType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ innerType ], _, _, _, _) when SynLongIdent.isResponse ident ->
            Some innerType
        | _ -> None

    let (|DictionaryType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ key ; value ], _, _, _, _) when SynLongIdent.isDictionary ident ->
            Some (key, value)
        | _ -> None

    let (|IDictionaryType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ key ; value ], _, _, _, _) when SynLongIdent.isIDictionary ident ->
            Some (key, value)
        | _ -> None

    let (|IReadOnlyDictionaryType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ key ; value ], _, _, _, _) when
            SynLongIdent.isReadOnlyDictionary ident
            ->
            Some (key, value)
        | _ -> None

    let (|MapType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ key ; value ], _, _, _, _) when SynLongIdent.isMap ident ->
            Some (key, value)
        | _ -> None

    let (|BigInt|_|) (fieldType : SynType) : unit option =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent |> List.map _.idText with
            | [ "bigint" ]
            | [ "BigInteger" ]
            | [ "Numerics" ; "BigInteger" ]
            | [ "System" ; "Numerics" ; "BigInteger" ] -> Some ()
            | _ -> None
        | _ -> None

    /// Returns the type, qualified as in e.g. `System.Boolean`.
    let (|PrimitiveType|_|) (fieldType : SynType) : LongIdent option =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent with
            | [ i ] -> Primitives.qualifyType i.idText
            | _ -> None
        | _ -> None

    let (|String|_|) (fieldType : SynType) : unit option =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent with
            | [ i ] ->
                [ "string" ]
                |> List.tryFind (fun s -> s = i.idText)
                |> Option.map ignore<string>
            | _ -> None
        | _ -> None

    let (|Byte|_|) (fieldType : SynType) : unit option =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent with
            | [ i ] -> [ "byte" ] |> List.tryFind (fun s -> s = i.idText) |> Option.map ignore<string>
            | _ -> None
        | _ -> None

    let (|Guid|_|) (fieldType : SynType) : unit option =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent |> List.map (fun i -> i.idText) with
            | [ "System" ; "Guid" ]
            | [ "Guid" ] -> Some ()
            | _ -> None
        | _ -> None

    let (|HttpResponseMessage|_|) (fieldType : SynType) : unit option =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent |> List.map (fun i -> i.idText) with
            | [ "System" ; "Net" ; "Http" ; "HttpResponseMessage" ]
            | [ "Net" ; "Http" ; "HttpResponseMessage" ]
            | [ "Http" ; "HttpResponseMessage" ]
            | [ "HttpResponseMessage" ] -> Some ()
            | _ -> None
        | _ -> None

    let (|HttpContent|_|) (fieldType : SynType) : unit option =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent |> List.map (fun i -> i.idText) with
            | [ "System" ; "Net" ; "Http" ; "HttpContent" ]
            | [ "Net" ; "Http" ; "HttpContent" ]
            | [ "Http" ; "HttpContent" ]
            | [ "HttpContent" ] -> Some ()
            | _ -> None
        | _ -> None

    let (|Stream|_|) (fieldType : SynType) : unit option =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent |> List.map (fun i -> i.idText) with
            | [ "System" ; "IO" ; "Stream" ]
            | [ "IO" ; "Stream" ]
            | [ "Stream" ] -> Some ()
            | _ -> None
        | _ -> None

    let (|NumberType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent with
            | [ i ] ->
                // We won't bother with the case that the user has done e.g. `Single` (relying on `System` being open).
                match Primitives.qualifyType i.idText with
                | Some qualified ->
                    match i.idText with
                    | "char"
                    | "string" -> None
                    | _ -> Some qualified
                | None -> None
            | _ -> None
        | _ -> None

    /// Returns the name of the measure, and the outer type.
    let (|Measure|_|) (fieldType : SynType) : (Ident * LongIdent) option =
        match fieldType with
        | SynType.App (NumberType outer,
                       _,
                       [ SynType.LongIdent (SynLongIdent.SynLongIdent ([ ident ], _, _)) ],
                       _,
                       _,
                       _,
                       _) -> Some (ident, outer)
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

    let (|DateTimeOffset|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.LongIdent (SynLongIdent.SynLongIdent (ident, _, _)) ->
            match ident |> List.map (fun i -> i.idText) with
            | [ "System" ; "DateTimeOffset" ]
            | [ "DateTimeOffset" ] -> Some ()
            | _ -> None
        | _ -> None

    let (|Uri|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.LongIdent (SynLongIdent.SynLongIdent (ident, _, _)) ->
            match ident |> List.map (fun i -> i.idText) with
            | [ "System" ; "Uri" ]
            | [ "Uri" ] -> Some ()
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

    let (|DirectoryInfo|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.LongIdent (SynLongIdent.SynLongIdent (ident, _, _)) ->
            match ident |> List.map (fun i -> i.idText) with
            | [ "System" ; "IO" ; "DirectoryInfo" ]
            | [ "IO" ; "DirectoryInfo" ]
            | [ "DirectoryInfo" ] -> Some ()
            | _ -> None
        | _ -> None

    let (|FileInfo|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.LongIdent (SynLongIdent.SynLongIdent (ident, _, _)) ->
            match ident |> List.map (fun i -> i.idText) with
            | [ "System" ; "IO" ; "FileInfo" ]
            | [ "IO" ; "FileInfo" ]
            | [ "FileInfo" ] -> Some ()
            | _ -> None
        | _ -> None
