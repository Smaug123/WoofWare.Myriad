namespace WoofWare.Myriad.Plugins

open System
open System.Collections.Generic
open System.Text
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range
open TypeEquality
open WoofWare.Myriad.Plugins
open WoofWare.Whippet.Fantomas

type internal ArgParserOutputSpec =
    {
        ExtensionMethods : bool
    }

type internal FlagDu =
    {
        Name : Ident
        Case1Name : Ident
        Case2Name : Ident
        /// Hopefully this is simply the const bool True or False, but it might e.g. be a literal
        Case1Arg : SynExpr
        /// Hopefully this is simply the const bool True or False, but it might e.g. be a literal
        Case2Arg : SynExpr
    }

    static member FromBoolean (flagDu : FlagDu) (value : SynExpr) =
        SynExpr.ifThenElse
            (SynExpr.equals value flagDu.Case1Arg)
            (SynExpr.createLongIdent' [ flagDu.Name ; flagDu.Case2Name ])
            (SynExpr.createLongIdent' [ flagDu.Name ; flagDu.Case1Name ])

/// The default value of an argument which admits default values can be pulled from different sources.
/// This defines which source a particular default value comes from.
type internal ArgumentDefaultSpec =
    /// From parsing the environment variable with the given name (e.g. "WOOFWARE_DISABLE_FOO" or whatever).
    | EnvironmentVariable of name : SynExpr
    /// From calling the static member `{typeWeParseInto}.Default{name}()`
    /// For example, if `type MyArgs = { Thing : Choice<int, int> }`, then
    /// we would use `MyArgs.DefaultThing () : int`.
    | FunctionCall of name : Ident

type internal Accumulation<'choice> =
    | Required
    | Optional
    | Choice of 'choice
    | ChoicePositional of attrContents : SynExpr option
    | List of Accumulation<'choice>

type private ParseFunction<'acc> =
    {
        FieldName : Ident
        TargetVariable : Ident
        /// Any of the forms in this set are acceptable, but make sure they all start with a dash, or we might
        /// get confused with positional args or something! I haven't thought that hard about this.
        /// In the default case, this is `Const("arg-name")` for the `ArgName : blah` field; note that we have
        /// omitted the initial `--` that will be required at runtime.
        ArgForm : SynExpr list
        /// If this is a boolean-like field (e.g. a bool or a flag DU), the help text should look a bit different:
        /// we should lie to the user about the value of the cases there.
        /// Similarly, if we're reading from an environment variable with the laxer parsing rules of accepting e.g.
        /// "0" instead of "false", we need to know if we're reading a bool.
        /// In that case, `boolCases` is Some, and contains the construction of the flag (or boolean, in which case
        /// you get no data).
        BoolCases : Choice<FlagDu, unit> option
        Help : SynExpr option
        /// A function string -> %TargetType%, where TargetVariable is probably a `%TargetType% option`.
        /// (Depending on `Accumulation`, we'll remove the `option` at the end of the parse, asserting that the
        /// argument was supplied.)
        /// This is allowed to throw if it fails to parse.
        Parser : SynExpr
        /// If `Accumulation` is `List`, then this is the type of the list *element*; analogously for optionals
        /// and choices and so on.
        TargetType : SynType
        Accumulation : 'acc
    }

    /// A SynExpr of type `string` which we can display to the user at generated-program runtime to display all
    /// the ways they can refer to this arg.
    member arg.HumanReadableArgForm : SynExpr =
        let formatString = List.replicate arg.ArgForm.Length "--%s" |> String.concat " / "

        (SynExpr.applyFunction (SynExpr.createIdent "sprintf") (SynExpr.CreateConst formatString), arg.ArgForm)
        ||> List.fold SynExpr.applyFunction
        |> SynExpr.paren


module internal ShibaGenerator =
    //let log (s : string) = System.IO.File.AppendAllText ("/tmp/myriad.log", s + "\n")
    type RecognisedType =
        | Union of UnionType
        | Record of RecordType

        member this.Name : Ident =
            match this with
            | Union unionType -> unionType.Name
            | Record recordType -> recordType.Name

    let private identifyAsFlag (flagDus : FlagDu list) (ty : SynType) : FlagDu option =
        match ty with
        | SynType.LongIdent (SynLongIdent.SynLongIdent (ident, _, _)) ->
            flagDus
            |> List.tryPick (fun du ->
                let duName = du.Name.idText
                let ident = List.last(ident).idText
                if duName = ident then Some du else None
            )
        | _ -> None

    /// Convert e.g. "Foo" into "--foo".
    let argify (ident : Ident) : string =
        let result = StringBuilder ()

        for c in ident.idText do
            if Char.IsUpper c then
                result.Append('-').Append (Char.ToLowerInvariant c) |> ignore<StringBuilder>
            else
                result.Append c |> ignore<StringBuilder>

        result.ToString().TrimStart '-'

    type LeafData<'choice> =
        {
            /// Call this function to turn the input into the `TypeAfterParse`.
            /// For example, `--foo=3` would have TypeAfterParse of `int`, and
            /// `ParseFn` would be a function `string -> int`.
            ParseFn : SynExpr
            /// The type of this field, as it will appear in the final user's record.
            TypeAfterParse : SynType
            /// Essentially, how many times this leaf is expected to appear.
            Acc : Accumulation<'choice>
            /// `None` if not positional. `Some None` if positional and the PositionalArgs attribute had no contents.
            /// `Some Some` if the PositionalArgs attribute had an argument.
            Positional : SynExpr option option
        }

    type private ParseFunctionSpec<'choice> =
        /// A leaf node, e.g. `--foo=3`.
        | Leaf of LeafData<'choice>
        /// An opaque node we didn't recognise: e.g. `Foo : SomeType`.
        /// We're probably going to stamp out an "in-progress" type for this node.
        /// (Either that, or it's just a type we don't recognise, and then compilation will fail.)
        | UserDefined of isRecord : bool * typeName : Ident
        /// An optional opaque node we didn't recognise: e.g. `Foo : SomeType option`.
        /// We're probably going to stamp out an "in-progress" type for this node.
        /// (Either that, or it's just a type we don't recognise, and then compilation will fail.)
        | OptionOfUserDefined

    /// Builds a function or lambda of one string argument, which returns a `ty` (as modified by the `Accumulation`;
    /// for example, maybe it returns a `ty option` or a `ty list`).
    /// The resulting SynType, if you get one, is the type of the *element* being parsed; so if the Accumulation is List, the SynType
    /// is the list element.
    let rec private createParseFunction<'choice>
        (choice : ArgumentDefaultSpec option -> 'choice)
        (flagDus : FlagDu list)
        (userDefinedRecordTypesWithParser : IEnumerable<string>)
        (userDefinedUnionTypesWithParser : IEnumerable<string>)
        (fieldName : Ident)
        (attrs : SynAttribute list)
        (ty : SynType)
        : ParseFunctionSpec<'choice>
        =
        let positional =
            attrs
            |> List.tryPick (fun a ->
                match (List.last a.TypeName.LongIdent).idText with
                | "PositionalArgsAttribute"
                | "PositionalArgs" ->
                    match a.ArgExpr with
                    | SynExpr.Const (SynConst.Unit, _) -> Some None
                    | a -> Some (Some a)
                | _ -> None
            )

        match ty with
        | String ->
            {
                ParseFn = SynExpr.createLambda "x" (SynExpr.createIdent "x")
                Acc = Accumulation.Required
                TypeAfterParse = SynType.string
                Positional = positional
            }
            |> ParseFunctionSpec.Leaf
        | PrimitiveType pt ->
            {
                ParseFn =
                    SynExpr.createLambda
                        "x"
                        (SynExpr.applyFunction
                            (SynExpr.createLongIdent' (pt @ [ Ident.create "Parse" ]))
                            (SynExpr.createIdent "x"))
                Acc = Accumulation.Required
                TypeAfterParse = ty
                Positional = positional
            }
            |> ParseFunctionSpec.Leaf
        | Uri ->
            {
                ParseFn =
                    SynExpr.createLambda
                        "x"
                        (SynExpr.applyFunction (SynExpr.createLongIdent [ "System" ; "Uri" ]) (SynExpr.createIdent "x"))
                Acc = Accumulation.Required
                TypeAfterParse = ty
                Positional = positional
            }
            |> ParseFunctionSpec.Leaf
        | TimeSpan ->
            let parseExact =
                attrs
                |> List.tryPick (fun attr ->
                    match attr.TypeName with
                    | SynLongIdent.SynLongIdent (idents, _, _) ->
                        match idents |> List.map (fun i -> i.idText) |> List.tryLast with
                        | Some "ParseExactAttribute"
                        | Some "ParseExact" -> Some attr.ArgExpr
                        | _ -> None
                )

            let culture =
                attrs
                |> List.tryPick (fun attr ->
                    match attr.TypeName with
                    | SynLongIdent.SynLongIdent (idents, _, _) ->
                        match idents |> List.map (fun i -> i.idText) |> List.tryLast with
                        | Some "InvariantCultureAttribute"
                        | Some "InvariantCulture" -> Some ()
                        | _ -> None
                )

            let parser =
                match parseExact, culture with
                | None, None ->
                    SynExpr.createIdent "x"
                    |> SynExpr.applyFunction (SynExpr.createLongIdent [ "System" ; "TimeSpan" ; "Parse" ])
                | Some format, None ->
                    [
                        SynExpr.createIdent "x"
                        format
                        SynExpr.createLongIdent [ "System" ; "Globalization" ; "CultureInfo" ; "CurrentCulture" ]
                    ]
                    |> SynExpr.tuple
                    |> SynExpr.applyFunction (SynExpr.createLongIdent [ "System" ; "TimeSpan" ; "ParseExact" ])
                | None, Some () ->
                    [
                        SynExpr.createIdent "x"
                        SynExpr.createLongIdent [ "System" ; "Globalization" ; "CultureInfo" ; "InvariantCulture" ]
                    ]
                    |> SynExpr.tuple
                    |> SynExpr.applyFunction (SynExpr.createLongIdent [ "System" ; "TimeSpan" ; "Parse" ])
                | Some format, Some () ->
                    [
                        SynExpr.createIdent "x"
                        format
                        SynExpr.createLongIdent [ "System" ; "Globalization" ; "CultureInfo" ; "InvariantCulture" ]
                    ]
                    |> SynExpr.tuple
                    |> SynExpr.applyFunction (SynExpr.createLongIdent [ "System" ; "TimeSpan" ; "ParseExact" ])
                |> SynExpr.createLambda "x"

            {
                ParseFn = parser
                Acc = Accumulation.Required
                TypeAfterParse = ty
                Positional = positional
            }
            |> ParseFunctionSpec.Leaf
        | FileInfo ->
            {
                ParseFn =
                    SynExpr.createLambda
                        "x"
                        (SynExpr.applyFunction
                            (SynExpr.createLongIdent [ "System" ; "IO" ; "FileInfo" ])
                            (SynExpr.createIdent "x"))
                Acc = Accumulation.Required
                TypeAfterParse = ty
                Positional = positional
            }
            |> ParseFunctionSpec.Leaf
        | DirectoryInfo ->
            {
                ParseFn =
                    SynExpr.createLambda
                        "x"
                        (SynExpr.applyFunction
                            (SynExpr.createLongIdent [ "System" ; "IO" ; "DirectoryInfo" ])
                            (SynExpr.createIdent "x"))
                Acc = Accumulation.Required
                TypeAfterParse = ty
                Positional = positional
            }
            |> ParseFunctionSpec.Leaf
        | OptionType eltTy ->
            match
                createParseFunction
                    choice
                    flagDus
                    userDefinedRecordTypesWithParser
                    userDefinedUnionTypesWithParser
                    fieldName
                    attrs
                    eltTy
            with
            | ParseFunctionSpec.Leaf data ->
                match data.Acc with
                | Accumulation.Optional ->
                    failwith
                        $"ArgParser does not support optionals containing options at field %s{fieldName.idText}: %O{ty}"
                | Accumulation.ChoicePositional _
                | Accumulation.Choice _ ->
                    failwith
                        $"ArgParser does not support optionals containing choices at field %s{fieldName.idText}: %O{ty}"
                | Accumulation.List _ ->
                    failwith $"ArgParser does not support optional lists at field %s{fieldName.idText}: %O{ty}"
                | Accumulation.Required ->
                    ParseFunctionSpec.Leaf
                        { data with
                            Acc = Accumulation.Optional
                        }
            | ParseFunctionSpec.UserDefined _ -> ParseFunctionSpec.OptionOfUserDefined
            | ParseFunctionSpec.OptionOfUserDefined ->
                failwith $"ArgParser does not support lists of options at field %s{fieldName.idText}"
        | ChoiceType elts ->
            match elts with
            | [ elt1 ; elt2 ] ->
                if not (SynType.provablyEqual elt1 elt2) then
                    failwith
                        $"ArgParser was unable to prove types %O{elt1} and %O{elt2} to be equal in a Choice. We require them to be equal."

                match
                    createParseFunction
                        choice
                        flagDus
                        userDefinedRecordTypesWithParser
                        userDefinedUnionTypesWithParser
                        fieldName
                        attrs
                        elt1
                with
                | ParseFunctionSpec.Leaf data ->
                    match data.Acc with
                    | Accumulation.Optional ->
                        failwith
                            $"ArgParser does not support choices containing options at field %s{fieldName.idText}: %O{ty}"
                    | Accumulation.List _ ->
                        failwith
                            $"ArgParser does not support choices containing lists at field %s{fieldName.idText}: %O{ty}"
                    | Accumulation.ChoicePositional _
                    | Accumulation.Choice _ ->
                        failwith
                            $"ArgParser does not support choices containing choices at field %s{fieldName.idText}: %O{ty}"
                    | Accumulation.Required ->

                    let relevantAttrs =
                        attrs
                        |> List.choose (fun attr ->
                            let (SynLongIdent.SynLongIdent (name, _, _)) = attr.TypeName

                            match name |> List.map _.idText with
                            | [ "ArgumentDefaultFunction" ]
                            | [ "ArgumentDefaultFunctionAttribute" ]
                            | [ "Plugins" ; "ArgumentDefaultFunction" ]
                            | [ "Plugins" ; "ArgumentDefaultFunctionAttribute" ]
                            | [ "Myriad" ; "Plugins" ; "ArgumentDefaultFunction" ]
                            | [ "Myriad" ; "Plugins" ; "ArgumentDefaultFunctionAttribute" ]
                            | [ "WoofWare" ; "Myriad" ; "Plugins" ; "ArgumentDefaultFunction" ]
                            | [ "WoofWare" ; "Myriad" ; "Plugins" ; "ArgumentDefaultFunctionAttribute" ] ->
                                ArgumentDefaultSpec.FunctionCall (Ident.create ("Default" + fieldName.idText))
                                |> Some
                            | [ "ArgumentDefaultEnvironmentVariable" ]
                            | [ "ArgumentDefaultEnvironmentVariableAttribute" ]
                            | [ "Plugins" ; "ArgumentDefaultEnvironmentVariable" ]
                            | [ "Plugins" ; "ArgumentDefaultEnvironmentVariableAttribute" ]
                            | [ "Myriad" ; "Plugins" ; "ArgumentDefaultEnvironmentVariable" ]
                            | [ "Myriad" ; "Plugins" ; "ArgumentDefaultEnvironmentVariableAttribute" ]
                            | [ "WoofWare" ; "Myriad" ; "Plugins" ; "ArgumentDefaultEnvironmentVariable" ]
                            | [ "WoofWare" ; "Myriad" ; "Plugins" ; "ArgumentDefaultEnvironmentVariableAttribute" ] ->

                                ArgumentDefaultSpec.EnvironmentVariable attr.ArgExpr |> Some
                            | _ -> None
                        )

                    let relevantAttr =
                        match relevantAttrs with
                        | [] -> None
                        | [ x ] -> Some x
                        | _ ->
                            failwith
                                $"Expected Choice to be annotated with at most one ArgumentDefaultFunction or similar, but it was annotated with multiple. Field: %s{fieldName.idText}"

                    match positional with
                    | Some positional ->
                        ParseFunctionSpec.Leaf
                            { data with
                                Acc = Accumulation.ChoicePositional positional
                            }
                    | None ->
                        ParseFunctionSpec.Leaf
                            { data with
                                Acc = Accumulation.Choice (choice relevantAttr)
                            }
                | _ ->
                    failwith
                        $"Choices are only allowed to contain leaves; at %s{fieldName.idText}, got type %s{SynType.toHumanReadableString elt1}"
            | elts ->
                let elts = elts |> List.map string<SynType> |> String.concat ", "

                failwith
                    $"ArgParser requires Choice to be of the form Choice<'a, 'a>; that is, two arguments, both the same. For field %s{fieldName.idText}, got: %s{elts}"
        | ListType eltTy ->
            match
                createParseFunction
                    choice
                    flagDus
                    userDefinedRecordTypesWithParser
                    userDefinedUnionTypesWithParser
                    fieldName
                    attrs
                    eltTy
            with
            | ParseFunctionSpec.Leaf data ->
                ParseFunctionSpec.Leaf
                    { data with
                        Acc = Accumulation.List data.Acc
                    }
            | _ ->
                failwith
                    $"Lists are only allowed to contain leaves; at %s{fieldName.idText}, got type %s{SynType.toHumanReadableString eltTy}"
        | ty ->
            match identifyAsFlag flagDus ty with
            | None ->
                match ty with
                | SynType.LongIdent (SynLongIdent.SynLongIdent (id = id)) ->
                    let typeName = List.last id

                    if Seq.contains typeName.idText userDefinedRecordTypesWithParser then
                        ParseFunctionSpec.UserDefined (true, typeName)
                    elif Seq.contains (List.last id).idText userDefinedUnionTypesWithParser then
                        ParseFunctionSpec.UserDefined (false, typeName)
                    else
                        failwith
                            $"we did not recognise the type %s{SynType.toHumanReadableString ty} as something we could build a parser for"
                | _ ->
                    failwith
                        $"we did not recognise the type %s{SynType.toHumanReadableString ty} as something we could build a parser for"
            | Some flagDu ->
                // Parse as a bool, and then do the `if-then` dance.
                let parser =
                    SynExpr.createIdent "x"
                    |> SynExpr.applyFunction (SynExpr.createLongIdent [ "System" ; "Boolean" ; "Parse" ])
                    |> FlagDu.FromBoolean flagDu
                    |> SynExpr.createLambda "x"

                {
                    ParseFn = parser
                    Acc = Accumulation.Required
                    TypeAfterParse = ty
                    Positional = positional
                }
                |> ParseFunctionSpec.Leaf

    type internal DatalessUnion =
        {
            Cases : (string * SynAttribute list) list
        }

    type internal ParsedRecordStructure<'choice> =
        {
            Original : RecordType
            /// Map of field name to parser for that field
            LeafNodes : Map<string, LeafData<'choice>>
            Records : Map<string, ParsedRecordStructure<'choice>>
            Unions : Map<string, ParsedUnionStructure<'choice>>
        }

    and internal ParsedUnionStructure<'choice> =
        {
            Original : UnionType
            Cases : Map<string, ParsedRecordStructure<'choice>>
        }

    /// Build the "in-progress record" which is basically "the input record, but with all fields mutable and optional".
    let private inProgressRecordType (record : ParsedRecordStructure<'choice>) : RecordType =
        let leafFields =
            record.LeafNodes
            |> Map.toSeq
            |> Seq.map (fun (ident, data) -> failwith "TODO")
            |> Seq.toList

        let unionFields =
            record.Unions
            |> Map.toSeq
            |> Seq.map (fun (ident, data) -> failwith "TODO")
            |> Seq.toList

        let recordFields =
            record.Records
            |> Map.toSeq
            |> Seq.map (fun (ident, data) -> failwith "TODO")
            |> Seq.toList

        let fields =
            leafFields @ unionFields @ recordFields
            |> fun l ->
                if l.IsEmpty then
                    {
                        Attrs = []
                        Ident = Some (Ident.create "_Dummy")
                        Type = SynType.unit
                    }
                    |> SynField.make
                    |> List.singleton
                else
                    l |> List.map (SynField.withMutability true)

        let members =
            // for each field `FieldName` in order, we've made a variable `arg%i`
            // which has done the optionality check
            let instantiation =
                record.Original.Fields
                |> List.mapi (fun i (SynField.SynField (idOpt = ident)) ->
                    match ident with
                    | None ->
                        failwith
                            $"expected field in record %s{record.Original.Name.idText} to have a name, but it did not"
                    | Some ident -> SynLongIdent.create [ ident ], SynExpr.createIdent $"arg%i{i}"
                )
                |> SynExpr.createRecord None
                |> SynExpr.applyFunction (SynExpr.createIdent "Ok")

            let defaultOf =
                SynExpr.typeApp [ SynType.anon ] (SynExpr.createLongIdent [ "Unchecked" ; "defaultof" ])

            let assignVariables =
                record.Original.Fields
                |> List.mapi (fun i f -> (i, f))
                |> List.collect (fun (i, SynField.SynField (attributes = attrs ; fieldType = ty ; idOpt = ident)) ->
                    match ident with
                    | None ->
                        failwith
                            $"expected field in record %s{record.Original.Name.idText} to have a name, but it did not"
                    | Some ident ->

                    let valueForThisVar =
                        match record.Records |> Map.tryFind ident.idText with
                        | Some subRecord ->
                            // This was a record; defer to its parser.
                            // TODO: need to know if it has positionals
                            [
                                SynMatchClause.create
                                    (SynPat.identWithArgs
                                        [ Ident.create "Ok" ]
                                        (SynArgPats.create [ SynPat.named "result" ]))
                                    (SynExpr.createIdent "result")
                                SynMatchClause.create
                                    (SynPat.identWithArgs
                                        [ Ident.create "Error" ]
                                        (SynArgPats.create [ SynPat.named "err" ]))
                                    (SynExpr.sequential
                                        [
                                            SynExpr.callMethodArg
                                                "AddRange"
                                                (SynExpr.createIdent "errors")
                                                (SynExpr.createIdent "err")
                                            defaultOf
                                        ])
                            ]
                            |> SynExpr.createMatch (SynExpr.dotGet ident.idText (SynExpr.createIdent "this"))
                        | None ->

                        match record.Unions |> Map.tryFind ident.idText with
                        | Some union ->
                            // This was a union; defer to its parser.
                            failwith "TODO"
                        | None ->

                        match record.LeafNodes |> Map.tryFind ident.idText with
                        | Some leaf ->
                            match leaf.Positional with
                            | Some pos ->
                                // Positional args carried in from external argument.
                                // TODO: register whether they came before or after separator
                                SynExpr.createIdent "positionals"
                            | None ->

                            let extract =
                                match leaf.TypeAfterParse with
                                | ChoiceType [ _ ; _ ] ->
                                    [
                                        SynMatchClause.create
                                            (SynPat.identWithArgs
                                                [ Ident.create "Some" ]
                                                (SynArgPats.create [ SynPat.named "result" ]))
                                            (SynExpr.applyFunction
                                                (SynExpr.createIdent "Choice1Of2")
                                                (SynExpr.createIdent "result"))
                                        SynMatchClause.create
                                            (SynPat.identWithArgs [ Ident.create "None" ] (SynArgPats.create []))
                                            (SynExpr.CreateConst "TODO"
                                             |> SynExpr.applyFunction (SynExpr.createIdent "Choice2Of2"))
                                    ]
                                    |> SynExpr.createMatch (SynExpr.dotGet ident.idText (SynExpr.createIdent "this"))
                                | ListType _ -> SynExpr.dotGet ident.idText (SynExpr.createIdent "this")
                                | PrimitiveType _ ->
                                    [
                                        SynMatchClause.create
                                            (SynPat.identWithArgs
                                                [ Ident.create "Some" ]
                                                (SynArgPats.create [ SynPat.named "result" ]))
                                            (SynExpr.createIdent "result")
                                        SynMatchClause.create
                                            (SynPat.identWithArgs [ Ident.create "None" ] (SynArgPats.create []))
                                            (SynExpr.sequential
                                                [
                                                    SynExpr.callMethodArg
                                                        "Add"
                                                        (SynExpr.CreateConst $"no value provided for %s{ident.idText}")
                                                        (SynExpr.createIdent "errors")
                                                    defaultOf
                                                ])
                                    ]
                                    |> SynExpr.createMatch (SynExpr.dotGet ident.idText (SynExpr.createIdent "this"))
                                | OptionType _ -> SynExpr.dotGet ident.idText (SynExpr.createIdent "this")
                                | ty -> failwith $"Could not convert type %s{SynType.toHumanReadableString ty}"

                            extract
                        | None ->
                            failwith
                                $"somehow we never classified the field %s{ident.idText} of %s{record.Original.Name.idText}"

                    valueForThisVar
                    |> SynBinding.basic [ Ident.create $"arg%i{i}" ] []
                    |> SynBinding.withReturnAnnotation ty
                    |> List.singleton
                )

            SynExpr.ifThenElse
                (SynExpr.equals (SynExpr.dotGet "Count" (SynExpr.createIdent "errors")) (SynExpr.CreateConst 0))
                (SynExpr.createIdent "errors"
                 |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "Seq" ; "toList" ])
                 |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Error"))
                instantiation
            |> SynExpr.createLet assignVariables
            |> SynExpr.createLet
                [
                    SynBinding.basic
                        [ Ident.create "errors" ]
                        []
                        (SynExpr.applyFunction
                            (SynExpr.typeApp [ SynType.string ] (SynExpr.createIdent "ResizeArray"))
                            (SynExpr.CreateConst ()))
                ]
            |> SynBinding.basic
                [ Ident.create "this" ; Ident.create "Assemble" ]
                [
                    SynPat.annotateType (SynType.list SynType.string) (SynPat.named "positionals")
                ]
            |> SynBinding.withReturnAnnotation (
                SynType.app
                    "Result"
                    [
                        SynType.createLongIdent [ record.Original.Name ]
                        SynType.list SynType.string
                    ]
            )

        {
            Name = record.Original.Name.idText + "_InProgress" |> Ident.create
            Fields = fields
            Members = members |> SynMemberDefn.memberImplementation |> List.singleton |> Some
            XmlDoc = PreXmlDoc.create $"A partially-parsed %s{record.Original.Name.idText}." |> Some
            Generics =
                match record.Original.Generics with
                | None -> None
                | Some _ ->
                    failwith $"Record type %s{record.Original.Name.idText} had generics, which we don't support."
            TypeAccessibility = Some (SynAccess.Private range0)
            ImplAccessibility = None
            Attributes = []
        }

    type internal AllInfo =
        {
            /// Map of identifier to parser
            RecordParsers : IReadOnlyDictionary<string, ParsedRecordStructure<ArgumentDefaultSpec>>
            /// Map of identifier to parser
            UnionParsers : IReadOnlyDictionary<string, ParsedUnionStructure<ArgumentDefaultSpec>>
            /// Map of identifier to DU information
            FlagDus : Map<string, FlagDu>
            /// Map of identifier to DU information
            DatalessUnions : Map<string, DatalessUnion>
        }

    /// Returns None if we haven't yet obtained parse structures for the dependencies of this record.
    let private parseRecord
        (knownRecordParserTypes : IReadOnlyDictionary<string, ParsedRecordStructure<ArgumentDefaultSpec>>)
        (knownUnionParserTypes : IReadOnlyDictionary<string, ParsedUnionStructure<ArgumentDefaultSpec>>)
        (flagDus : FlagDu list)
        (rt : RecordType)
        : ParsedRecordStructure<ArgumentDefaultSpec> option
        =
        let getChoice (spec : ArgumentDefaultSpec option) : ArgumentDefaultSpec =
            match spec with
            | None ->
                failwith
                    $"Non-positional Choice args must have an `[<ArgumentDefault*>]` attribute on them, in record {rt.Name.idText}."
            | Some spec -> spec

        let aggregated =
            (Some ([], [], []), rt.Fields)
            ||> List.fold (fun aggr (SynField.SynField (idOpt = ident ; attributes = attrs ; fieldType = ty)) ->
                match aggr with
                | None -> None
                | Some (leaf, records, unions) ->

                match ident with
                | None ->
                    failwith
                        $"expected all fields on record type %s{rt.Name.idText} to have a name, but at least one did not"
                | Some ident ->

                let spec =
                    createParseFunction
                        getChoice
                        flagDus
                        knownRecordParserTypes.Keys
                        knownUnionParserTypes.Keys
                        ident
                        (SynAttributes.toAttrs attrs)
                        ty

                match spec with
                | Leaf data -> ((ident.idText, data) :: leaf, records, unions) |> Some
                | UserDefined (isRecord, typeName) ->
                    if isRecord then
                        match knownRecordParserTypes.TryGetValue typeName.idText with
                        | false, _ -> None
                        | true, v -> (leaf, (ident.idText, v) :: records, unions) |> Some
                    else
                        match knownUnionParserTypes.TryGetValue typeName.idText with
                        | false, _ -> None
                        | true, v -> (leaf, records, (ident.idText, v) :: unions) |> Some
                | OptionOfUserDefined -> failwith "todo"
            )

        match aggregated with
        | None -> None
        | Some (leaf, records, unions) ->
            {
                Original = rt
                LeafNodes = leaf |> Map.ofList
                Records = records |> Map.ofList
                Unions = unions |> Map.ofList
            }
            |> Some

    /// Returns None if we haven't yet obtained parse structures for the dependencies of this union.
    /// This function already knows that it's a parser: that is, every case has exactly one field.
    /// It doesn't necessarily know that those fields can be parsed as records.
    let private parseUnion
        (knownRecordTypes : IReadOnlyDictionary<string, ParsedRecordStructure<ArgumentDefaultSpec>>)
        (ut : UnionType)
        : ParsedUnionStructure<ArgumentDefaultSpec> option
        =
        ut.Cases
        |> List.map (fun case ->
            let field =
                match case.Fields with
                | [ x ] -> x
                | [] ->
                    failwith
                        $"Logic error: expected case %s{case.Name.idText} to have exactly one field, but it had none"
                | _ ->
                    failwith
                        $"Logic error: expected case %s{case.Name.idText} to have exactly one field, but it had more than one"

            match field.Type with
            | SynType.LongIdent (SynLongIdent.SynLongIdent (id = id)) ->
                match knownRecordTypes.TryGetValue (List.last id).idText with
                | false, _ -> None
                | true, v -> Some (case.Name.idText, v)
            | _ ->
                failwith
                    "ArgParser generator requires discriminated union cases to each contain exactly one field which is a record type, to hold their data."
        )
        |> List.allSome
        |> Option.map Map.ofList
        |> Option.map (fun x ->
            {
                Original = ut
                Cases = x
            }
        )

    let internal parseStructureWithinNs (unions : UnionType list) (records : RecordType list) : AllInfo =
        let flagDus, datalessUnions, parserUnions =
            (([], [], []), unions)
            ||> List.fold (fun (flagDus, datalessUnions, unions) union ->
                match union.Cases |> List.tryFind (fun case -> not case.Fields.IsEmpty) with
                | Some dataCarryingCase ->
                    match union.Cases |> List.tryFind (fun case -> case.Fields.Length <> 1) with
                    | Some badCase ->
                        failwith
                            $"Unions must either be dataless or every field must have exactly one member. Type %s{union.Name.idText} has case %s{dataCarryingCase.Name.idText} with data, but case %s{badCase.Name.idText} doesn't have exactly one field."
                    | None ->
                        // OK, all cases have exactly one field.
                        flagDus, datalessUnions, union :: unions
                | None ->

                let datalessUnionBranch () =
                    let datalessUnion =
                        {
                            DatalessUnion.Cases =
                                union.Cases |> List.map (fun case -> case.Name.idText, case.Attributes)
                        }

                    flagDus, (union.Name.idText, datalessUnion) :: datalessUnions, unions

                // dataless or flag
                match union.Cases with
                | [ c1 ; c2 ] ->
                    let c1Attr =
                        c1.Attributes
                        |> List.tryPick (fun attr ->
                            match attr.TypeName with
                            | SynLongIdent.SynLongIdent (id, _, _) ->
                                match id |> List.last |> _.idText with
                                | "ArgumentFlagAttribute"
                                | "ArgumentFlag" -> Some (SynExpr.stripOptionalParen attr.ArgExpr)
                                | _ -> None
                        )

                    let c2Attr =
                        c2.Attributes
                        |> List.tryPick (fun attr ->
                            match attr.TypeName with
                            | SynLongIdent.SynLongIdent (id, _, _) ->
                                match id |> List.last |> _.idText with
                                | "ArgumentFlagAttribute"
                                | "ArgumentFlag" -> Some (SynExpr.stripOptionalParen attr.ArgExpr)
                                | _ -> None
                        )

                    match c1Attr, c2Attr with
                    | Some _, None
                    | None, Some _ ->
                        failwith
                            "[<ArgumentFlag>] must be placed on both cases of a two-case discriminated union, with opposite argument values on each case."
                    | None, None ->
                        // actually a dataless union
                        datalessUnionBranch ()
                    | Some c1Attr, Some c2Attr ->

                    // Sanity check where possible
                    match c1Attr, c2Attr with
                    | SynExpr.Const (SynConst.Bool b1, _), SynExpr.Const (SynConst.Bool b2, _) ->
                        if b1 = b2 then
                            failwith
                                "[<ArgumentFlag>] must have opposite argument values on each case in a two-case discriminated union."
                    | _, _ -> ()

                    match c1.Fields, c2.Fields with
                    | [], [] ->
                        let flagDu =
                            {
                                Name = union.Name
                                Case1Name = c1.Name
                                Case1Arg = c1Attr
                                Case2Name = c2.Name
                                Case2Arg = c2Attr
                            }

                        (union.Name.idText, flagDu) :: flagDus, datalessUnions, unions
                    | _, _ ->
                        failwith "[<ArgumentFlag>] may only be placed on discriminated union members with no data."
                | _ -> datalessUnionBranch ()
            )

        let allKnownUnionTypes = Dictionary ()
        let allKnownRecordTypes = Dictionary ()

        let mutable keepLooping = true

        while keepLooping do
            keepLooping <- false
            let mutable madeAChange = false

            for record in records do
                if not (allKnownRecordTypes.ContainsKey record.Name.idText) then
                    match parseRecord allKnownRecordTypes allKnownUnionTypes (flagDus |> List.map snd) record with
                    | None -> keepLooping <- true
                    | Some v ->
                        allKnownRecordTypes.Add (record.Name.idText, v)
                        madeAChange <- true

            for union in parserUnions do
                match parseUnion allKnownRecordTypes union with
                | None -> keepLooping <- true
                | Some v ->
                    allKnownUnionTypes.Add (union.Name.idText, v)
                    madeAChange <- true

            if not madeAChange then
                let knownRecords = allKnownRecordTypes.Keys |> String.concat ","
                let knownUnions = allKnownUnionTypes.Keys |> String.concat ","

                failwith
                    $"Cyclic dependency detected which we can't break. Known records:\n%s{knownRecords}\nKnown unions:\n%s{knownUnions}"

        {
            RecordParsers = allKnownRecordTypes
            UnionParsers = allKnownUnionTypes
            FlagDus = Map.ofList flagDus
            DatalessUnions = Map.ofList datalessUnions
        }

    /// Some types don't have in-progress equivalents (e.g. a no-data DU, which is "basically primitive");
    /// hence the `option`.
    let createInProgressRecognisedType
        (flagDuNames : string ICollection)
        (allKnownTypeIdents : string list)
        (ty : RecognisedType)
        : RecordType option
        =
        /// Get the "in-progress type" corresponding to the type with this name.
        let getInProgressTypeName (ty : LongIdent) : SynType =
            // TODO: this is super jank
            let ident = List.last ty

            if flagDuNames.Contains ident.idText then
                // Flag DUs have no in-progress form as such
                SynType.createLongIdent ty |> SynType.option
            elif List.contains ident.idText allKnownTypeIdents then
                SynType.createLongIdent [ ident.idText + "_InProgress" |> Ident.create ]
            else
                // TODO: this is just nonsense, probably
                SynType.createLongIdent ty |> SynType.option

        let makeType (attrs : SynAttribute list) (ty : SynType) (id : Ident) : SynField option =
            match ty with
            | ChoiceType [ left ; right ] ->
                if not (SynType.provablyEqual left right) then
                    failwith
                        $"ArgParser was unable to prove types %O{left} and %O{right} to be equal in a Choice. We require them to be equal."

                {
                    Attrs = []
                    Ident = Some id
                    Type = SynType.option left
                }
                |> SynField.make
                |> Some
            | ChoiceType _ ->
                failwith
                    $"Only `Choice`s with exactly two args are supported, and they must have the same type on each side (field name: %s{id.idText})"
            | ListType contents ->
                // TODO: jank conditional
                if
                    attrs
                    |> List.exists (fun x -> List.last(x.TypeName.LongIdent).idText.StartsWith "PositionalArgs")
                then
                    // Omit positional args, they are treated in the Finalise
                    None
                else

                {
                    Attrs = []
                    Ident = Some id
                    Type =
                        // Parser will take strings later, when finalising
                        SynType.list SynType.string
                }
                |> SynField.make
                |> Some
            | PrimitiveType ty ->
                {
                    Attrs = []
                    Ident = Some id
                    Type = SynType.option (SynType.createLongIdent ty)
                }
                |> SynField.make
                |> Some
            | OptionType ty ->
                {
                    Attrs = []
                    Ident = Some id
                    Type =
                        // an `option` is its own in-progress
                        SynType.option ty
                }
                |> SynField.make
                |> Some
            | SynType.LongIdent (SynLongIdent.SynLongIdent (ident, _, _)) ->
                // Assume this is in-progress
                {
                    Attrs = []
                    Ident = Some id
                    Type = getInProgressTypeName ident
                }
                |> SynField.make
                |> Some
            | ty -> failwith $"TODO: %O{ty}"

        match ty with
        | RecognisedType.Union union ->
            if union.Cases |> List.forall (fun case -> case.Fields.IsEmpty) then
                None
            else

            {
                Name = union.Name.idText + "_InProgress" |> Ident.create
                XmlDoc = PreXmlDoc.create $"A partially-parsed %s{union.Name.idText}." |> Some
                Members =
                    SynExpr.CreateConst "TODO: now construct the object"
                    |> SynBinding.basic
                        [ Ident.create "this" ; Ident.create "Assemble" ]
                        [
                            SynPat.annotateType (SynType.list SynType.string) (SynPat.named "positionals")
                        ]
                    |> SynBinding.withReturnAnnotation (
                        SynType.app "Result" [ SynType.createLongIdent [ union.Name ] ; SynType.list SynType.string ]
                    )
                    |> SynMemberDefn.memberImplementation
                    |> List.singleton
                    |> Some
                Fields =
                    union.Cases
                    |> List.mapi (fun i data -> i, data)
                    |> List.choose (fun (caseNum, case) ->
                        match case.Fields with
                        | [] ->
                            failwith
                                $"Union type %s{union.Name.idText} has case %s{case.Name.idText} with no data; we require all cases to have exactly one field, or else all cases to be empty."
                        | [ x ] -> makeType x.Attrs x.Type (Ident.create $"Case_%i{caseNum}")
                        | _ ->
                            failwith
                                $"Union type %s{union.Name.idText} has case %s{case.Name.idText} with multiple fields; we require all cases to have exactly one field, or else all cases to be empty. Define a record type to hold the contents."
                    )
                    |> fun l ->
                        if l.IsEmpty then
                            [
                                SynField.make
                                    {
                                        Attrs = []
                                        Ident = Some (Ident.create "_Dummy")
                                        Type = SynType.unit
                                    }
                            ]
                        else
                            l |> List.map (SynField.withMutability true)
                Generics =
                    match union.Generics with
                    | None -> None
                    | Some _ -> failwith $"Union type %s{union.Name.idText} had generics, which we don't support."
                TypeAccessibility = Some (SynAccess.Private range0)
                ImplAccessibility = None
                Attributes = []
            }
            |> Some
        | RecognisedType.Record record ->
            {
                Name = record.Name.idText + "_InProgress" |> Ident.create
                Fields =
                    record.Fields
                    |> List.choose (fun (SynField.SynField (attrs, _, id, ty, _, _, _, _, _)) ->
                        match id with
                        | None ->
                            failwith $"expected field in record %s{record.Name.idText} to have a name, but it did not"
                        | Some id -> makeType (SynAttributes.toAttrs attrs) ty id
                    )
                    |> fun l ->
                        if l.IsEmpty then
                            {
                                Attrs = []
                                Ident = Some (Ident.create "_Dummy")
                                Type = SynType.unit
                            }
                            |> SynField.make
                            |> List.singleton
                        else
                            l |> List.map (SynField.withMutability true)
                Members =
                    // for each field `FieldName` in order, we've made a variable `arg%i`
                    // which has done the optionality check
                    let instantiation =
                        record.Fields
                        |> List.mapi (fun i (SynField.SynField (idOpt = ident)) ->
                            match ident with
                            | None ->
                                failwith
                                    $"expected field in record %s{record.Name.idText} to have a name, but it did not"
                            | Some ident -> SynLongIdent.create [ ident ], SynExpr.createIdent $"arg%i{i}"
                        )
                        |> SynExpr.createRecord None
                        |> SynExpr.applyFunction (SynExpr.createIdent "Ok")

                    let defaultOf =
                        SynExpr.typeApp [ SynType.anon ] (SynExpr.createLongIdent [ "Unchecked" ; "defaultof" ])

                    let assignVariables =
                        record.Fields
                        |> List.mapi (fun i f -> (i, f))
                        |> List.collect (fun
                                             (i, SynField.SynField (attributes = attrs ; fieldType = ty ; idOpt = ident)) ->
                            match ident with
                            | None ->
                                failwith
                                    $"expected field in record %s{record.Name.idText} to have a name, but it did not"
                            | Some ident ->
                            // TODO: jank conditional
                            if
                                attrs
                                |> SynAttributes.toAttrs
                                |> List.exists (fun x ->
                                    List.last(x.TypeName.LongIdent).idText.StartsWith "PositionalArgs"
                                )
                            then
                                // Positional args carried in from external argument
                                [
                                    SynBinding.basic
                                        [ Ident.create $"arg%i{i}" ]
                                        []
                                        (SynExpr.createIdent "positionals")
                                    |> SynBinding.withReturnAnnotation ty
                                    // prevent further usages of positional args
                                    SynBinding.basic [ Ident.create "positionals" ] [] (SynExpr.CreateConst ())
                                ]
                            else
                                let extract =
                                    match ty with
                                    | ChoiceType [ _ ; _ ] ->
                                        [
                                            SynMatchClause.create
                                                (SynPat.identWithArgs
                                                    [ Ident.create "Some" ]
                                                    (SynArgPats.create [ SynPat.named "result" ]))
                                                (SynExpr.applyFunction
                                                    (SynExpr.createIdent "Choice1Of2")
                                                    (SynExpr.createIdent "result"))
                                            SynMatchClause.create
                                                (SynPat.identWithArgs [ Ident.create "None" ] (SynArgPats.create []))
                                                (SynExpr.CreateConst "TODO"
                                                 |> SynExpr.applyFunction (SynExpr.createIdent "Choice2Of2"))
                                        ]
                                        |> SynExpr.createMatch (
                                            SynExpr.dotGet ident.idText (SynExpr.createIdent "this")
                                        )
                                    | ListType _ -> SynExpr.dotGet ident.idText (SynExpr.createIdent "this")
                                    | PrimitiveType _ ->
                                        [
                                            SynMatchClause.create
                                                (SynPat.identWithArgs
                                                    [ Ident.create "Some" ]
                                                    (SynArgPats.create [ SynPat.named "result" ]))
                                                (SynExpr.createIdent "result")
                                            SynMatchClause.create
                                                (SynPat.identWithArgs [ Ident.create "None" ] (SynArgPats.create []))
                                                (SynExpr.sequential
                                                    [
                                                        SynExpr.callMethodArg
                                                            "Add"
                                                            (SynExpr.CreateConst
                                                                $"no value provided for %s{ident.idText}")
                                                            (SynExpr.createIdent "errors")
                                                        defaultOf
                                                    ])
                                        ]
                                        |> SynExpr.createMatch (
                                            SynExpr.dotGet ident.idText (SynExpr.createIdent "this")
                                        )
                                    | OptionType _ -> SynExpr.dotGet ident.idText (SynExpr.createIdent "this")
                                    | SynType.LongIdent (SynLongIdent.SynLongIdent _) ->
                                        // TODO: need to know if it has positionals
                                        [
                                            SynMatchClause.create
                                                (SynPat.identWithArgs
                                                    [ Ident.create "Ok" ]
                                                    (SynArgPats.create [ SynPat.named "result" ]))
                                                (SynExpr.createIdent "result")
                                            SynMatchClause.create
                                                (SynPat.identWithArgs
                                                    [ Ident.create "Error" ]
                                                    (SynArgPats.create [ SynPat.named "err" ]))
                                                (SynExpr.sequential
                                                    [
                                                        SynExpr.callMethodArg
                                                            "AddRange"
                                                            (SynExpr.createIdent "errors")
                                                            (SynExpr.createIdent "err")
                                                        defaultOf
                                                    ])
                                        ]
                                        |> SynExpr.createMatch (
                                            SynExpr.dotGet ident.idText (SynExpr.createIdent "this")
                                        )
                                    | ty -> failwith $"TODO: got type {ty} which we don't know how to handle"

                                extract
                                |> SynBinding.basic [ Ident.create $"arg%i{i}" ] []
                                |> SynBinding.withReturnAnnotation ty
                                |> List.singleton
                        )

                    SynExpr.ifThenElse
                        (SynExpr.equals (SynExpr.dotGet "Count" (SynExpr.createIdent "errors")) (SynExpr.CreateConst 0))
                        (SynExpr.createIdent "errors"
                         |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "Seq" ; "toList" ])
                         |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Error"))
                        instantiation
                    |> SynExpr.createLet assignVariables
                    |> SynExpr.createLet
                        [
                            SynBinding.basic
                                [ Ident.create "errors" ]
                                []
                                (SynExpr.applyFunction
                                    (SynExpr.typeApp [ SynType.string ] (SynExpr.createIdent "ResizeArray"))
                                    (SynExpr.CreateConst ()))
                        ]
                    |> SynBinding.basic
                        [ Ident.create "this" ; Ident.create "Assemble" ]
                        [
                            SynPat.annotateType (SynType.list SynType.string) (SynPat.named "positionals")
                        ]
                    |> SynBinding.withReturnAnnotation (
                        SynType.app "Result" [ SynType.createLongIdent [ record.Name ] ; SynType.list SynType.string ]
                    )
                    |> SynMemberDefn.memberImplementation
                    |> List.singleton
                    |> Some
                XmlDoc = PreXmlDoc.create $"A partially-parsed %s{record.Name.idText}." |> Some
                Generics =
                    match record.Generics with
                    | None -> None
                    | Some _ -> failwith $"Record type %s{record.Name.idText} had generics, which we don't support."
                TypeAccessibility = Some (SynAccess.Private range0)
                ImplAccessibility = None
                Attributes = []
            }
            |> Some

    let createHelpersModule (opens : SynOpenDeclTarget list) (ns : LongIdent) (info : AllInfo) : SynModuleDecl =
        let modName =
            let ns = ns |> List.map _.idText |> String.concat "_"
            Ident.create $"ArgParseHelpers_%s{ns}"

        let modInfo =
            SynComponentInfo.create modName
            |> SynComponentInfo.withAccessibility (SynAccess.Private range0)
            |> SynComponentInfo.withDocString (PreXmlDoc.create $"Helper types for arg parsing")

        let flagDuNames = info.FlagDus.Keys

        let reducedRecordTypes =
            info.RecordParsers
            |> Seq.map (fun (KeyValue (_, record)) -> inProgressRecordType record |> RecordType.ToAst)
            |> Seq.toList

        let reducedUnionTypes =
            info.UnionParsers
            |> Seq.map (fun (KeyValue (_, union)) -> failwith "TODO")
            |> Seq.toList

        let taggedMod =
            [
                for openStatement in opens do
                    yield SynModuleDecl.openAny openStatement
                yield SynModuleDecl.openAny (SynOpenDeclTarget.ModuleOrNamespace (SynLongIdent.create ns, range0))

                yield (reducedRecordTypes @ reducedUnionTypes) |> SynModuleDecl.createTypes
            ]
            |> SynModuleDecl.nestedModule modInfo

        taggedMod

    // The type for which we're generating args may refer to any of the supplied records/unions.
    let createModule
        (opens : SynOpenDeclTarget list)
        (ns : LongIdent)
        ((taggedType : SynTypeDefn, spec : ArgParserOutputSpec))
        (allUnionTypes : UnionType list)
        (allRecordTypes : RecordType list)
        : SynModuleOrNamespace
        =
        let taggedType =
            match taggedType with
            | SynTypeDefn.SynTypeDefn (sci,
                                       SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (access, fields, _), _),
                                       smd,
                                       _,
                                       _,
                                       _) -> RecordType.OfRecord sci smd access fields
            | _ -> failwith "[<ArgParser>] currently only supports being placed on records."

        let modAttrs, modName =
            if spec.ExtensionMethods then
                [ SynAttribute.autoOpen ], Ident.create (taggedType.Name.idText + "ArgParse")
            else
                [ SynAttribute.requireQualifiedAccess ; SynAttribute.compilationRepresentation ], taggedType.Name

        let modInfo =
            SynComponentInfo.create modName
            |> SynComponentInfo.withDocString (
                PreXmlDoc.create $"Methods to parse arguments for the type %s{taggedType.Name.idText}"
            )
            |> SynComponentInfo.addAttributes modAttrs

        let parseStateIdent = Ident.create $"ParseState_%s{taggedType.Name.idText}"

        let parseStateType =
            [
                SynUnionCase.create
                    {
                        Attributes = []
                        Fields = []
                        Name = Ident.create "AwaitingKey"
                        XmlDoc = Some (PreXmlDoc.create "Ready to consume a key or positional arg")
                        Access = None
                    }
                SynUnionCase.create
                    {
                        Attributes = []
                        Fields =
                            [
                                {
                                    Attrs = []
                                    Ident = Some (Ident.create "key")
                                    Type = SynType.string
                                }
                            ]
                        Name = Ident.create "AwaitingValue"
                        XmlDoc = Some (PreXmlDoc.create "Waiting to receive a value for the key we've already consumed")
                        Access = None
                    }
            ]
            |> SynTypeDefnRepr.union
            |> SynTypeDefn.create (
                SynComponentInfo.create parseStateIdent
                |> SynComponentInfo.setAccessibility (Some (SynAccess.Private range0))
            )
            |> List.singleton
            |> SynModuleDecl.createTypes

        let taggedMod =
            let argsParam =
                SynPat.named "args"
                |> SynPat.annotateType (SynType.appPostfix "list" SynType.string)

            let parsePrime =
                SynExpr.CreateConst "todo"
                |> SynExpr.applyFunction (SynExpr.createIdent "failwith")
                |> SynBinding.basic
                    [ Ident.create "parse'" ]
                    [
                        SynPat.named "getEnvironmentVariable"
                        |> SynPat.annotateType (SynType.funFromDomain SynType.string SynType.string)
                        argsParam
                    ]
                |> SynBinding.withReturnAnnotation (SynType.createLongIdent [ taggedType.Name ])

            let parsePrimeCall =
                if spec.ExtensionMethods then
                    // need to fully qualify
                    [ taggedType.Name ; Ident.create "parse'" ]
                else
                    [ Ident.create "parse'" ]

            let parse =
                SynExpr.createLongIdent' parsePrimeCall
                |> SynExpr.applyTo (SynExpr.createLongIdent [ "System" ; "Environment" ; "GetEnvironmentVariable" ])
                |> SynExpr.applyTo (SynExpr.createIdent "args")
                |> SynBinding.basic [ Ident.create "parse" ] [ argsParam ]
                |> SynBinding.withReturnAnnotation (SynType.createLongIdent [ taggedType.Name ])

            [
                yield parseStateType

                if spec.ExtensionMethods then
                    let bindingPrime = parsePrime |> SynMemberDefn.staticMember

                    let binding = parse |> SynMemberDefn.staticMember

                    let componentInfo =
                        SynComponentInfo.create taggedType.Name
                        |> SynComponentInfo.withDocString (PreXmlDoc.create "Extension methods for argument parsing")

                    let containingType =
                        SynTypeDefnRepr.augmentation ()
                        |> SynTypeDefn.create componentInfo
                        |> SynTypeDefn.withMemberDefns [ bindingPrime ; binding ]

                    yield SynModuleDecl.createTypes [ containingType ]
                else
                    yield SynModuleDecl.createLet parsePrime

                    yield SynModuleDecl.createLet parse
            ]
            |> SynModuleDecl.nestedModule modInfo

        [
            for openStatement in opens do
                yield SynModuleDecl.openAny openStatement
            yield taggedMod
        ]
        |> SynModuleOrNamespace.createNamespace ns

open Myriad.Core

/// Myriad generator that provides a catamorphism for an algebraic data type.
[<MyriadGenerator("arg-parser")>]
type ShibaGenerator () =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = [ ".fs" ]

        member _.Generate (context : GeneratorContext) =
            // try
            //     System.IO.File.Delete "/tmp/myriad.log"
            // with
            // | _ -> ()

            let ast, _ =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            let types =
                // Bug in WoofWare.Whippet, probably: we return types in the wrong order
                Ast.getTypes ast |> List.map (fun (ns, types) -> ns, List.rev types)

            let opens = AstHelper.extractOpens ast

            let namespaceAndTypes =
                types
                |> List.collect (fun (ns, types) ->
                    let typeWithAttr =
                        types
                        |> List.choose (fun ty ->
                            match SynTypeDefn.getAttribute typeof<ArgParserAttribute>.Name ty with
                            | None -> None
                            | Some attr ->
                                let arg =
                                    match SynExpr.stripOptionalParen attr.ArgExpr with
                                    | SynExpr.Const (SynConst.Bool value, _) -> value
                                    | SynExpr.Const (SynConst.Unit, _) -> ArgParserAttribute.DefaultIsExtensionMethod
                                    | arg ->
                                        failwith
                                            $"Unrecognised argument %+A{arg} to [<%s{nameof ArgParserAttribute}>]. Literals are not supported. Use `true` or `false` (or unit) only."

                                let spec =
                                    {
                                        ExtensionMethods = arg
                                    }

                                Some (ty, spec)
                        )

                    typeWithAttr
                    |> List.map (fun taggedType ->
                        let unions, records, others =
                            (([], [], []), types)
                            ||> List.fold (fun
                                               (unions, records, others)
                                               (SynTypeDefn.SynTypeDefn (sci, repr, smd, _, _, _) as ty) ->
                                match repr with
                                | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union (access, cases, _), _) ->
                                    UnionType.OfUnion sci smd access cases :: unions, records, others
                                | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (access, fields, _), _) ->
                                    unions, RecordType.OfRecord sci smd access fields :: records, others
                                | _ -> unions, records, ty :: others
                            )

                        if not others.IsEmpty then
                            failwith
                                $"Error: all types recursively defined together with a ShibaGenerator type must be discriminated unions or records. %+A{others}"

                        (ns, taggedType, unions, records)
                    )
                )

            let unionsAndRecordsByNs =
                (Map.empty, namespaceAndTypes)
                ||> List.fold (fun types (ns, _, unions, records) ->
                    let nsKey = ns |> List.map _.idText |> String.concat "."

                    types
                    |> Map.change
                        nsKey
                        (fun v ->
                            match v with
                            | None -> Some (unions, records)
                            | Some (u, r) -> Some (unions @ u, records @ r)
                        )
                )

            let structuresWithinNs =
                unionsAndRecordsByNs
                |> Map.map (fun _ (us, rs) -> ShibaGenerator.parseStructureWithinNs us rs)

            let helpersMod =
                structuresWithinNs
                |> Map.toSeq
                |> Seq.map (fun (ns, info) ->
                    ShibaGenerator.createHelpersModule opens (ns.Split '.' |> Seq.map Ident.create |> List.ofSeq) info
                )
                |> Seq.toList
                |> fun l -> [ yield! l ]
                |> SynModuleOrNamespace.createNamespace [ Ident.create "ArgParserHelpers" ]

            let modules =
                namespaceAndTypes
                |> List.map (fun (ns, taggedType, unions, records) ->
                    ShibaGenerator.createModule opens ns taggedType unions records
                )

            Output.Ast (helpersMod :: modules)
