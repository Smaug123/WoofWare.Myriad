namespace WoofWare.Myriad.Plugins

open System
open System.Collections.Generic
open System.Text
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range
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
            /// Any of the forms in this set are acceptable, but make sure they all start with a dash, or we might
            /// get confused with positional args or something! I haven't thought that hard about this.
            /// In the default case, this is `Const("arg-name")` for the `ArgName : blah` field; note that we have
            /// omitted the initial `--` that will be required at runtime.
            ArgForm : SynExpr list
            /// Name of the field of the in-progress record storing this leaf.
            TargetConstructionField : Ident
            /// If this is a boolean-like field (e.g. a bool or a flag DU), the help text should look a bit different:
            /// we should lie to the user about the value of the cases there.
            /// Similarly, if we're reading from an environment variable with the laxer parsing rules of accepting e.g.
            /// "0" instead of "false", we need to know if we're reading a bool.
            /// In that case, `boolCases` is Some, and contains the construction of the flag (or boolean, in which case
            /// you get no data).
            BoolCases : Choice<FlagDu, unit> option
        }

        /// A SynExpr of type `string` which we can display to the user at generated-program runtime to display all
        /// the ways they can refer to this arg.
        member arg.HumanReadableArgForm : SynExpr =
            let formatString = List.replicate arg.ArgForm.Length "--%s" |> String.concat " / "

            (SynExpr.applyFunction (SynExpr.createIdent "sprintf") (SynExpr.CreateConst formatString), arg.ArgForm)
            ||> List.fold SynExpr.applyFunction
            |> SynExpr.paren

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

        let longForms =
            attrs
            |> List.choose (fun attr ->
                match attr.TypeName with
                | SynLongIdent.SynLongIdent (ident, _, _) ->
                    match (List.last ident).idText with
                    | "ArgumentLongForm"
                    | "ArgumentLongFormAttribute" -> Some attr.ArgExpr
                    | _ -> None
            )
            |> function
                | [] -> List.singleton (SynExpr.CreateConst (argify fieldName))
                | l -> List.ofSeq l

        match ty with
        | String ->
            {
                ParseFn = SynExpr.createLambda "x" (SynExpr.createIdent "x")
                Acc = Accumulation.Required
                TypeAfterParse = SynType.string
                Positional = positional
                ArgForm = longForms
                TargetConstructionField = fieldName
                BoolCases = None
            }
            |> ParseFunctionSpec.Leaf
        | PrimitiveType pt ->
            let isBoolLike =
                if pt |> List.map _.idText = [ "System" ; "Boolean" ] then
                    Some (Choice2Of2 ())
                else
                    identifyAsFlag flagDus ty |> Option.map Choice1Of2

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
                ArgForm = longForms
                TargetConstructionField = fieldName
                BoolCases = isBoolLike
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
                ArgForm = longForms
                TargetConstructionField = fieldName
                BoolCases = None
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
                ArgForm = longForms
                TargetConstructionField = fieldName
                BoolCases = None
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
                ArgForm = longForms
                TargetConstructionField = fieldName
                BoolCases = None
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
                ArgForm = longForms
                TargetConstructionField = fieldName
                BoolCases = None
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
                    ArgForm = longForms
                    TargetConstructionField = fieldName
                    BoolCases = Some (Choice1Of2 flagDu)
                }
                |> ParseFunctionSpec.Leaf

    type internal DatalessUnion =
        {
            Cases : (string * SynAttribute list) list
        }

    type internal ParsedRecordStructure<'choice> =
        {
            NameOfInProgressType : Ident
            Original : RecordType
            /// Map of field name to parser for that field
            LeafNodes : Map<string, LeafData<'choice>>
            Records : Map<string, ParsedRecordStructure<'choice>>
            Unions : Map<string, ParsedUnionStructure<'choice>>
            FlagDus : FlagDu list
        }

    and internal ParsedUnionStructure<'choice> =
        {
            Original : UnionType
            Cases : Map<string, ParsedRecordStructure<'choice>>
        }

    /// `member this.SetFlagValue_ (errors_ : ResizeArray<string>) (key : string) : bool = ...`
    /// The second member of the `flags` list tuple is the constant "true" with which we will interpret the
    /// arity-0 `--foo`. So in the case of a boolean-typed field, this is `true`; in the case of a Flag-typed field,
    /// this is `FlagType.WhicheverCaseHadTrue`.
    let private setFlagValue (flags : (LeafData<'choice> * SynExpr) list) : SynBinding =
        (SynExpr.CreateConst false, flags)
        ||> List.fold (fun finalExpr (flag, trueCase) ->
            let multipleErrorMessage =
                SynExpr.createIdent "sprintf"
                |> SynExpr.applyTo (SynExpr.CreateConst "Flag '%s' was supplied multiple times")
                |> SynExpr.applyTo flag.HumanReadableArgForm

            let matchFlag =
                [
                    SynMatchClause.create
                        (SynPat.nameWithArgs "Some" [ SynPat.anon ])
                        // This is an error, but it's one we can gracefully report at the end.
                        (SynExpr.sequential
                            [
                                multipleErrorMessage
                                |> SynExpr.pipeThroughFunction (SynExpr.dotGet "Add" (SynExpr.createIdent "errors_"))
                                SynExpr.CreateConst true
                            ])

                    SynMatchClause.create
                        (SynPat.named "None")
                        ([
                            SynExpr.assign
                                (SynLongIdent.create [ Ident.create "this" ; flag.TargetConstructionField ])
                                (SynExpr.pipeThroughFunction (SynExpr.createIdent "Some") trueCase)
                            SynExpr.CreateConst true
                         ]
                         |> SynExpr.sequential)
                ]
                |> SynExpr.createMatch (
                    SynExpr.createLongIdent' [ Ident.create "this" ; flag.TargetConstructionField ]
                )

            (finalExpr, flag.ArgForm)
            ||> List.fold (fun finalExpr argForm ->
                SynExpr.ifThenElse
                    (SynExpr.applyFunction
                        (SynExpr.createLongIdent [ "System" ; "String" ; "Equals" ])
                        (SynExpr.tuple
                            [
                                SynExpr.createIdent "key"
                                SynExpr.applyFunction
                                    (SynExpr.applyFunction
                                        (SynExpr.createIdent "sprintf")
                                        (SynExpr.CreateConst "--%s"))
                                    argForm
                                SynExpr.createLongIdent [ "System" ; "StringComparison" ; "OrdinalIgnoreCase" ]
                            ]))
                    finalExpr
                    matchFlag
            )
        )
        |> SynBinding.basic
            [ Ident.create "this" ; Ident.create "SetFlagValue_" ]
            [
                SynPat.annotateType (SynType.app "ResizeArray" [ SynType.string ]) (SynPat.named "errors_")
                SynPat.annotateType SynType.string (SynPat.named "key")
            ]
        |> SynBinding.withReturnAnnotation (SynType.named "bool")
        |> SynBinding.withXmlDoc (PreXmlDoc.create "Returns false if we didn't set a value.")
        |> SynBinding.makeInstanceMember

    /// `member this.ProcessKeyValueRecord_ (errors_ : ResizeArray<string>) (key : string) (value : string) : Result<unit, string option> = ...`
    /// Returns a possible error.
    /// A parse failure might not be fatal (e.g. maybe the input was optionally of arity 0, and we failed to do
    /// the parse because in fact the key decided not to take this argument); in that case we return Error None.
    ///
    /// `args` is a list of the name of the field and the structure which is that field's contents.
    let private processKeyValueRecord<'choice> (args : (string * ParsedRecordStructure<'choice>) list) : SynBinding =
        (SynExpr.applyFunction (SynExpr.createIdent "Error") (SynExpr.createIdent "None"), args)
        ||> List.fold (fun finalBranch (fieldName, _record) ->
            [
                SynMatchClause.create
                    (SynPat.nameWithArgs "Ok" [ SynPat.unit ])
                    (SynExpr.applyFunction (SynExpr.createIdent "Ok") (SynExpr.CreateConst ()))
                SynMatchClause.create
                    (SynPat.nameWithArgs "Error" [ SynPat.named "e" ])
                    (SynExpr.sequential
                        [

                            finalBranch
                        ])
            ]
            |> SynExpr.createMatch (
                SynExpr.createLongIdent [ "this" ; fieldName ; "ProcessKeyValue" ]
                |> SynExpr.applyTo (SynExpr.createIdent "errors_")
                |> SynExpr.applyTo (SynExpr.createIdent "key")
                |> SynExpr.applyTo (SynExpr.createIdent "value")
            )
        )
        |> SynExpr.createLet
            [
                SynBinding.basic
                    [ Ident.create "errors" ]
                    []
                    (SynExpr.applyFunction (SynExpr.createIdent "ResizeArray") (SynExpr.CreateConst ()))
                |> SynBinding.withReturnAnnotation (SynType.app "ResizeArray" [ SynType.string ])
            ]
        |> SynBinding.basic
            [ Ident.create "this" ; Ident.create "ProcessKeyValueRecord_" ]
            [
                SynPat.annotateType (SynType.app "ResizeArray" [ SynType.string ]) (SynPat.named "errors_")
                SynPat.annotateType SynType.string (SynPat.named "key")
                SynPat.annotateType SynType.string (SynPat.named "value")
            ]
        |> SynBinding.withReturnAnnotation (
            SynType.app "Result" [ SynType.unit ; SynType.appPostfix "option" SynType.string ]
        )
        |> SynBinding.withXmlDoc (
            [
                " Passes the key-value pair to any child records, returning Error if no key was matched."
                " If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>)."
                " This can nevertheless be a successful parse, e.g. when the key may have arity 0."
            ]
            |> PreXmlDoc.create'
        )
        |> SynBinding.makeInstanceMember

    /// `member this.ProcessKeyValueSelf_ (errors_ : ResizeArray<string>) (key : string) (value : string) : Result<unit, string option> = ...`
    /// Returns a possible error.
    /// A parse failure might not be fatal (e.g. maybe the input was optionally of arity 0, and we failed to do
    /// the parse because in fact the key decided not to take this argument); in that case we return Error None.
    let private processKeyValueSelf<'choice> (args : LeafData<'choice> list) : SynBinding =
        let args =
            args
            |> List.map (fun arg ->
                match arg.Acc with
                | Accumulation.Required
                | Accumulation.Choice _
                | Accumulation.ChoicePositional _
                | Accumulation.Optional ->
                    let multipleErrorMessage =
                        SynExpr.createIdent "sprintf"
                        |> SynExpr.applyTo (SynExpr.CreateConst "Argument '%s' was supplied multiple times: %s and %s")
                        |> SynExpr.applyTo arg.HumanReadableArgForm
                        |> SynExpr.applyTo (SynExpr.createIdent "x" |> SynExpr.callMethod "ToString" |> SynExpr.paren)
                        |> SynExpr.applyTo (
                            SynExpr.createIdent "value" |> SynExpr.callMethod "ToString" |> SynExpr.paren
                        )

                    let performAssignment =
                        [
                            SynExpr.createIdent "value"
                            |> SynExpr.pipeThroughFunction arg.ParseFn
                            |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Some")
                            |> SynExpr.assign (
                                SynLongIdent.create [ Ident.create "this" ; arg.TargetConstructionField ]
                            )

                            SynExpr.applyFunction (SynExpr.createIdent "Ok") (SynExpr.CreateConst ())
                        ]
                        |> SynExpr.sequential

                    [
                        SynMatchClause.create
                            (SynPat.nameWithArgs "Some" [ SynPat.named "x" ])
                            (SynExpr.sequential
                                [
                                    multipleErrorMessage
                                    |> SynExpr.pipeThroughFunction (
                                        SynExpr.dotGet "Add" (SynExpr.createIdent "errors_")
                                    )
                                    SynExpr.applyFunction (SynExpr.createIdent "Ok") (SynExpr.CreateConst ())
                                ])
                        SynMatchClause.create
                            (SynPat.named "None")
                            (SynExpr.pipeThroughTryWith
                                SynPat.anon
                                (SynExpr.createLongIdent [ "exc" ; "Message" ]
                                 |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Some")
                                 |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Error"))
                                performAssignment)
                    ]
                    |> SynExpr.createMatch (
                        SynExpr.createLongIdent' [ Ident.create "this" ; arg.TargetConstructionField ]
                    )
                | Accumulation.List (Accumulation.List _)
                | Accumulation.List Accumulation.Optional
                | Accumulation.List (Accumulation.Choice _) ->
                    failwith
                        "WoofWare.Myriad invariant violated: expected a list to contain only a Required accumulation. Non-positional lists cannot be optional or Choice, nor can they themselves contain lists."
                | Accumulation.List (Accumulation.ChoicePositional _)
                // ChoicePositional gets aggregated just like any other arg into its containing list;
                // it's only when freezing the in-progress structure that we annotate them with choice information.
                | Accumulation.List Accumulation.Required ->
                    [
                        SynExpr.createIdent "value"
                        |> SynExpr.pipeThroughFunction arg.ParseFn
                        |> SynExpr.pipeThroughFunction (
                            SynExpr.createLongIdent'
                                [ Ident.create "this" ; arg.TargetConstructionField ; Ident.create "Add" ]
                        )
                        SynExpr.CreateConst () |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Ok")
                    ]
                    |> SynExpr.sequential
                |> fun expr -> arg.ArgForm, expr
            )

        // let posArg =
        //     SynExpr.createIdent "value"
        //     |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent ["positionals" ; "Add"])
        //     |> List.singleton

        (SynExpr.applyFunction (SynExpr.createIdent "Error") (SynExpr.createIdent "None"), args)
        ||> List.fold (fun finalBranch (argForm, arg) ->
            (finalBranch, argForm)
            ||> List.fold (fun finalBranch argForm ->
                arg
                |> SynExpr.ifThenElse
                    (SynExpr.applyFunction
                        (SynExpr.createLongIdent [ "System" ; "String" ; "Equals" ])
                        (SynExpr.tuple
                            [
                                SynExpr.createIdent "key"
                                SynExpr.applyFunction
                                    (SynExpr.applyFunction
                                        (SynExpr.createIdent "sprintf")
                                        (SynExpr.CreateConst "--%s"))
                                    argForm
                                SynExpr.createLongIdent [ "System" ; "StringComparison" ; "OrdinalIgnoreCase" ]
                            ]))
                    finalBranch
            )
        )
        |> SynBinding.basic
            [ Ident.create "this" ; Ident.create "ProcessKeyValueSelf_" ]
            [
                SynPat.annotateType (SynType.app "ResizeArray" [ SynType.string ]) (SynPat.named "errors_")
                SynPat.annotateType SynType.string (SynPat.named "key")
                SynPat.annotateType SynType.string (SynPat.named "value")
            ]
        |> SynBinding.withReturnAnnotation (
            SynType.app "Result" [ SynType.unit ; SynType.appPostfix "option" SynType.string ]
        )
        |> SynBinding.withXmlDoc (
            [
                " Processes the key-value pair, returning Error if no key was matched."
                " If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>)."
                " This can nevertheless be a successful parse, e.g. when the key may have arity 0."
            ]
            |> PreXmlDoc.create'
        )
        |> SynBinding.makeInstanceMember

    /// Build the "in-progress record" which is basically "the input record, but with all fields mutable and optional".
    let private inProgressRecordType (record : ParsedRecordStructure<ArgumentDefaultSpec>) : RecordType =
        let leafFields =
            record.LeafNodes
            |> Map.toSeq
            |> Seq.map (fun (ident, data) ->
                let ty, mutability =
                    match data.Acc with
                    | Accumulation.Choice _ -> SynType.option data.TypeAfterParse, true
                    | Accumulation.ChoicePositional _ -> failwith "TODO"
                    | Accumulation.List acc ->
                        SynType.app' (SynType.createLongIdent' [ "ResizeArray" ]) [ data.TypeAfterParse ], false
                    | Accumulation.Optional -> SynType.option data.TypeAfterParse, true
                    | Accumulation.Required -> SynType.option data.TypeAfterParse, true

                {
                    Attrs = []
                    Type = ty
                    Ident = Some (Ident.create ident)
                }
                |> SynField.make
                |> SynField.withMutability mutability
            )
            |> Seq.toList

        let unionFields =
            record.Unions
            |> Map.toSeq
            |> Seq.map (fun (ident, data) -> failwith "TODO")
            |> Seq.toList

        let recordFields =
            record.Records
            |> Map.toSeq
            |> Seq.map (fun (ident, data) ->
                {
                    Attrs = []
                    Ident = Ident.create ident |> Some
                    Type = SynType.createLongIdent [ data.NameOfInProgressType ]
                }
                |> SynField.make
            )
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

        let assembleMethod =
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
                |> fun record ->
                    SynExpr.tupleNoParen
                        [
                            record
                            SynExpr.applyFunction
                                (SynExpr.createLongIdent [ "Seq" ; "tryExactlyOne" ])
                                (SynExpr.createIdent "positionalConsumers")
                        ]
                |> SynExpr.paren
                |> SynExpr.applyFunction (SynExpr.createIdent "Ok")

            let defaultOf =
                SynExpr.typeApp [ SynType.anon ] (SynExpr.createLongIdent [ "Unchecked" ; "defaultof" ])

            let assignVariables =
                record.Original.Fields
                |> List.mapi (fun i f -> (i, f))
                |> List.collect (fun (i, SynField.SynField (fieldType = ty ; idOpt = ident)) ->
                    match ident with
                    | None ->
                        failwith
                            $"expected field in record %s{record.Original.Name.idText} to have a name, but it did not"
                    | Some ident ->

                    let valueForThisVar =
                        match record.Records |> Map.tryFind ident.idText with
                        | Some subRecord ->
                            // This was a record; defer to its parser.
                            let subAssembleCall =
                                SynExpr.dotGet ident.idText (SynExpr.createIdent "this")
                                |> SynExpr.callMethodArg "Assemble_" (SynExpr.createIdent "getEnvironmentVariable")
                                |> SynExpr.applyTo (SynExpr.createIdent "positionals")

                            // TODO: need to know if it has positionals
                            [
                                SynMatchClause.create
                                    (SynPat.identWithArgs
                                        [ Ident.create "Ok" ]
                                        (SynArgPats.create
                                            [ SynPat.named "result" ; SynPat.named "consumedPositional" ]))
                                    (SynExpr.sequential
                                        [
                                            SynExpr.createMatch
                                                (SynExpr.createIdent "consumedPositional")
                                                [
                                                    SynMatchClause.create
                                                        (SynPat.named "None")
                                                        (SynExpr.CreateConst ())
                                                    SynMatchClause.create
                                                        (SynPat.nameWithArgs
                                                            "Some"
                                                            [ SynPat.named "positionalConsumer" ])
                                                        (SynExpr.callMethodArg
                                                            "Add"
                                                            (SynExpr.createIdent "positionalConsumer")
                                                            (SynExpr.createIdent "positionalConsumers"))
                                                ]
                                            SynExpr.createIdent "result"
                                        ])
                                SynMatchClause.create
                                    (SynPat.identWithArgs
                                        [ Ident.create "Error" ]
                                        (SynArgPats.create [ SynPat.named "err" ]))
                                    (SynExpr.sequential
                                        [
                                            SynExpr.callMethodArg
                                                "AddRange"
                                                (SynExpr.createIdent "err")
                                                (SynExpr.createIdent "errors")
                                            defaultOf
                                        ])
                            ]
                            |> SynExpr.createMatch subAssembleCall
                        | None ->

                        match record.Unions |> Map.tryFind ident.idText with
                        | Some union ->
                            // This was a union; defer to its parser.
                            failwith "TODO"
                        | None ->

                        match record.LeafNodes |> Map.tryFind ident.idText with
                        | Some leaf ->
                            match leaf.Positional with
                            // TODO: account for includeFlagLike
                            | Some includeFlagLike ->
                                // Positional args carried in from external argument.
                                // TODO: register whether they came before or after separator
                                match leaf.Acc with
                                | List acc ->
                                    match acc with
                                    | Accumulation.List _ ->
                                        failwith "unexpected: positional args should not be a list of lists"
                                    | Accumulation.Required ->
                                        // TODO: we need to preserve the ordering on these with respect to
                                        // the explicitly passed `--foo=` positionals
                                        SynExpr.createIdent "positionals"
                                        |> SynExpr.pipeThroughFunction (
                                            SynExpr.applyFunction
                                                (SynExpr.createLongIdent [ "Seq" ; "map" ])
                                                (SynExpr.createLambda
                                                    "x"
                                                    (SynExpr.createMatch
                                                        (SynExpr.createIdent "x")
                                                        [
                                                            SynMatchClause.create
                                                                (SynPat.identWithArgs
                                                                    [ Ident.create "Choice1Of2" ]
                                                                    (SynArgPats.createNamed [ "x" ]))
                                                                (SynExpr.createIdent "x")
                                                            SynMatchClause.create
                                                                (SynPat.identWithArgs
                                                                    [ Ident.create "Choice2Of2" ]
                                                                    (SynArgPats.createNamed [ "x" ]))
                                                                (SynExpr.createIdent "x")
                                                        ]))
                                        )
                                        |> SynExpr.pipeThroughFunction (
                                            SynExpr.applyFunction
                                                (SynExpr.createLongIdent [ "Seq" ; "map" ])
                                                leaf.ParseFn
                                        )
                                        // TODO and this will have to account for the ordering
                                        |> SynExpr.pipeThroughFunction (
                                            SynExpr.createLambda
                                                "x"
                                                (SynExpr.createLongIdent [ "Seq" ; "append" ]
                                                 |> SynExpr.applyTo (
                                                     SynExpr.createLongIdent'
                                                         [ Ident.create "this" ; leaf.TargetConstructionField ]
                                                 )
                                                 |> SynExpr.applyTo (SynExpr.createIdent "x"))
                                        )
                                        |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "Seq" ; "toList" ])
                                    | Accumulation.Optional ->
                                        failwith "unexpected: positional args should not be a list of options"
                                    | Accumulation.Choice _ ->
                                        failwith
                                            "internal error: positional args, if Choicey, should be a ChoicePositional"
                                    | Accumulation.ChoicePositional attrContents ->
                                        SynExpr.createIdent "positionals"
                                        |> SynExpr.pipeThroughFunction (
                                            SynExpr.applyFunction
                                                (SynExpr.createLongIdent [ "List" ; "map" ])
                                                (SynExpr.createLambda
                                                    "x"
                                                    (SynExpr.createMatch
                                                        (SynExpr.createIdent "x")
                                                        [
                                                            SynMatchClause.create
                                                                (SynPat.identWithArgs
                                                                    [ Ident.create "Choice1Of2" ]
                                                                    (SynArgPats.createNamed [ "x" ]))
                                                                (SynExpr.applyFunction
                                                                    leaf.ParseFn
                                                                    (SynExpr.createIdent "x")
                                                                 |> SynExpr.pipeThroughFunction (
                                                                     SynExpr.createIdent "Choice1Of2"
                                                                 ))
                                                            SynMatchClause.create
                                                                (SynPat.identWithArgs
                                                                    [ Ident.create "Choice2Of2" ]
                                                                    (SynArgPats.createNamed [ "x" ]))
                                                                (SynExpr.applyFunction
                                                                    leaf.ParseFn
                                                                    (SynExpr.createIdent "x")
                                                                 |> SynExpr.pipeThroughFunction (
                                                                     SynExpr.createIdent "Choice2Of2"
                                                                 ))
                                                        ]))
                                        )
                                | _ -> failwith "unexpected: positional arguments should be a list"
                            | None ->

                            let parseFn =
                                match leaf.BoolCases with
                                | Some boolLike ->
                                    let trueCase, falseCase =
                                        match boolLike with
                                        | Choice2Of2 () -> SynExpr.CreateConst true, SynExpr.CreateConst false
                                        | Choice1Of2 flag ->
                                            FlagDu.FromBoolean flag (SynExpr.CreateConst true),
                                            FlagDu.FromBoolean flag (SynExpr.CreateConst false)

                                    // We permit environment variables to be populated with 0 and 1 as well.
                                    SynExpr.ifThenElse
                                        (SynExpr.applyFunction
                                            (SynExpr.createLongIdent [ "System" ; "String" ; "Equals" ])
                                            (SynExpr.tuple
                                                [
                                                    SynExpr.createIdent "x"
                                                    SynExpr.CreateConst "1"
                                                    SynExpr.createLongIdent
                                                        [ "System" ; "StringComparison" ; "OrdinalIgnoreCase" ]
                                                ]))
                                        (SynExpr.ifThenElse
                                            (SynExpr.applyFunction
                                                (SynExpr.createLongIdent [ "System" ; "String" ; "Equals" ])
                                                (SynExpr.tuple
                                                    [
                                                        SynExpr.createIdent "x"
                                                        SynExpr.CreateConst "0"
                                                        SynExpr.createLongIdent
                                                            [ "System" ; "StringComparison" ; "OrdinalIgnoreCase" ]
                                                    ]))
                                            (SynExpr.createIdent "x" |> SynExpr.pipeThroughFunction leaf.ParseFn)
                                            falseCase)
                                        trueCase
                                    |> SynExpr.createLambda "x"
                                | None -> leaf.ParseFn

                            let extract =
                                match leaf.Acc with
                                | Accumulation.ChoicePositional choice -> failwith "TODO"
                                | Accumulation.Choice choice ->
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
                                            (match choice with
                                             | ArgumentDefaultSpec.EnvironmentVariable var ->
                                                 var
                                                 |> SynExpr.pipeThroughFunction (
                                                     SynExpr.createIdent "getEnvironmentVariable"
                                                 )
                                                 |> SynExpr.pipeThroughFunction parseFn
                                             | ArgumentDefaultSpec.FunctionCall name ->
                                                 SynExpr.callMethod
                                                     name.idText
                                                     (SynExpr.createIdent' record.Original.Name)
                                             |> SynExpr.paren
                                             |> SynExpr.applyFunction (SynExpr.createIdent "Choice2Of2"))
                                    ]
                                    |> SynExpr.createMatch (SynExpr.dotGet ident.idText (SynExpr.createIdent "this"))
                                | Accumulation.List acc ->
                                    // TODO: use the acc here too?!
                                    SynExpr.dotGet ident.idText (SynExpr.createIdent "this")
                                    |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "Seq" ; "toList" ])
                                | Accumulation.Optional -> SynExpr.dotGet ident.idText (SynExpr.createIdent "this")
                                | Accumulation.Required ->
                                    // fall back to assuming it's basically primitive
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
                                                        (leaf.ArgForm.[0]
                                                         |> SynExpr.applyFunction (
                                                             SynExpr.CreateConst
                                                                 "Required argument '--%s' received no value"
                                                             |> SynExpr.applyFunction (SynExpr.createIdent "sprintf")
                                                         )
                                                         |> SynExpr.paren)
                                                        (SynExpr.createIdent "errors")
                                                    defaultOf
                                                ])
                                    ]
                                    |> SynExpr.createMatch (SynExpr.dotGet ident.idText (SynExpr.createIdent "this"))

                            extract
                        | None ->
                            failwith
                                $"somehow we never classified the field %s{ident.idText} of %s{record.Original.Name.idText}"

                    valueForThisVar
                    |> SynBinding.basic [ Ident.create $"arg%i{i}" ] []
                    |> SynBinding.withReturnAnnotation ty
                    |> List.singleton
                )

            instantiation
            |> SynExpr.ifThenElse
                (SynExpr.lessThanOrEqual
                    (SynExpr.CreateConst 1)
                    (SynExpr.dotGet "Count" (SynExpr.createIdent "positionalConsumers")))
                (SynExpr.createIdent "positionalConsumers"
                 |> SynExpr.applyFunction (
                     SynExpr.applyFunction (SynExpr.createLongIdent [ "String" ; "concat" ]) (SynExpr.CreateConst ", ")
                 )
                 |> SynExpr.plus (SynExpr.CreateConst "Multiple parsers consumed positional args: ")
                 |> SynExpr.paren
                 |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "List" ; "singleton" ])
                 |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Error"))
            |> SynExpr.ifThenElse
                (SynExpr.equals (SynExpr.dotGet "Count" (SynExpr.createIdent "errors")) (SynExpr.CreateConst 0))
                (SynExpr.createIdent "errors"
                 |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "Seq" ; "toList" ])
                 |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Error"))
            |> SynExpr.createLet assignVariables
            |> SynExpr.createLet
                [
                    SynBinding.basic
                        [ Ident.create "errors" ]
                        []
                        (SynExpr.applyFunction
                            (SynExpr.typeApp [ SynType.string ] (SynExpr.createIdent "ResizeArray"))
                            (SynExpr.CreateConst ()))
                    SynBinding.basic
                        [ Ident.create "positionalConsumers" ]
                        []
                        (SynExpr.applyFunction
                            (SynExpr.typeApp [ SynType.string ] (SynExpr.createIdent "ResizeArray"))
                            (SynExpr.CreateConst ()))
                ]
            |> SynBinding.basic
                [ Ident.create "this" ; Ident.create "Assemble_" ]
                [
                    SynPat.annotateType
                        (SynType.funFromDomain SynType.string SynType.string)
                        (SynPat.named "getEnvironmentVariable")
                    SynPat.annotateType
                        (SynType.list (SynType.app "Choice" [ SynType.string ; SynType.string ]))
                        (SynPat.named "positionals")
                ]
            |> SynBinding.withReturnAnnotation (
                SynType.app
                    "Result"
                    [
                        SynType.tupleNoParen
                            [
                                SynType.createLongIdent [ record.Original.Name ]
                                SynType.option SynType.string
                            ]
                        |> Option.get
                        SynType.list SynType.string
                    ]
            )
            |> SynBinding.withXmlDoc (
                PreXmlDoc.create
                    "Freeze this in-progress type. On success, returns the frozen type and the arg (if any) which consumed the input positional args."
            )
            |> SynMemberDefn.memberImplementation

        let emptyConstructor =
            [
                for KeyValue (nodeName, leaf) in record.LeafNodes do
                    let rhs =
                        match leaf.Acc with
                        | Accumulation.Required
                        | Accumulation.Optional
                        | Accumulation.Choice _ -> SynExpr.createIdent "None"
                        | Accumulation.ChoicePositional _ -> failwith "todo"
                        | Accumulation.List acc ->
                            SynExpr.applyFunction (SynExpr.createIdent "ResizeArray") (SynExpr.CreateConst ())

                    yield SynLongIdent.create [ Ident.create nodeName ], rhs
                for KeyValue (nodeName, subRecord) in record.Records do
                    yield
                        SynLongIdent.create [ Ident.create nodeName ],
                        SynExpr.callMethod "_Empty" (SynExpr.createIdent' subRecord.NameOfInProgressType)
                for KeyValue (nodeName, subUnion) in record.Unions do
                    yield SynLongIdent.create [ Ident.create nodeName ], failwith "TODO"
            ]
            |> SynExpr.createRecord None
            |> SynBinding.basic [ Ident.create "_Empty" ] [ SynPat.unit ]
            |> SynBinding.withReturnAnnotation (SynType.createLongIdent [ record.NameOfInProgressType ])
            |> SynMemberDefn.staticMember

        let processKeyValueSelf =
            if record.LeafNodes.IsEmpty then
                None
            else
                record.LeafNodes
                |> Map.toSeq
                |> Seq.map snd
                |> Seq.toList
                |> processKeyValueSelf
                |> SynMemberDefn.memberImplementation
                |> Some

        let processKeyValueChildRecords =
            if record.Records.IsEmpty then
                None
            else
                record.Records
                |> Map.toSeq
                |> Seq.toList
                |> processKeyValueRecord
                |> SynMemberDefn.memberImplementation
                |> Some

        let processKeyValue =
            let afterErrorFromRecord =
                SynExpr.applyFunction (SynExpr.createIdent "Error") (SynExpr.createIdent "None")

            let afterErrorFromLeaf =
                match processKeyValueChildRecords with
                | None -> afterErrorFromRecord
                | Some _ ->
                    [
                        SynMatchClause.create
                            (SynPat.nameWithArgs "Ok" [ SynPat.unit ])
                            (SynExpr.applyFunction (SynExpr.createIdent "Ok") (SynExpr.CreateConst ()))
                        SynMatchClause.create
                            (SynPat.nameWithArgs "Error" [ SynPat.named "errorFromRecord" ])
                            afterErrorFromRecord
                    ]
                    |> SynExpr.createMatch (
                        SynExpr.createLongIdent [ "this" ; "ProcessKeyValueRecord_" ]
                        |> SynExpr.applyTo (SynExpr.createIdent "errors_")
                        |> SynExpr.applyTo (SynExpr.createIdent "key")
                        |> SynExpr.applyTo (SynExpr.createIdent "value")
                    )

            let firstMatch =
                match processKeyValueSelf with
                | None -> afterErrorFromLeaf
                | Some _ ->
                    [
                        SynMatchClause.create
                            (SynPat.nameWithArgs "Ok" [ SynPat.unit ])
                            (SynExpr.applyFunction (SynExpr.createIdent "Ok") (SynExpr.CreateConst ()))
                        SynMatchClause.create
                            (SynPat.nameWithArgs "Error" [ SynPat.named "errorFromLeaf" ])
                            afterErrorFromLeaf
                    ]
                    |> SynExpr.createMatch (
                        SynExpr.createLongIdent [ "this" ; "ProcessKeyValueSelf_" ]
                        |> SynExpr.applyTo (SynExpr.createIdent "errors_")
                        |> SynExpr.applyTo (SynExpr.createIdent "key")
                        |> SynExpr.applyTo (SynExpr.createIdent "value")
                    )

            firstMatch
            |> SynBinding.basic
                [ Ident.create "this" ; Ident.create "ProcessKeyValue" ]
                [
                    SynPat.annotateType (SynType.app "ResizeArray" [ SynType.string ]) (SynPat.named "errors_")
                    SynPat.annotateType SynType.string (SynPat.named "key")
                    SynPat.annotateType SynType.string (SynPat.named "value")
                ]
            |> SynBinding.withReturnAnnotation (SynType.app "Result" [ SynType.unit ; SynType.option SynType.string ])
            |> SynBinding.makeInstanceMember
            |> SynMemberDefn.memberImplementation

        let flags =
            record.LeafNodes
            |> Map.toSeq
            |> Seq.choose (fun (_, pf) ->
                match pf.Acc with
                | Required
                | Optional
                | Accumulation.Choice _ -> Some pf
                // We don't allow flags to be passed multiple times and accumulated into a list.
                | Accumulation.List _
                | Accumulation.ChoicePositional _ -> None
            )
            |> Seq.choose (fun pf ->
                match pf.TypeAfterParse with
                | PrimitiveType pt ->
                    if (pt |> List.map _.idText) = [ "System" ; "Boolean" ] then
                        Some (pf, SynExpr.CreateConst true)
                    else
                        None
                | ty ->
                    match identifyAsFlag record.FlagDus ty with
                    | Some flag -> (pf, FlagDu.FromBoolean flag (SynExpr.CreateConst true)) |> Some
                    | _ -> None
            )
            |> Seq.toList

        let setFlagValue = setFlagValue flags |> SynMemberDefn.memberImplementation

        {
            Name = record.NameOfInProgressType
            Fields = fields
            Members =
                [
                    Some assembleMethod
                    Some emptyConstructor
                    processKeyValueSelf
                    processKeyValueChildRecords
                    Some processKeyValue
                    Some setFlagValue
                ]
                |> List.choose id
                |> Some
            XmlDoc = PreXmlDoc.create $"A partially-parsed %s{record.Original.Name.idText}." |> Some
            Generics =
                match record.Original.Generics with
                | None -> None
                | Some _ ->
                    failwith $"Record type %s{record.Original.Name.idText} had generics, which we don't support."
            TypeAccessibility = Some (SynAccess.Internal range0)
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
                NameOfInProgressType = rt.Name.idText + "_InProgress" |> Ident.create
                Original = rt
                LeafNodes = leaf |> Map.ofList
                Records = records |> Map.ofList
                Unions = unions |> Map.ofList
                FlagDus = flagDus
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

    let helperModuleName (namespaceName : LongIdent) : Ident =
        let ns = namespaceName |> List.map _.idText |> String.concat "_"
        Ident.create $"ArgParseHelpers_%s{ns}"

    let createHelpersModule (opens : SynOpenDeclTarget list) (ns : LongIdent) (info : AllInfo) : SynModuleDecl =
        let modName = helperModuleName ns

        let modInfo =
            SynComponentInfo.create modName
            |> SynComponentInfo.withAccessibility (SynAccess.Internal range0)
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

    /// `let rec go (state : %ParseState%) (args : string list) : unit = ...`
    let private mainLoop (parseState : Ident) (errorAcc : Ident) (leftoverArgs : Ident) : SynBinding =
        /// `go (AwaitingValue arg) args`
        let recurseValue =
            SynExpr.createIdent "go"
            |> SynExpr.applyTo (
                SynExpr.paren (
                    SynExpr.applyFunction
                        (SynExpr.createLongIdent' [ parseState ; Ident.create "AwaitingValue" ])
                        (SynExpr.createIdent "arg")
                )
            )

        /// `go AwaitingKey args`
        let recurseKey =
            (SynExpr.createIdent "go")
            |> SynExpr.applyTo (SynExpr.createLongIdent' [ parseState ; Ident.create "AwaitingKey" ])
            |> SynExpr.applyTo (SynExpr.createIdent "args")

        /// `failwithf "Unable to process argument ..."`
        let fail =
            SynExpr.createIdent "failwithf"
            |> SynExpr.applyTo (SynExpr.CreateConst "Unable to process argument %s as key %s and value %s")
            |> SynExpr.applyTo (SynExpr.createIdent "arg")
            |> SynExpr.applyTo (SynExpr.createIdent "key")
            |> SynExpr.applyTo (SynExpr.createIdent "value")

        let processAsPositional =
            SynExpr.sequential
                [
                    SynExpr.createIdent "arg"
                    |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Choice1Of2")
                    |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent' [ leftoverArgs ; Ident.create "Add" ])

                    recurseKey
                ]

        let notMatched =
            let handleFailure =
                [
                    SynMatchClause.create (SynPat.named "None") fail

                    SynMatchClause.create
                        (SynPat.nameWithArgs "Some" [ SynPat.named "msg" ])
                        (SynExpr.sequential
                            [
                                SynExpr.createIdent "sprintf"
                                |> SynExpr.applyTo (SynExpr.CreateConst "%s (at arg %s)")
                                |> SynExpr.applyTo (SynExpr.createIdent "msg")
                                |> SynExpr.applyTo (SynExpr.createIdent "arg")
                                |> SynExpr.pipeThroughFunction (SynExpr.dotGet "Add" (SynExpr.createIdent' errorAcc))

                                recurseKey
                            ])
                ]
                |> SynExpr.createMatch (SynExpr.createIdent "x")

            handleFailure

        let argStartsWithDashes =
            SynExpr.createIdent "arg"
            |> SynExpr.callMethodArg
                "StartsWith"
                (SynExpr.tuple
                    [
                        SynExpr.CreateConst "--"
                        SynExpr.createLongIdent [ "System" ; "StringComparison" ; "Ordinal" ]
                    ])

        let processKey =
            SynExpr.ifThenElse
                argStartsWithDashes
                processAsPositional
                (SynExpr.ifThenElse
                    (SynExpr.equals (SynExpr.createIdent "arg") (SynExpr.CreateConst "--help"))
                    (SynExpr.createLet
                        [
                            SynBinding.basic
                                [ Ident.create "equals" ]
                                []
                                (SynExpr.callMethodArg "IndexOf" (SynExpr.CreateConst '=') (SynExpr.createIdent "arg"))
                        ]
                        (SynExpr.ifThenElse
                            (SynExpr.lessThan (SynExpr.CreateConst 0) (SynExpr.createIdent "equals"))
                            (SynExpr.createLet
                                [
                                    SynBinding.basic
                                        [ Ident.create "key" ]
                                        []
                                        (SynExpr.arrayIndexRange
                                            (Some (SynExpr.CreateConst 0))
                                            (Some (SynExpr.minusN (SynLongIdent.createS "equals") 1))
                                            (SynExpr.createIdent "arg"))
                                    SynBinding.basic
                                        [ Ident.create "value" ]
                                        []
                                        (SynExpr.arrayIndexRange
                                            (Some (SynExpr.plus (SynExpr.createIdent "equals") (SynExpr.CreateConst 1)))
                                            None
                                            (SynExpr.createIdent "arg"))
                                ]
                                (SynExpr.createMatch
                                    (SynExpr.callMethodArg
                                        "ProcessKeyValue"
                                        (SynExpr.createIdent "errors_")
                                        (SynExpr.createIdent "inProgress")
                                     |> SynExpr.applyTo (SynExpr.createIdent "key")
                                     |> SynExpr.applyTo (SynExpr.createIdent "value"))
                                    [
                                        SynMatchClause.create (SynPat.nameWithArgs "Ok" [ SynPat.unit ]) recurseKey

                                        SynMatchClause.create
                                            (SynPat.nameWithArgs "Error" [ SynPat.named "x" ])
                                            notMatched
                                    ]))
                            (SynExpr.createIdent "args" |> SynExpr.pipeThroughFunction recurseValue)))
                    ( //SynExpr.createIdent "helpText"
                    //|> SynExpr.applyTo (SynExpr.CreateConst ())
                    SynExpr.CreateConst "TODO"
                    |> SynExpr.pipeThroughFunction (
                        SynExpr.applyFunction
                            (SynExpr.createIdent "failwithf")
                            (SynExpr.CreateConst @"Help text requested.\n%s")
                    )))

        let processValue =
            // During failure, we've received an optional exception message that happened when we tried to parse
            // the value; it's in the variable `exc`.
            // `fail` is for the case where we're genuinely emitting an error.
            // If we're in `PositionalArgs true` mode, though, we won't call `fail`.
            // TODO: unused?!
            let fail =
                [
                    SynExpr.createIdent "failwithf"
                    |> SynExpr.applyTo (
                        SynExpr.CreateConst @"Unable to process supplied arg %s. Help text follows.\n%s"
                    )
                    |> SynExpr.applyTo (SynExpr.createIdent "key")
                    |> SynExpr.applyTo (
                        SynExpr.applyFunction (SynExpr.createIdent "helpText") (SynExpr.CreateConst ())
                        |> SynExpr.paren
                    )
                    |> SynMatchClause.create (SynPat.named "None")

                    SynExpr.createIdent "msg"
                    |> SynExpr.pipeThroughFunction (SynExpr.dotGet "Add" (SynExpr.createIdent' errorAcc))
                    |> SynMatchClause.create (SynPat.nameWithArgs "Some" [ SynPat.named "msg" ])
                ]
                |> SynExpr.createMatch (SynExpr.createIdent "exc")

            let onFailure =
                [
                    SynExpr.createIdent "key"
                    |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Choice1Of2")
                    |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent' [ leftoverArgs ; Ident.create "Add" ])

                    SynExpr.createIdent "go"
                    |> SynExpr.applyTo (SynExpr.createLongIdent' [ parseState ; Ident.create "AwaitingKey" ])
                    |> SynExpr.applyTo (SynExpr.listCons (SynExpr.createIdent "arg") (SynExpr.createIdent "args"))
                ]
                |> SynExpr.sequential

            [
                SynMatchClause.create
                    (SynPat.nameWithArgs "Ok" [ SynPat.unit ])
                    (SynExpr.applyFunction
                        (SynExpr.applyFunction
                            (SynExpr.createIdent "go")
                            (SynExpr.createLongIdent' [ parseState ; Ident.create "AwaitingKey" ]))
                        (SynExpr.createIdent "args"))
                SynMatchClause.create
                    (SynPat.nameWithArgs "Error" [ SynPat.named "exc" ])
                    (SynExpr.ifThenElse
                        (SynExpr.applyFunction
                            (SynExpr.callMethodArg
                                "SetFlagValue_"
                                (SynExpr.createIdent "errors_")
                                (SynExpr.createIdent "inProgress"))
                            (SynExpr.createIdent "key"))
                        onFailure
                        (SynExpr.createIdent "go"
                         |> SynExpr.applyTo (SynExpr.createLongIdent' [ parseState ; Ident.create "AwaitingKey" ])
                         |> SynExpr.applyTo (SynExpr.listCons (SynExpr.createIdent "arg") (SynExpr.createIdent "args"))))
            ]
            |> SynExpr.createMatch (
                SynExpr.applyFunction
                    (SynExpr.applyFunction
                        (SynExpr.callMethodArg
                            "ProcessKeyValue"
                            (SynExpr.createIdent "errors_")
                            (SynExpr.createIdent "inProgress"))
                        (SynExpr.createIdent "key"))
                    (SynExpr.createIdent "arg")
            )

        let argBody =
            [
                SynMatchClause.create
                    (SynPat.identWithArgs [ parseState ; Ident.create "AwaitingKey" ] (SynArgPats.create []))
                    processKey
                SynMatchClause.create
                    (SynPat.identWithArgs
                        [ parseState ; Ident.create "AwaitingValue" ]
                        (SynArgPats.createNamed [ "key" ]))
                    processValue
            ]
            |> SynExpr.createMatch (SynExpr.createIdent "state")

        let body =
            let trailingArgMessage =
                SynExpr.createIdent "sprintf"
                |> SynExpr.applyTo (
                    SynExpr.CreateConst
                        "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                )
                |> SynExpr.applyTo (SynExpr.createIdent "key")

            [
                SynMatchClause.create
                    SynPat.emptyList
                    (SynExpr.createMatch
                        (SynExpr.createIdent "state")
                        [
                            SynMatchClause.create
                                (SynPat.identWithArgs [ parseState ; Ident.create "AwaitingKey" ] (SynArgPats.create []))
                                (SynExpr.CreateConst ())
                            SynMatchClause.create
                                (SynPat.identWithArgs
                                    [ parseState ; Ident.create "AwaitingValue" ]
                                    (SynArgPats.createNamed [ "key" ]))
                                (SynExpr.ifThenElse
                                    (SynExpr.applyFunction
                                        (SynExpr.callMethodArg
                                            "SetFlagValue_"
                                            (SynExpr.createIdent "errors_")
                                            (SynExpr.createIdent "inProgress"))
                                        (SynExpr.createIdent "key"))
                                    (trailingArgMessage
                                     |> SynExpr.pipeThroughFunction (
                                         SynExpr.dotGet "Add" (SynExpr.createIdent' errorAcc)
                                     ))
                                    (SynExpr.CreateConst ()))
                        ])
                SynMatchClause.create
                    (SynPat.listCons (SynPat.createConst (SynConst.Create "--")) (SynPat.named "rest"))
                    (SynExpr.callMethodArg
                        "AddRange"
                        (SynExpr.paren (
                            SynExpr.createIdent "rest"
                            |> SynExpr.pipeThroughFunction (
                                SynExpr.applyFunction
                                    (SynExpr.createLongIdent [ "Seq" ; "map" ])
                                    (SynExpr.createIdent "Choice2Of2")
                            )
                        ))
                        (SynExpr.createIdent' leftoverArgs))
                SynMatchClause.create (SynPat.listCons (SynPat.named "arg") (SynPat.named "args")) argBody
            ]
            |> SynExpr.createMatch (SynExpr.createIdent "args")

        let args =
            [
                SynPat.named "state"
                |> SynPat.annotateType (SynType.createLongIdent [ parseState ])
                SynPat.named "args"
                |> SynPat.annotateType (SynType.appPostfix "list" SynType.string)
            ]

        SynBinding.basic [ Ident.create "go" ] args body
        |> SynBinding.withRecursion true

    // The type for which we're generating args may refer to any of the supplied records/unions.
    let createModule
        (opens : SynOpenDeclTarget list)
        (ns : LongIdent)
        ((taggedType : SynTypeDefn, spec : ArgParserOutputSpec))
        (helperModName : LongIdent)
        (structures : AllInfo)
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

        let taggedTypeInfo = structures.RecordParsers.[taggedType.Name.idText]

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
                |> SynComponentInfo.setAccessibility (Some (SynAccess.Internal range0))
            )
            |> List.singleton
            |> SynModuleDecl.createTypes

        let taggedMod =
            let argsParam =
                SynPat.named "args"
                |> SynPat.annotateType (SynType.appPostfix "list" SynType.string)

            let raiseErrors (errorIdent : Ident) =
                SynExpr.createIdent' errorIdent
                |> SynExpr.pipeThroughFunction (
                    SynExpr.applyFunction
                        (SynExpr.createLongIdent [ "String" ; "concat" ])
                        (SynExpr.createLongIdent [ "System" ; "Environment" ; "NewLine" ])
                )
                |> SynExpr.pipeThroughFunction (
                    SynExpr.createLambda
                        "x"
                        (SynExpr.plus (SynExpr.CreateConst "Errors during parse!\\n") (SynExpr.createIdent "x"))
                )
                |> SynExpr.pipeThroughFunction (SynExpr.createIdent "failwith")

            let parsePrime =
                [
                    SynExpr.applyFunction
                        (SynExpr.createIdent "go")
                        (SynExpr.createLongIdent' [ parseStateIdent ; Ident.create "AwaitingKey" ])
                    |> SynExpr.applyTo (SynExpr.createIdent "args")

                    SynExpr.ifThenElse
                        (SynExpr.dotGet "Count" (SynExpr.createIdent "errors_")
                         |> SynExpr.equals (SynExpr.CreateConst 0))
                        (raiseErrors (Ident.create "errors_"))
                        (SynExpr.CreateConst ())

                    [
                        SynMatchClause.create
                            (SynPat.nameWithArgs
                                "Ok"
                                [ SynPat.tuple [ SynPat.named "result" ; SynPat.named "posConsumer" ] ])
                            (SynExpr.ifThenElse
                                (SynExpr.booleanAnd
                                    (SynExpr.dotGet "Count" (SynExpr.createIdent "positionals")
                                     |> SynExpr.greaterThan (SynExpr.CreateConst 0))
                                    (SynExpr.dotGet "IsNone" (SynExpr.createIdent "posConsumer")))
                                (SynExpr.createIdent "result")
                                (SynExpr.applyFunction (SynExpr.createIdent "failwith") (SynExpr.CreateConst "TODO")))
                        SynMatchClause.create
                            (SynPat.nameWithArgs "Error" [ SynPat.named "e" ])
                            (raiseErrors (Ident.create "e"))
                    ]
                    |> SynExpr.createMatch (
                        SynExpr.callMethodArg
                            "Assemble_"
                            (SynExpr.createIdent "getEnvironmentVariable")
                            (SynExpr.createIdent "inProgress")
                        |> SynExpr.applyTo (
                            SynExpr.createIdent "positionals"
                            |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "Seq" ; "toList" ])
                            |> SynExpr.paren
                        )
                    )
                ]
                |> SynExpr.sequential
                |> SynExpr.createLet
                    [
                        SynBinding.basic
                            [ Ident.create "errors_" ]
                            []
                            (SynExpr.applyFunction (SynExpr.createIdent "ResizeArray") (SynExpr.CreateConst ()))
                        mainLoop parseStateIdent (Ident.create "errors_") (Ident.create "positionals")
                    ]
                |> SynExpr.createLet
                    [
                        SynBinding.basic
                            [ Ident.create "inProgress" ]
                            []
                            (SynExpr.applyFunction
                                (SynExpr.createLongIdent' (
                                    helperModName @ [ taggedTypeInfo.NameOfInProgressType ; Ident.create "_Empty" ]
                                ))
                                (SynExpr.CreateConst ()))

                        SynBinding.basic
                            [ Ident.create "positionals" ]
                            []
                            (SynExpr.applyFunction (SynExpr.createIdent "ResizeArray") (SynExpr.CreateConst ()))
                        |> SynBinding.withReturnAnnotation (
                            SynType.app "ResizeArray" [ SynType.app "Choice" [ SynType.string ; SynType.string ] ]
                        )
                    ]
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

            let helperModNamespaceName = Ident.create "ArgParserHelpers"

            let helpersMod =
                structuresWithinNs
                |> Map.toSeq
                |> Seq.map (fun (ns, info) ->
                    ShibaGenerator.createHelpersModule opens (ns.Split '.' |> Seq.map Ident.create |> List.ofSeq) info
                )
                |> Seq.toList
                |> fun l -> [ yield! l ]
                |> SynModuleOrNamespace.createNamespace [ helperModNamespaceName ]

            let modules =
                namespaceAndTypes
                |> List.map (fun (ns, taggedType, _, _) ->
                    let opens =
                        SynOpenDeclTarget.ModuleOrNamespace (SynLongIdent.create [ helperModNamespaceName ], range0)
                        :: opens

                    ShibaGenerator.createModule
                        opens
                        ns
                        taggedType
                        [ ShibaGenerator.helperModuleName ns ]
                        structuresWithinNs.[ns |> List.map _.idText |> String.concat "."]
                )

            Output.Ast (helpersMod :: modules)
