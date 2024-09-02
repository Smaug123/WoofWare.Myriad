namespace WoofWare.Myriad.Plugins

open System
open System.Text
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range
open Fantomas.FCS.Xml
open Myriad.Core

type internal ArgParserOutputSpec =
    {
        ExtensionMethods : bool
    }

/// The default value of an argument which admits default values can be pulled from different sources.
/// This defines which source a particular default value comes from.
type private ArgumentDefaultSpec =
    /// From parsing the environment variable with the given name (e.g. "WOOFWARE_DISABLE_FOO" or whatever).
    | EnvironmentVariable of name : SynExpr
    /// From calling the static member `{typeWeParseInto}.Default{name}()`
    /// For example, if `type MyArgs = { Thing : Choice<int, int> }`, then
    /// we would use `MyArgs.DefaultThing () : int`.
    | FunctionCall of name : Ident

type private Accumulation =
    | Required
    | Optional
    | Choice of ArgumentDefaultSpec
    | List

type private ParseFunction =
    {
        FieldName : Ident
        TargetVariable : Ident
        ArgForm : string
        Help : SynExpr option
        /// A function string -> %TargetType%, where TargetVariable is probably a `%TargetType% option`.
        /// (Depending on `Accumulation`, we'll remove the `option` at the end of the parse, asserting that the
        /// argument was supplied.)
        /// This is allowed to throw if it fails to parse.
        Parser : SynExpr
        /// If `Accumulation` is `List`, then this is the type of the list *element*; analogously for optionals
        /// and choices and so on.
        TargetType : SynType
        Accumulation : Accumulation
    }

type private ParserSpec =
    {
        NonPositionals : ParseFunction list
        /// The variable into which positional arguments will be accumulated.
        /// In this case, the TargetVariable is a `ResizeArray` rather than the usual `option`.
        Positionals : ParseFunction option
    }

type private ArgToParse =
    | Positional of ParseFunction
    | NonPositional of ParseFunction

[<RequireQualifiedAccess>]
module internal ArgParserGenerator =

    /// Convert e.g. "Foo" into "--foo".
    let argify (ident : Ident) : string =
        let result = StringBuilder ()
        result.Append "-" |> ignore<StringBuilder>

        for c in ident.idText do
            if Char.IsUpper c then
                result.Append('-').Append (Char.ToLowerInvariant c) |> ignore<StringBuilder>
            else
                result.Append c |> ignore<StringBuilder>

        result.ToString ()

    /// Builds a function or lambda of one string argument, which returns a `ty` (as modified by the `Accumulation`;
    /// for example, maybe it returns a `ty option` or a `ty list`).
    /// The resulting SynType is the type of the *element* being parsed; so if the Accumulation is List, the SynType
    /// is the list element.
    let rec private createParseFunction
        (fieldName : Ident)
        (attrs : SynAttribute list)
        (ty : SynType)
        : SynExpr * Accumulation * SynType
        =
        match ty with
        | String -> SynExpr.createLambda "x" (SynExpr.createIdent "x"), Accumulation.Required, SynType.string
        | PrimitiveType pt ->
            SynExpr.createLambda
                "x"
                (SynExpr.applyFunction
                    (SynExpr.createLongIdent' (pt @ [ Ident.create "Parse" ]))
                    (SynExpr.createIdent "x")),
            Accumulation.Required,
            ty
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

            parser, Accumulation.Required, ty
        | FileInfo ->
            SynExpr.createLambda
                "x"
                (SynExpr.applyFunction
                    (SynExpr.createLongIdent [ "System" ; "IO" ; "FileInfo" ])
                    (SynExpr.createIdent "x")),
            Accumulation.Required,
            ty
        | DirectoryInfo ->
            SynExpr.createLambda
                "x"
                (SynExpr.applyFunction
                    (SynExpr.createLongIdent [ "System" ; "IO" ; "DirectoryInfo" ])
                    (SynExpr.createIdent "x")),
            Accumulation.Required,
            ty
        | OptionType eltTy ->
            let parseElt, acc, childTy = createParseFunction fieldName attrs eltTy

            match acc with
            | Accumulation.Optional ->
                failwith
                    $"ArgParser does not support optionals containing options at field %s{fieldName.idText}: %O{ty}"
            | Accumulation.Choice _ ->
                failwith
                    $"ArgParser does not support optionals containing choices at field %s{fieldName.idText}: %O{ty}"
            | Accumulation.List ->
                failwith $"ArgParser does not support optional lists at field %s{fieldName.idText}: %O{ty}"
            | Accumulation.Required -> parseElt, Accumulation.Optional, childTy
        | ChoiceType elts ->
            match elts with
            | [ elt1 ; elt2 ] ->
                if not (SynType.provablyEqual elt1 elt2) then
                    failwith
                        $"ArgParser was unable to prove types %O{elt1} and %O{elt2} to be equal in a Choice. We require them to be equal."

                let parseElt, acc, childTy = createParseFunction fieldName attrs elt1

                match acc with
                | Accumulation.Optional ->
                    failwith
                        $"ArgParser does not support choices containing options at field %s{fieldName.idText}: %O{ty}"
                | Accumulation.List ->
                    failwith
                        $"ArgParser does not support choices containing lists at field %s{fieldName.idText}: %O{ty}"
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
                    | [] ->
                        failwith
                            $"Expected Choice to be annotated with ArgumentDefaultFunction or similar, but it was not. Field: %s{fieldName.idText}"
                    | [ x ] -> x
                    | _ ->
                        failwith
                            $"Expected Choice to be annotated with exactly one ArgumentDefaultFunction or similar, but it was annotated with multiple. Field: %s{fieldName.idText}"

                parseElt, Accumulation.Choice relevantAttr, childTy
            | elts ->
                let elts = elts |> List.map string<SynType> |> String.concat ", "

                failwith
                    $"ArgParser requires Choice to be of the form Choice<'a, 'a>; that is, two arguments, both the same. For field %s{fieldName.idText}, got: %s{elts}"
        | ListType eltTy ->
            let parseElt, acc, childTy = createParseFunction fieldName attrs eltTy

            match acc with
            | Accumulation.List ->
                failwith $"ArgParser does not support nested lists at field %s{fieldName.idText}: %O{ty}"
            | Accumulation.Choice _ ->
                failwith $"ArgParser does not support lists containing choices at field %s{fieldName.idText}: %O{ty}"
            | Accumulation.Optional ->
                failwith $"ArgParser does not support lists of options at field %s{fieldName.idText}: %O{ty}"
            | Accumulation.Required -> parseElt, Accumulation.List, childTy
        | _ -> failwith $"Could not decide how to parse arguments for field %s{fieldName.idText} of type %O{ty}"

    let private toParseSpec (finalRecord : RecordType) : ParserSpec =
        finalRecord.Fields
        |> List.iter (fun (SynField.SynField (isStatic = isStatic)) ->
            if isStatic then
                failwith "No static record fields allowed in ArgParserGenerator"
        )

        let args : ArgToParse list =
            finalRecord.Fields
            |> List.map (fun (SynField.SynField (attrs, _, identOption, fieldType, _, _, _, _, _)) ->
                let attrs = attrs |> List.collect (fun a -> a.Attributes)

                let positionalArgAttr =
                    attrs
                    |> List.tryFind (fun a ->
                        match (List.last a.TypeName.LongIdent).idText with
                        | "PositionalArgsAttribute"
                        | "PositionalArgs" -> true
                        | _ -> false
                    )

                let parseExactModifier =
                    attrs
                    |> List.tryPick (fun a ->
                        match (List.last a.TypeName.LongIdent).idText with
                        | "ParseExactAttribute"
                        | "ParseExact" -> Some a.ArgExpr
                        | _ -> None
                    )

                let helpText =
                    attrs
                    |> List.tryPick (fun a ->
                        match (List.last a.TypeName.LongIdent).idText with
                        | "ArgumentHelpTextAttribute"
                        | "ArgumentHelpText" -> Some a.ArgExpr
                        | _ -> None
                    )

                let helpText =
                    match parseExactModifier, helpText with
                    | None, ht -> ht
                    | Some pe, None ->
                        SynExpr.createIdent "sprintf"
                        |> SynExpr.applyTo (SynExpr.CreateConst "[Parse format (.NET): %s]")
                        |> SynExpr.applyTo pe
                        |> Some
                    | Some pe, Some ht ->
                        SynExpr.createIdent "sprintf"
                        |> SynExpr.applyTo (SynExpr.CreateConst "%s [Parse format (.NET): %s]")
                        |> SynExpr.applyTo ht
                        |> SynExpr.applyTo pe
                        |> Some

                let ident =
                    match identOption with
                    | None -> failwith "expected args field to have a name, but it did not"
                    | Some i -> i

                let parser, accumulation, parseTy = createParseFunction ident attrs fieldType

                match positionalArgAttr with
                | Some _ ->
                    match accumulation with
                    | Accumulation.List ->
                        {
                            FieldName = ident
                            Parser = parser
                            TargetVariable = ident
                            Accumulation = accumulation
                            TargetType = parseTy
                            ArgForm = argify ident
                            Help = helpText
                        }
                        |> ArgToParse.Positional
                    | _ -> failwith $"Expected positional arg accumulation type to be List, but it was %O{fieldType}"
                | None ->
                    {
                        FieldName = ident
                        Parser = parser
                        TargetVariable = ident
                        Accumulation = accumulation
                        TargetType = parseTy
                        ArgForm = argify ident
                        Help = helpText
                    }
                    |> ArgToParse.NonPositional
            )

        let positional, nonPositionals =
            let mutable p = None
            let n = ResizeArray ()

            for arg in args do
                match arg with
                | ArgToParse.Positional arg ->
                    match p with
                    | None -> p <- Some arg
                    | Some existing ->
                        failwith
                            $"Multiple args were tagged with `Positional`: %s{existing.TargetVariable.idText}, %s{arg.TargetVariable.idText}"
                | ArgToParse.NonPositional arg -> n.Add arg

            p, List.ofSeq n

        {
            NonPositionals = nonPositionals
            Positionals = positional
        }

    /// let helpText : string = ...
    let private helpText
        (typeName : Ident)
        (positional : ParseFunction option)
        (args : ParseFunction list)
        : SynBinding
        =
        let toPrintable (prefix : string) (arg : ParseFunction) : SynExpr =
            let ty = arg.TargetType |> SynType.toHumanReadableString

            let helpText =
                match arg.Help with
                | None -> SynExpr.CreateConst ""
                | Some helpText ->
                    SynExpr.applyFunction (SynExpr.createIdent "sprintf") (SynExpr.CreateConst " : %s")
                    |> SynExpr.applyTo (SynExpr.paren helpText)
                    |> SynExpr.paren

            let descriptor =
                match arg.Accumulation with
                | Accumulation.Required -> SynExpr.CreateConst ""
                | Accumulation.Optional -> SynExpr.CreateConst " (optional)"
                | Accumulation.Choice (ArgumentDefaultSpec.EnvironmentVariable var) ->
                    // We don't print out the default value in case it's a secret. People often pass secrets
                    // through env vars!
                    var
                    |> SynExpr.pipeThroughFunction (
                        SynExpr.applyFunction
                            (SynExpr.createIdent "sprintf")
                            (SynExpr.CreateConst " (default value populated from env var %s)")
                    )
                    |> SynExpr.paren
                | Accumulation.Choice (ArgumentDefaultSpec.FunctionCall var) ->
                    SynExpr.callMethod var.idText (SynExpr.createIdent' typeName)
                    |> SynExpr.pipeThroughFunction (
                        SynExpr.applyFunction
                            (SynExpr.createIdent "sprintf")
                            (SynExpr.CreateConst " (default value: %O)")
                    )
                    |> SynExpr.paren
                | Accumulation.List -> SynExpr.CreateConst " (can be repeated)"

            let prefix = $"%s{arg.ArgForm}  %s{ty}%s{prefix}"

            SynExpr.applyFunction (SynExpr.createIdent "sprintf") (SynExpr.CreateConst (prefix + "%s%s"))
            |> SynExpr.applyTo descriptor
            |> SynExpr.applyTo helpText
            |> SynExpr.paren

        args
        |> List.map (toPrintable "")
        |> fun l ->
            match positional with
            | None -> l
            | Some pos -> l @ [ toPrintable " (positional args)" pos ]
        |> SynExpr.listLiteral
        |> SynExpr.pipeThroughFunction (
            SynExpr.applyFunction (SynExpr.createLongIdent [ "String" ; "concat" ]) (SynExpr.CreateConst @"\n")
        )
        |> SynBinding.basic [ Ident.create "helpText" ] [ SynPat.unit ]

    /// `let processKeyValue (key : string) (value : string) : Result<unit, string option> = ...`
    /// Returns a possible error.
    /// A parse failure might not be fatal (e.g. maybe the input was optionally of arity 0, and we failed to do
    /// the parse because in fact the key decided not to take this argument); in that case we return Error None.
    let private processKeyValue (argParseErrors : Ident) (args : ParseFunction list) : SynBinding =
        (SynExpr.applyFunction (SynExpr.createIdent "Error") (SynExpr.createIdent "None"), args)
        ||> List.fold (fun finalBranch arg ->
            match arg.Accumulation with
            | Accumulation.Required
            | Accumulation.Choice _
            | Accumulation.Optional ->
                let multipleErrorMessage =
                    SynExpr.createIdent "sprintf"
                    |> SynExpr.applyTo (SynExpr.CreateConst "Argument '%s' was supplied multiple times: %O and %O")
                    |> SynExpr.applyTo (SynExpr.CreateConst arg.ArgForm)
                    |> SynExpr.applyTo (SynExpr.createIdent "x")
                    |> SynExpr.applyTo (SynExpr.createIdent "value")

                let performAssignment =
                    [
                        SynExpr.createIdent "value"
                        |> SynExpr.pipeThroughFunction arg.Parser
                        |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Some")
                        |> SynExpr.assign (SynLongIdent.createI arg.TargetVariable)

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
                                    SynExpr.dotGet "Add" (SynExpr.createIdent' argParseErrors)
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
                |> SynExpr.createMatch (SynExpr.createIdent' arg.TargetVariable)
            | Accumulation.List ->
                [
                    SynExpr.createIdent "value"
                    |> SynExpr.pipeThroughFunction (
                        SynExpr.createLongIdent' [ arg.TargetVariable ; Ident.create "Add" ]
                    )
                    |> SynExpr.applyFunction arg.Parser
                    SynExpr.CreateConst () |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Ok")
                ]
                |> SynExpr.sequential
            |> SynExpr.ifThenElse
                (SynExpr.applyFunction
                    (SynExpr.createLongIdent [ "System" ; "String" ; "Equals" ])
                    (SynExpr.tuple
                        [
                            SynExpr.createIdent "key"
                            SynExpr.CreateConst arg.ArgForm
                            SynExpr.createLongIdent [ "System" ; "StringComparison" ; "OrdinalIgnoreCase" ]
                        ]))
                finalBranch
        )
        |> SynBinding.basic
            [ Ident.create "processKeyValue" ]
            [
                SynPat.annotateType SynType.string (SynPat.named "key")
                SynPat.annotateType SynType.string (SynPat.named "value")
            ]
        |> SynBinding.withReturnAnnotation (
            SynType.app "Result" [ SynType.unit ; SynType.appPostfix "option" SynType.string ]
        )
        |> SynBinding.withXmlDoc (
            [
                " Processes the key-value pair, returning Error if no key was matched."
                " If the key is an arg which can arity 1, but throws when consuming that arg, we return Error(<the message>)."
                " This can nevertheless be a successful parse, e.g. when the key may have arity 0."
            ]
            |> PreXmlDoc.create'
        )

    /// `let setFlagValue (key : string) : bool = ...`
    let private setFlagValue (parseState : Ident) (argParseErrors : Ident) (flags : ParseFunction list) : SynBinding =
        (SynExpr.CreateConst false, flags)
        ||> List.fold (fun finalExpr flag ->
            let multipleErrorMessage =
                SynExpr.createIdent "sprintf"
                |> SynExpr.applyTo (SynExpr.CreateConst "Flag '%s' was supplied multiple times")
                |> SynExpr.applyTo (SynExpr.CreateConst flag.ArgForm)

            [
                SynMatchClause.create
                    (SynPat.nameWithArgs "Some" [ SynPat.named "x" ])
                    // This is an error, but it's one we can gracefully report at the end.
                    (SynExpr.sequential
                        [
                            multipleErrorMessage
                            |> SynExpr.pipeThroughFunction (SynExpr.dotGet "Add" (SynExpr.createIdent' argParseErrors))
                            SynExpr.CreateConst true
                        ])

                SynMatchClause.create
                    (SynPat.named "None")
                    ([
                        SynExpr.assign
                            (SynLongIdent.createI flag.TargetVariable)
                            (SynExpr.applyFunction (SynExpr.createIdent "Some") (SynExpr.CreateConst true))
                        SynExpr.CreateConst true
                     ]
                     |> SynExpr.sequential)
            ]
            |> SynExpr.createMatch (SynExpr.createIdent' flag.TargetVariable)
            |> SynExpr.ifThenElse
                (SynExpr.applyFunction
                    (SynExpr.createLongIdent [ "System" ; "String" ; "Equals" ])
                    (SynExpr.tuple
                        [
                            SynExpr.createIdent "key"
                            SynExpr.CreateConst flag.ArgForm
                            SynExpr.createLongIdent [ "System" ; "StringComparison" ; "OrdinalIgnoreCase" ]
                        ]))
                finalExpr
        )
        |> SynBinding.basic [ Ident.create "setFlagValue" ] [ SynPat.annotateType SynType.string (SynPat.named "key") ]
        |> SynBinding.withReturnAnnotation (SynType.named "bool")
        |> SynBinding.withXmlDoc (PreXmlDoc.create "Returns false if we didn't set a value.")

    /// `let rec go (state : %ParseState%) (args : string list) : unit = ...`
    let private mainLoop
        (parseState : Ident)
        (errorAcc : Ident)
        (leftoverArgs : Ident)
        (leftoverArgParser : SynExpr)
        : SynBinding
        =
        /// `go (AwaitingValue arg) args
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
                (SynExpr.sequential
                    [
                        (SynExpr.createIdent "arg"
                         |> SynExpr.pipeThroughFunction leftoverArgParser
                         |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent' [ leftoverArgs ; Ident.create "Add" ]))

                        recurseKey
                    ])
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
                                    (SynExpr.createIdent "processKeyValue"
                                     |> SynExpr.applyTo (SynExpr.createIdent "key")
                                     |> SynExpr.applyTo (SynExpr.createIdent "value"))
                                    [
                                        SynMatchClause.create (SynPat.nameWithArgs "Ok" [ SynPat.unit ]) recurseKey

                                        SynMatchClause.create (SynPat.nameWithArgs "Error" [ SynPat.named "None" ]) fail
                                        SynMatchClause.create
                                            (SynPat.nameWithArgs
                                                "Error"
                                                [ SynPat.nameWithArgs "Some" [ SynPat.named "msg" ] |> SynPat.paren ])
                                            (SynExpr.sequential
                                                [
                                                    SynExpr.createIdent "sprintf"
                                                    |> SynExpr.applyTo (SynExpr.CreateConst "%s (at arg %s)")
                                                    |> SynExpr.applyTo (SynExpr.createIdent "msg")
                                                    |> SynExpr.applyTo (SynExpr.createIdent "arg")
                                                    |> SynExpr.pipeThroughFunction (
                                                        SynExpr.dotGet "Add" (SynExpr.createIdent' errorAcc)
                                                    )

                                                    recurseKey
                                                ])
                                    ]))
                            (SynExpr.createIdent "args" |> SynExpr.pipeThroughFunction recurseValue)))
                    (SynExpr.createIdent "helpText"
                     |> SynExpr.applyTo (SynExpr.CreateConst ())
                     |> SynExpr.pipeThroughFunction (
                         SynExpr.applyFunction
                             (SynExpr.createIdent "failwithf")
                             (SynExpr.CreateConst @"Help text requested.\n%s")
                     )))

        let processValue =
            // During failure, we've received an optional exception message that happened when we tried to parse
            // the value; it's in the variable `exc`.
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
                        (SynExpr.applyFunction (SynExpr.createIdent "setFlagValue") (SynExpr.createIdent "key"))
                        fail
                        (SynExpr.createIdent "go"
                         |> SynExpr.applyTo (SynExpr.createLongIdent' [ parseState ; Ident.create "AwaitingKey" ])
                         |> SynExpr.applyTo (SynExpr.listCons (SynExpr.createIdent "arg") (SynExpr.createIdent "args"))))
            ]
            |> SynExpr.createMatch (
                SynExpr.applyFunction
                    (SynExpr.applyFunction (SynExpr.createIdent "processKeyValue") (SynExpr.createIdent "key"))
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
                                        (SynExpr.createIdent "setFlagValue")
                                        (SynExpr.createIdent "key"))
                                    (trailingArgMessage
                                     |> SynExpr.pipeThroughFunction (
                                         SynExpr.dotGet "Add" (SynExpr.createIdent' errorAcc)
                                     ))
                                    (SynExpr.CreateConst ()))
                        ])
                SynMatchClause.create
                    (SynPat.listCons (SynPat.createConst (SynConst.CreateString "--")) (SynPat.named "rest"))
                    (SynExpr.callMethodArg
                        "AddRange"
                        (SynExpr.paren (
                            SynExpr.createIdent "rest"
                            |> SynExpr.pipeThroughFunction (
                                SynExpr.applyFunction (SynExpr.createLongIdent [ "Seq" ; "map" ]) leftoverArgParser
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
                |> SynPat.annotateType (SynType.appPostfix "list" (SynType.string))
            ]

        SynBinding.basic [ Ident.create "go" ] args body
        |> SynBinding.withRecursion true

    /// Takes a single argument, `args : string list`, and returns something of the type indicated by `recordType`.
    let createRecordParse (parseState : Ident) (recordType : RecordType) : SynExpr =
        let spec = toParseSpec recordType
        // For each argument (positional and non-positional), create an accumulator for it.
        let bindings =
            spec.NonPositionals
            |> List.map (fun pf ->
                match pf.Accumulation with
                | Accumulation.Required
                | Accumulation.Choice _
                | Accumulation.Optional ->
                    SynExpr.createIdent "None"
                    |> SynBinding.basic [ pf.TargetVariable ] []
                    |> SynBinding.withMutability true
                    |> SynBinding.withReturnAnnotation (SynType.appPostfix "option" pf.TargetType)
                | Accumulation.List ->
                    SynExpr.createIdent "ResizeArray"
                    |> SynExpr.applyTo (SynExpr.CreateConst ())
                    |> SynBinding.basic [ pf.TargetVariable ] []
                    |> SynBinding.withReturnAnnotation (SynType.appPostfix "ResizeArray" pf.TargetType)
            )

        let bindings, leftoverArgsName, leftoverArgsParser =
            let bindingName, leftoverArgsParser, leftoverArgsType =
                match spec.Positionals with
                | None ->
                    Ident.create "parser_LeftoverArgs",
                    (SynExpr.createLambda "x" (SynExpr.createIdent "x")),
                    SynType.string
                | Some pf -> pf.TargetVariable, pf.Parser, pf.TargetType

            let bindings =
                SynExpr.createIdent "ResizeArray"
                |> SynExpr.applyTo (SynExpr.CreateConst ())
                |> SynBinding.basic [ bindingName ] []
                |> SynBinding.withReturnAnnotation (SynType.appPostfix "ResizeArray" leftoverArgsType)
                |> fun b -> b :: bindings

            bindings, bindingName, leftoverArgsParser

        let argParseErrors = Ident.create "ArgParser_errors"

        let errorCollection : SynBinding =
            SynExpr.createIdent "ResizeArray"
            |> SynExpr.applyTo (SynExpr.CreateConst ())
            |> SynBinding.basic [ argParseErrors ] []

        let helpText = helpText recordType.Name spec.Positionals spec.NonPositionals

        let bindings = errorCollection :: helpText :: bindings

        let unchecked =
            SynExpr.createLongIdent [ "Unchecked" ; "defaultof" ]
            |> SynExpr.typeApp [ SynType.anon ]

        // Determine whether any required arg is missing, and freeze args into immutable form.
        let freezeNonPositionalArgs =
            spec.NonPositionals
            |> List.map (fun pf ->
                match pf.Accumulation with
                | Accumulation.Choice spec ->
                    let getDefaultValue =
                        match spec with
                        | ArgumentDefaultSpec.EnvironmentVariable name ->
                            let result =
                                name
                                |> SynExpr.pipeThroughFunction (SynExpr.createIdent "getEnvironmentVariable")

                            let errorMessage =
                                SynExpr.createIdent "sprintf"
                                |> SynExpr.applyTo (
                                    SynExpr.CreateConst
                                        "No value was supplied for %s, nor was environment variable %s set"
                                )
                                |> SynExpr.applyTo (SynExpr.CreateConst pf.ArgForm)
                                |> SynExpr.applyTo name

                            [
                                SynMatchClause.create
                                    SynPat.createNull
                                    (SynExpr.sequential
                                        [
                                            errorMessage
                                            |> SynExpr.pipeThroughFunction (
                                                SynExpr.dotGet "Add" (SynExpr.createIdent' argParseErrors)
                                            )
                                            unchecked
                                        ])

                                SynMatchClause.create
                                    (SynPat.named "x")
                                    (SynExpr.createIdent "x" |> SynExpr.pipeThroughFunction pf.Parser)
                            ]
                            |> SynExpr.createMatch result
                        | ArgumentDefaultSpec.FunctionCall name ->
                            SynExpr.callMethod name.idText (SynExpr.createIdent' recordType.Name)

                    [
                        SynMatchClause.create
                            (SynPat.named "None")
                            (getDefaultValue
                             |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Choice2Of2"))
                        SynMatchClause.create
                            (SynPat.nameWithArgs "Some" [ SynPat.named "x" ])
                            (SynExpr.applyFunction (SynExpr.createIdent "Choice1Of2") (SynExpr.createIdent "x"))
                    ]
                    |> SynExpr.createMatch (SynExpr.createIdent' pf.TargetVariable)
                    |> SynBinding.basic [ pf.TargetVariable ] []
                | Accumulation.Optional ->
                    SynBinding.basic [ pf.TargetVariable ] [] (SynExpr.createIdent' pf.TargetVariable)
                | Accumulation.List ->
                    SynBinding.basic
                        [ pf.TargetVariable ]
                        []
                        (SynExpr.createIdent' pf.TargetVariable
                         |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "Seq" ; "toList" ]))
                | Accumulation.Required ->
                    let errorMessage =
                        SynExpr.createIdent "sprintf"
                        |> SynExpr.applyTo (SynExpr.CreateConst "Required argument '%s' received no value")
                        |> SynExpr.applyTo (SynExpr.CreateConst (argify pf.TargetVariable))

                    [
                        SynMatchClause.create
                            (SynPat.named "None")
                            (SynExpr.sequential
                                [
                                    errorMessage
                                    |> SynExpr.pipeThroughFunction (
                                        SynExpr.dotGet "Add" (SynExpr.createIdent' argParseErrors)
                                    )
                                    unchecked
                                ])

                        SynMatchClause.create
                            (SynPat.nameWithArgs "Some" [ SynPat.named "x" ])
                            (SynExpr.createIdent "x")
                    ]
                    |> SynExpr.createMatch (SynExpr.createIdent' pf.TargetVariable)
                    |> SynBinding.basic [ pf.TargetVariable ] []
            )

        let freezePositional =
            match spec.Positionals with
            | None ->
                // Check if there are leftover args. If there are, throw.
                let errorMessage =
                    SynExpr.createIdent' leftoverArgsName
                    |> SynExpr.pipeThroughFunction (
                        SynExpr.applyFunction
                            (SynExpr.createLongIdent [ "String" ; "concat" ])
                            (SynExpr.CreateConst " ")
                    )
                    |> SynExpr.pipeThroughFunction (
                        SynExpr.applyFunction
                            (SynExpr.createIdent "sprintf")
                            (SynExpr.CreateConst "There were leftover args: %s")
                    )

                SynExpr.ifThenElse
                    (SynExpr.dotGet "Count" (SynExpr.createIdent' leftoverArgsName)
                     |> SynExpr.equals (SynExpr.CreateConst 0))
                    (SynExpr.sequential
                        [
                            errorMessage
                            |> SynExpr.pipeThroughFunction (SynExpr.dotGet "Add" (SynExpr.createIdent' argParseErrors))
                            unchecked
                        ])
                    (SynExpr.CreateConst ())
            | Some _ ->
                SynExpr.createIdent' leftoverArgsName
                |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "Seq" ; "toList" ])
            |> SynBinding.basic [ leftoverArgsName ] []
            |> List.singleton

        let freezeArgs = freezePositional @ freezeNonPositionalArgs

        let retPositional =
            match spec.Positionals with
            | None -> []
            | Some pf ->
                [
                    SynLongIdent.createI pf.TargetVariable, SynExpr.createIdent' pf.TargetVariable
                ]

        let retValue =
            let happyPath =
                spec.NonPositionals
                |> List.map (fun pf -> SynLongIdent.createI pf.TargetVariable, SynExpr.createIdent' pf.TargetVariable)
                |> fun np -> retPositional @ np
                |> AstHelper.instantiateRecord

            let sadPath =
                SynExpr.createIdent' argParseErrors
                |> SynExpr.pipeThroughFunction (
                    SynExpr.applyFunction (SynExpr.createLongIdent [ "String" ; "concat" ]) (SynExpr.CreateConst @"\n")
                )
                |> SynExpr.pipeThroughFunction (
                    SynExpr.createIdent "failwithf"
                    |> SynExpr.applyTo (SynExpr.CreateConst @"Errors during parse!\n%s")
                )

            let areErrors =
                SynExpr.dotGet "Count" (SynExpr.createIdent' argParseErrors)
                |> SynExpr.equals (SynExpr.CreateConst 0)

            SynExpr.ifThenElse areErrors sadPath happyPath

        let flags =
            spec.NonPositionals
            |> List.filter (fun pf ->
                match pf.TargetType with
                | PrimitiveType pt -> (pt |> List.map _.idText) = [ "System" ; "Boolean" ]
                | _ -> false
            )

        [
            SynExpr.createIdent "go"
            |> SynExpr.applyTo (SynExpr.createLongIdent' [ parseState ; Ident.create "AwaitingKey" ])
            |> SynExpr.applyTo (SynExpr.createIdent "args")

            SynExpr.createLet freezeArgs retValue
        ]
        |> SynExpr.sequential
        |> SynExpr.createLet (
            bindings
            @ [
                processKeyValue argParseErrors (Option.toList spec.Positionals @ spec.NonPositionals)
                setFlagValue parseState argParseErrors flags
                mainLoop parseState argParseErrors leftoverArgsName leftoverArgsParser
            ]
        )

    let createModule
        (opens : SynOpenDeclTarget list)
        (ns : LongIdent)
        ((taggedType : SynTypeDefn, spec : ArgParserOutputSpec))
        (_allUnionTypesTODO : SynTypeDefn list)
        (allRecordTypes : SynTypeDefn list)
        : SynModuleOrNamespace
        =
        let taggedType = RecordType.OfRecord taggedType

        let modAttrs, modName =
            if spec.ExtensionMethods then
                [ SynAttribute.autoOpen ], Ident.create (taggedType.Name.idText + "ArgParse")
            else
                [ SynAttribute.requireQualifiedAccess ; SynAttribute.compilationRepresentation ], taggedType.Name

        let modInfo =
            SynComponentInfo.create modName
            |> SynComponentInfo.withDocString (
                PreXmlDoc.Create $" Methods to parse arguments for the type %s{taggedType.Name.idText}"
            )
            |> SynComponentInfo.addAttributes modAttrs

        let parseStateIdent = Ident.create $"ParseState_%s{taggedType.Name.idText}"

        let parseStateType =
            [
                SynUnionCase.create
                    {
                        Attrs = []
                        Fields = []
                        Ident = Ident.create "AwaitingKey"
                    }
                SynUnionCase.create
                    {
                        Attrs = []
                        Fields =
                            [
                                {
                                    Attrs = []
                                    Ident = Ident.create "key"
                                    Type = SynType.string
                                }
                            ]
                        Ident = Ident.create "AwaitingValue"
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
                createRecordParse parseStateIdent taggedType
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
                yield SynModuleDecl.CreateOpen openStatement
            yield taggedMod
        ]
        |> SynModuleOrNamespace.createNamespace ns

    let generate (context : GeneratorContext) : Output =
        let ast, _ =
            Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

        let types = Ast.extractTypeDefn ast

        let opens = AstHelper.extractOpens ast

        let namespaceAndTypes =
            types
            |> List.choose (fun (ns, types) ->
                let typeWithAttr =
                    types
                    |> List.tryPick (fun ty ->
                        match Ast.getAttribute<ArgParserAttribute> ty with
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

                match typeWithAttr with
                | Some taggedType ->
                    let unions, records, others =
                        (([], [], []), types)
                        ||> List.fold (fun
                                           (unions, records, others)
                                           (SynTypeDefn.SynTypeDefn (_, repr, _, _, _, _) as ty) ->
                            match repr with
                            | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union _, _) ->
                                ty :: unions, records, others
                            | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record _, _) ->
                                unions, ty :: records, others
                            | _ -> unions, records, ty :: others
                        )

                    if not others.IsEmpty then
                        failwith
                            $"Error: all types recursively defined together with an ArgParserGenerator type must be discriminated unions or records. %+A{others}"

                    Some (ns, taggedType, unions, records)
                | _ -> None
            )

        let modules =
            namespaceAndTypes
            |> List.map (fun (ns, taggedType, unions, records) -> createModule opens ns taggedType unions records)

        Output.Ast modules

/// Myriad generator that provides a catamorphism for an algebraic data type.
[<MyriadGenerator("arg-parser")>]
type ArgParserGenerator () =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = [ ".fs" ]

        member _.Generate (context : GeneratorContext) = ArgParserGenerator.generate context