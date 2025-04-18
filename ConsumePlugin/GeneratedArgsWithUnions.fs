//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------






namespace ArgParserHelpers

/// Helper types for arg parsing
module internal ArgParseHelpers_ConsumePlugin_ArgsWithUnions =
    open System
    open System.IO
    open WoofWare.Myriad.Plugins
    open ConsumePlugin.ArgsWithUnions

    /// A partially-parsed BasicNoPositionals.
    type internal BasicNoPositionals_InProgress =
        {
            mutable Bar : string option
            mutable Baz : bool option
            mutable Foo : int option
            mutable Rest : ResizeArray<int>
        }

        /// Freeze this in-progress type. On success, returns the frozen type and the arg (if any) which consumed the input positional args.
        member this.Assemble_
            (getEnvironmentVariable : string -> string)
            (positionals : Choice<string * int, string * int> list)
            : Result<BasicNoPositionals * string option, string list>
            =
            let errors = ResizeArray<string> ()
            let positionalConsumers = ResizeArray<string> ()
            let outOfPlacePositionals : ResizeArray<string> = ResizeArray ()

            let arg0 : int =
                match this.Foo with
                | Some result -> result
                | None ->
                    errors.Add (sprintf "Required argument '--%s' received no value" "foo")
                    Unchecked.defaultof<_>

            let arg1 : string =
                match this.Bar with
                | Some result -> result
                | None ->
                    errors.Add (sprintf "Required argument '--%s' received no value" "bar")
                    Unchecked.defaultof<_>

            let arg2 : bool =
                match this.Baz with
                | Some result -> result
                | None ->
                    errors.Add (sprintf "Required argument '--%s' received no value" "baz")
                    Unchecked.defaultof<_>

            let arg3 : int list = this.Rest |> Seq.toList

            if positionalConsumers.Count <= 1 then
                if outOfPlacePositionals.Count > 0 then
                    outOfPlacePositionals
                    |> String.concat " "
                    |> (fun x ->
                        if 0 = outOfPlacePositionals.Count then
                            "Unmatched args which look like they are meant to be flags. " + x
                        else
                            sprintf
                                "Unmatched args which look like they are meant to be flags. If you intended them as positional args, explicitly pass them with the `%s=` syntax, or place them after a trailing `--`. %s"
                                positionalConsumers.[0]
                                x
                    )
                    |> errors.Add
                else
                    ()

                if errors.Count = 0 then
                    Ok (
                        {
                            Foo = arg0
                            Bar = arg1
                            Baz = arg2
                            Rest = arg3
                        },
                        Seq.tryExactlyOne positionalConsumers
                    )
                else
                    errors |> Seq.toList |> Error
            else
                ("Multiple parsers consumed positional args; this is an error in the application, not an error by the user: "
                 + String.concat ", " positionalConsumers)
                |> List.singleton
                |> Error

        static member _Empty () : BasicNoPositionals_InProgress =
            {
                Bar = None
                Baz = None
                Foo = None
                Rest = ResizeArray ()
            }

        /// Processes the key-value pair, returning Error if no key was matched.
        /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
        /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
        member this.ProcessKeyValueSelf_
            (argNum_ : int)
            (errors_ : ResizeArray<string>)
            (key : string)
            (value : string)
            : Result<unit, string option>
            =
            if System.String.Equals (key, sprintf "--%s" "rest", System.StringComparison.OrdinalIgnoreCase) then
                value |> (fun x -> System.Int32.Parse x) |> (fun x -> x) |> this.Rest.Add
                () |> Ok
            else if System.String.Equals (key, sprintf "--%s" "foo", System.StringComparison.OrdinalIgnoreCase) then
                match this.Foo with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "foo")
                        (x.ToString ())
                        (value.ToString ())
                    |> errors_.Add

                    Ok ()
                | None ->
                    try
                        this.Foo <- value |> (fun x -> System.Int32.Parse x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if System.String.Equals (key, sprintf "--%s" "baz", System.StringComparison.OrdinalIgnoreCase) then
                match this.Baz with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "baz")
                        (x.ToString ())
                        (value.ToString ())
                    |> errors_.Add

                    Ok ()
                | None ->
                    try
                        this.Baz <- value |> (fun x -> System.Boolean.Parse x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if System.String.Equals (key, sprintf "--%s" "bar", System.StringComparison.OrdinalIgnoreCase) then
                match this.Bar with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "bar")
                        (x.ToString ())
                        (value.ToString ())
                    |> errors_.Add

                    Ok ()
                | None ->
                    try
                        this.Bar <- value |> (fun x -> x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else
                Error None

        member this.ProcessKeyValue
            (argNum_ : int)
            (errors_ : ResizeArray<string>)
            (key : string)
            (value : string)
            : Result<unit, string option>
            =
            match this.ProcessKeyValueSelf_ argNum_ errors_ key value with
            | Ok () -> Ok ()
            | Error None -> Error None
            | Error (Some errorFromLeaf) -> Error (Some errorFromLeaf)

        /// Returns false if we didn't set a value.
        member this.SetFlagValue_ (errors_ : ResizeArray<string>) (key : string) : bool =
            if System.String.Equals (key, sprintf "--%s" "baz", System.StringComparison.OrdinalIgnoreCase) then
                match this.Baz with
                | Some _ ->
                    sprintf "Flag '%s' was supplied multiple times" (sprintf "--%s" "baz")
                    |> errors_.Add

                    true
                | None ->
                    this.Baz <- true |> Some
                    true
            else
                false

        /// Compute help text for this parser, optionally noting the given prefix on each argument and indenting each line by this many spaces.
        static member HelpText_ (prefix : string option) (indent : int) : string = failwith "TODO"

    /// A partially-parsed UsernamePasswordAuth.
    type internal UsernamePasswordAuth_InProgress =
        {
            mutable Password : string option
            mutable Username : string option
        }

        /// Freeze this in-progress type. On success, returns the frozen type and the arg (if any) which consumed the input positional args.
        member this.Assemble_
            (getEnvironmentVariable : string -> string)
            (positionals : Choice<string * int, string * int> list)
            : Result<UsernamePasswordAuth * string option, string list>
            =
            let errors = ResizeArray<string> ()
            let positionalConsumers = ResizeArray<string> ()
            let outOfPlacePositionals : ResizeArray<string> = ResizeArray ()

            let arg0 : string =
                match this.Username with
                | Some result -> result
                | None ->
                    errors.Add (sprintf "Required argument '--%s' received no value" "username")
                    Unchecked.defaultof<_>

            let arg1 : string =
                match this.Password with
                | Some result -> result
                | None ->
                    errors.Add (sprintf "Required argument '--%s' received no value" "password")
                    Unchecked.defaultof<_>

            if positionalConsumers.Count <= 1 then
                if outOfPlacePositionals.Count > 0 then
                    outOfPlacePositionals
                    |> String.concat " "
                    |> (fun x ->
                        if 0 = outOfPlacePositionals.Count then
                            "Unmatched args which look like they are meant to be flags. " + x
                        else
                            sprintf
                                "Unmatched args which look like they are meant to be flags. If you intended them as positional args, explicitly pass them with the `%s=` syntax, or place them after a trailing `--`. %s"
                                positionalConsumers.[0]
                                x
                    )
                    |> errors.Add
                else
                    ()

                if errors.Count = 0 then
                    Ok (
                        {
                            Username = arg0
                            Password = arg1
                        },
                        Seq.tryExactlyOne positionalConsumers
                    )
                else
                    errors |> Seq.toList |> Error
            else
                ("Multiple parsers consumed positional args; this is an error in the application, not an error by the user: "
                 + String.concat ", " positionalConsumers)
                |> List.singleton
                |> Error

        static member _Empty () : UsernamePasswordAuth_InProgress =
            {
                Password = None
                Username = None
            }

        /// Processes the key-value pair, returning Error if no key was matched.
        /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
        /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
        member this.ProcessKeyValueSelf_
            (argNum_ : int)
            (errors_ : ResizeArray<string>)
            (key : string)
            (value : string)
            : Result<unit, string option>
            =
            if System.String.Equals (key, sprintf "--%s" "username", System.StringComparison.OrdinalIgnoreCase) then
                match this.Username with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "username")
                        (x.ToString ())
                        (value.ToString ())
                    |> errors_.Add

                    Ok ()
                | None ->
                    try
                        this.Username <- value |> (fun x -> x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if
                System.String.Equals (key, sprintf "--%s" "password", System.StringComparison.OrdinalIgnoreCase)
            then
                match this.Password with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "password")
                        (x.ToString ())
                        (value.ToString ())
                    |> errors_.Add

                    Ok ()
                | None ->
                    try
                        this.Password <- value |> (fun x -> x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else
                Error None

        member this.ProcessKeyValue
            (argNum_ : int)
            (errors_ : ResizeArray<string>)
            (key : string)
            (value : string)
            : Result<unit, string option>
            =
            match this.ProcessKeyValueSelf_ argNum_ errors_ key value with
            | Ok () -> Ok ()
            | Error None -> Error None
            | Error (Some errorFromLeaf) -> Error (Some errorFromLeaf)

        /// Returns false if we didn't set a value.
        member this.SetFlagValue_ (errors_ : ResizeArray<string>) (key : string) : bool = false
        /// Compute help text for this parser, optionally noting the given prefix on each argument and indenting each line by this many spaces.
        static member HelpText_ (prefix : string option) (indent : int) : string = failwith "TODO"

    /// A partially-parsed TokenAuth.
    type internal TokenAuth_InProgress =
        {
            mutable Token : string option
        }

        /// Freeze this in-progress type. On success, returns the frozen type and the arg (if any) which consumed the input positional args.
        member this.Assemble_
            (getEnvironmentVariable : string -> string)
            (positionals : Choice<string * int, string * int> list)
            : Result<TokenAuth * string option, string list>
            =
            let errors = ResizeArray<string> ()
            let positionalConsumers = ResizeArray<string> ()
            let outOfPlacePositionals : ResizeArray<string> = ResizeArray ()

            let arg0 : string =
                match this.Token with
                | Some result -> result
                | None ->
                    errors.Add (sprintf "Required argument '--%s' received no value" "token")
                    Unchecked.defaultof<_>

            if positionalConsumers.Count <= 1 then
                if outOfPlacePositionals.Count > 0 then
                    outOfPlacePositionals
                    |> String.concat " "
                    |> (fun x ->
                        if 0 = outOfPlacePositionals.Count then
                            "Unmatched args which look like they are meant to be flags. " + x
                        else
                            sprintf
                                "Unmatched args which look like they are meant to be flags. If you intended them as positional args, explicitly pass them with the `%s=` syntax, or place them after a trailing `--`. %s"
                                positionalConsumers.[0]
                                x
                    )
                    |> errors.Add
                else
                    ()

                if errors.Count = 0 then
                    Ok (
                        {
                            Token = arg0
                        },
                        Seq.tryExactlyOne positionalConsumers
                    )
                else
                    errors |> Seq.toList |> Error
            else
                ("Multiple parsers consumed positional args; this is an error in the application, not an error by the user: "
                 + String.concat ", " positionalConsumers)
                |> List.singleton
                |> Error

        static member _Empty () : TokenAuth_InProgress =
            {
                Token = None
            }

        /// Processes the key-value pair, returning Error if no key was matched.
        /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
        /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
        member this.ProcessKeyValueSelf_
            (argNum_ : int)
            (errors_ : ResizeArray<string>)
            (key : string)
            (value : string)
            : Result<unit, string option>
            =
            if System.String.Equals (key, sprintf "--%s" "token", System.StringComparison.OrdinalIgnoreCase) then
                match this.Token with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "token")
                        (x.ToString ())
                        (value.ToString ())
                    |> errors_.Add

                    Ok ()
                | None ->
                    try
                        this.Token <- value |> (fun x -> x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else
                Error None

        member this.ProcessKeyValue
            (argNum_ : int)
            (errors_ : ResizeArray<string>)
            (key : string)
            (value : string)
            : Result<unit, string option>
            =
            match this.ProcessKeyValueSelf_ argNum_ errors_ key value with
            | Ok () -> Ok ()
            | Error None -> Error None
            | Error (Some errorFromLeaf) -> Error (Some errorFromLeaf)

        /// Returns false if we didn't set a value.
        member this.SetFlagValue_ (errors_ : ResizeArray<string>) (key : string) : bool = false
        /// Compute help text for this parser, optionally noting the given prefix on each argument and indenting each line by this many spaces.
        static member HelpText_ (prefix : string option) (indent : int) : string = failwith "TODO"

    /// A partially-parsed AuthOptions.
    type internal AuthOptions_InProgress =
        {
            Token : TokenAuth_InProgress
            UsernamePassword : UsernamePasswordAuth_InProgress
        }

        /// Freeze this in-progress type. On success, returns the frozen type and the arg (if any) which consumed the input positional args.
        member this.Assemble_
            (getEnvironmentVariable : string -> string)
            (positionals : Choice<string * int, string * int> list)
            : Result<AuthOptions * string option, string list>
            =
            failwith "TODO"

        static member _Empty () : AuthOptions_InProgress =
            {
                Token = TokenAuth_InProgress._Empty ()
                UsernamePassword = UsernamePasswordAuth_InProgress._Empty ()
            }

        /// Compute help text for this parser, optionally noting the given prefix on each argument and indenting each line by this many spaces.
        static member HelpText_ (prefix : string option) (indent : int) : string = failwith "TODO"

    /// A partially-parsed DoTheThing.
    type internal DoTheThing_InProgress =
        {
            mutable Auth : AuthOptions_InProgress
            mutable Basics : BasicNoPositionals_InProgress
        }

        /// Freeze this in-progress type. On success, returns the frozen type and the arg (if any) which consumed the input positional args.
        member this.Assemble_
            (getEnvironmentVariable : string -> string)
            (positionals : Choice<string * int, string * int> list)
            : Result<DoTheThing * string option, string list>
            =
            let errors = ResizeArray<string> ()
            let positionalConsumers = ResizeArray<string> ()
            let outOfPlacePositionals : ResizeArray<string> = ResizeArray ()

            let arg0 : BasicNoPositionals =
                match this.Basics.Assemble_ getEnvironmentVariable positionals with
                | Ok (result, consumedPositional) ->
                    match consumedPositional with
                    | None -> ()
                    | Some positionalConsumer -> positionalConsumers.Add positionalConsumer

                    result
                | Error err ->
                    errors.AddRange err
                    Unchecked.defaultof<_>

            let arg1 : AuthOptions =
                match this.Auth.Assemble_ getEnvironmentVariable positionals with
                | Ok (result, consumedPositional) ->
                    match consumedPositional with
                    | None -> ()
                    | Some positionalConsumer -> positionalConsumers.Add positionalConsumer

                    result
                | Error err ->
                    errors.AddRange err
                    Unchecked.defaultof<_>

            if positionalConsumers.Count <= 1 then
                if outOfPlacePositionals.Count > 0 then
                    outOfPlacePositionals
                    |> String.concat " "
                    |> (fun x ->
                        if 0 = outOfPlacePositionals.Count then
                            "Unmatched args which look like they are meant to be flags. " + x
                        else
                            sprintf
                                "Unmatched args which look like they are meant to be flags. If you intended them as positional args, explicitly pass them with the `%s=` syntax, or place them after a trailing `--`. %s"
                                positionalConsumers.[0]
                                x
                    )
                    |> errors.Add
                else
                    ()

                if errors.Count = 0 then
                    Ok (
                        {
                            Basics = arg0
                            Auth = arg1
                        },
                        Seq.tryExactlyOne positionalConsumers
                    )
                else
                    errors |> Seq.toList |> Error
            else
                ("Multiple parsers consumed positional args; this is an error in the application, not an error by the user: "
                 + String.concat ", " positionalConsumers)
                |> List.singleton
                |> Error

        static member _Empty () : DoTheThing_InProgress =
            {
                Basics = BasicNoPositionals_InProgress._Empty ()
                Auth = AuthOptions_InProgress._Empty ()
            }

        /// Passes the key-value pair to any child records, returning Error if no key was matched.
        /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
        /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
        member this.ProcessKeyValueRecord_
            (argNum_ : int)
            (errors_ : ResizeArray<string>)
            (key : string)
            (value : string)
            : Result<unit, string option>
            =
            let errors : ResizeArray<string> = ResizeArray ()

            match this.Basics.ProcessKeyValue argNum_ errors_ key value with
            | Ok () -> Ok ()
            | Error e -> Error None

        member this.ProcessKeyValue
            (argNum_ : int)
            (errors_ : ResizeArray<string>)
            (key : string)
            (value : string)
            : Result<unit, string option>
            =
            match this.ProcessKeyValueRecord_ argNum_ errors_ key value with
            | Ok () -> Ok ()
            | Error errorFromRecord -> Error errorFromRecord

        /// Returns false if we didn't set a value.
        member this.SetFlagValue_ (errors_ : ResizeArray<string>) (key : string) : bool = false
        /// Compute help text for this parser, optionally noting the given prefix on each argument and indenting each line by this many spaces.
        static member HelpText_ (prefix : string option) (indent : int) : string = failwith "TODO"
namespace ConsumePlugin.ArgsWithUnions

open ArgParserHelpers
open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type DoTheThing
[<RequireQualifiedAccess ; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DoTheThing =
    type internal ParseState_DoTheThing =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    let parse' (getEnvironmentVariable : string -> string) (args : string list) : DoTheThing =
        let inProgress =
            ArgParseHelpers_ConsumePlugin_ArgsWithUnions.DoTheThing_InProgress._Empty ()

        let positionals : ResizeArray<Choice<string * int, string * int>> = ResizeArray ()
        let errors_ = ResizeArray ()

        let rec go (argNum_ : int) (state : ParseState_DoTheThing) (args : string list) =
            match args with
            | [] ->
                match state with
                | ParseState_DoTheThing.AwaitingKey -> ()
                | ParseState_DoTheThing.AwaitingValue key ->
                    if inProgress.SetFlagValue_ errors_ key then
                        ()
                    else
                        sprintf
                            "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                            key
                        |> errors_.Add
            | "--" :: rest -> positionals.AddRange (rest |> Seq.map (fun x -> (x, argNum_ + 1)) |> Seq.map Choice2Of2)
            | arg :: args ->
                match state with
                | ParseState_DoTheThing.AwaitingKey ->
                    if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                        if arg = "--help" then
                            "TODO" |> failwithf "Help text requested.\n%s"
                        else
                            let equals = arg.IndexOf (char 61)

                            if equals < 0 then
                                go (argNum_ + 1) (ParseState_DoTheThing.AwaitingValue arg) args
                            else
                                let key = arg.[0 .. equals - 1]
                                let value = arg.[equals + 1 ..]

                                match inProgress.ProcessKeyValue argNum_ errors_ key value with
                                | Ok () -> go (argNum_ + 1) ParseState_DoTheThing.AwaitingKey args
                                | Error x ->
                                    match x with
                                    | None ->
                                        positionals.Add (Choice1Of2 (arg, argNum_))
                                        go (argNum_ + 1) ParseState_DoTheThing.AwaitingKey args
                                    | Some msg ->
                                        sprintf "%s (at arg %s)" msg arg |> errors_.Add
                                        go (argNum_ + 1) ParseState_DoTheThing.AwaitingKey args
                    else
                        (arg, argNum_) |> Choice1Of2 |> positionals.Add
                        go (argNum_ + 1) ParseState_DoTheThing.AwaitingKey args
                | ParseState_DoTheThing.AwaitingValue key ->
                    match inProgress.ProcessKeyValue argNum_ errors_ key arg with
                    | Ok () -> go argNum_ ParseState_DoTheThing.AwaitingKey args
                    | Error exc ->
                        if inProgress.SetFlagValue_ errors_ key then
                            go argNum_ ParseState_DoTheThing.AwaitingKey (arg :: args)
                        else
                            (key, argNum_) |> Choice1Of2 |> positionals.Add
                            go (argNum_ + 1) ParseState_DoTheThing.AwaitingKey (arg :: args)

        go 0 ParseState_DoTheThing.AwaitingKey args

        if 0 = errors_.Count then
            ()
        else
            errors_
            |> String.concat System.Environment.NewLine
            |> (fun x -> "Errors during parse!\n" + x)
            |> failwith

        match inProgress.Assemble_ getEnvironmentVariable (positionals |> Seq.toList) with
        | Ok (result, posConsumer) ->
            if positionals.Count > 0 && posConsumer.IsNone then
                positionals
                |> Seq.map (fun choiceValue ->
                    match choiceValue with
                    | Choice1Of2 (arg, _) -> arg
                    | Choice2Of2 (arg, _) -> arg
                )
                |> String.concat " "
                |> sprintf "Parse error: The following arguments were not consumed: %s"
                |> failwith
            else
                result
        | Error e ->
            e
            |> String.concat System.Environment.NewLine
            |> (fun x -> "Errors during parse!\n" + x)
            |> failwith

    let parse (args : string list) : DoTheThing =
        parse' System.Environment.GetEnvironmentVariable args
