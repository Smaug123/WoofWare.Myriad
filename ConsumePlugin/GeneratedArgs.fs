namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type BasicNoPositionals
[<RequireQualifiedAccess ; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BasicNoPositionals =
    type private ParseState_BasicNoPositionals =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    let parse' (getEnvironmentVariable : string -> string) (args : string list) : BasicNoPositionals =
        let ArgParser_errors = ResizeArray ()

        let helpText () =
            [
                (sprintf "%s  int32%s%s" (sprintf "--%s" "foo") "" "")
                (sprintf "%s  string%s%s" (sprintf "--%s" "bar") "" "")
                (sprintf "%s  bool%s%s" (sprintf "--%s" "baz") "" "")
                (sprintf "%s  int32%s%s" (sprintf "--%s" "rest") " (can be repeated)" "")
            ]
            |> String.concat "\n"

        let parser_LeftoverArgs : string ResizeArray = ResizeArray ()
        let mutable arg_0 : int option = None
        let mutable arg_1 : string option = None
        let mutable arg_2 : bool option = None
        let arg_3 : int ResizeArray = ResizeArray ()

        /// Processes the key-value pair, returning Error if no key was matched.
        /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
        /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
        let processKeyValue (key : string) (value : string) : Result<unit, string option> =
            if System.String.Equals (key, sprintf "--%s" "rest", System.StringComparison.OrdinalIgnoreCase) then
                value |> (fun x -> System.Int32.Parse x) |> arg_3.Add
                () |> Ok
            else if System.String.Equals (key, sprintf "--%s" "baz", System.StringComparison.OrdinalIgnoreCase) then
                match arg_2 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "baz")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_2 <- value |> (fun x -> System.Boolean.Parse x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if System.String.Equals (key, sprintf "--%s" "bar", System.StringComparison.OrdinalIgnoreCase) then
                match arg_1 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "bar")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_1 <- value |> (fun x -> x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if System.String.Equals (key, sprintf "--%s" "foo", System.StringComparison.OrdinalIgnoreCase) then
                match arg_0 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "foo")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_0 <- value |> (fun x -> System.Int32.Parse x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else
                Error None

        /// Returns false if we didn't set a value.
        let setFlagValue (key : string) : bool =
            if System.String.Equals (key, sprintf "--%s" "baz", System.StringComparison.OrdinalIgnoreCase) then
                match arg_2 with
                | Some x ->
                    sprintf "Flag '%s' was supplied multiple times" (sprintf "--%s" "baz")
                    |> ArgParser_errors.Add

                    true
                | None ->
                    arg_2 <- true |> Some
                    true
            else
                false

        let rec go (state : ParseState_BasicNoPositionals) (args : string list) =
            match args with
            | [] ->
                match state with
                | ParseState_BasicNoPositionals.AwaitingKey -> ()
                | ParseState_BasicNoPositionals.AwaitingValue key ->
                    if setFlagValue key then
                        ()
                    else
                        sprintf
                            "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                            key
                        |> ArgParser_errors.Add
            | "--" :: rest -> parser_LeftoverArgs.AddRange (rest |> Seq.map (fun x -> x))
            | arg :: args ->
                match state with
                | ParseState_BasicNoPositionals.AwaitingKey ->
                    if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                        if arg = "--help" then
                            helpText () |> failwithf "Help text requested.\n%s"
                        else
                            let equals = arg.IndexOf (char 61)

                            if equals < 0 then
                                args |> go (ParseState_BasicNoPositionals.AwaitingValue arg)
                            else
                                let key = arg.[0 .. equals - 1]
                                let value = arg.[equals + 1 ..]

                                match processKeyValue key value with
                                | Ok () -> go ParseState_BasicNoPositionals.AwaitingKey args
                                | Error x ->
                                    match x with
                                    | None ->
                                        failwithf "Unable to process argument %s as key %s and value %s" arg key value
                                    | Some msg ->
                                        sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                        go ParseState_BasicNoPositionals.AwaitingKey args
                    else
                        arg |> (fun x -> x) |> parser_LeftoverArgs.Add
                        go ParseState_BasicNoPositionals.AwaitingKey args
                | ParseState_BasicNoPositionals.AwaitingValue key ->
                    match processKeyValue key arg with
                    | Ok () -> go ParseState_BasicNoPositionals.AwaitingKey args
                    | Error exc ->
                        if setFlagValue key then
                            go ParseState_BasicNoPositionals.AwaitingKey (arg :: args)
                        else
                            match exc with
                            | None ->
                                failwithf "Unable to process supplied arg %s. Help text follows.\n%s" key (helpText ())
                            | Some msg -> msg |> ArgParser_errors.Add

        go ParseState_BasicNoPositionals.AwaitingKey args

        let parser_LeftoverArgs =
            if 0 = parser_LeftoverArgs.Count then
                ()
            else
                parser_LeftoverArgs
                |> String.concat " "
                |> sprintf "There were leftover args: %s"
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>

        let arg_0 =
            match arg_0 with
            | None ->
                sprintf "Required argument '%s' received no value" (sprintf "--%s" "foo")
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>
            | Some x -> x

        let arg_1 =
            match arg_1 with
            | None ->
                sprintf "Required argument '%s' received no value" (sprintf "--%s" "bar")
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>
            | Some x -> x

        let arg_2 =
            match arg_2 with
            | None ->
                sprintf "Required argument '%s' received no value" (sprintf "--%s" "baz")
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>
            | Some x -> x

        let arg_3 = arg_3 |> Seq.toList

        if 0 = ArgParser_errors.Count then
            {
                Bar = arg_1
                Baz = arg_2
                Foo = arg_0
                Rest = arg_3
            }
        else
            ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

    let parse (args : string list) : BasicNoPositionals =
        parse' System.Environment.GetEnvironmentVariable args
namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type Basic
[<RequireQualifiedAccess ; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Basic =
    type private ParseState_Basic =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    let parse' (getEnvironmentVariable : string -> string) (args : string list) : Basic =
        let ArgParser_errors = ResizeArray ()

        let helpText () =
            [
                (sprintf "%s  int32%s%s" (sprintf "--%s" "foo") "" (sprintf " : %s" ("This is a foo!")))
                (sprintf "%s  string%s%s" (sprintf "--%s" "bar") "" "")
                (sprintf "%s  bool%s%s" (sprintf "--%s" "baz") "" "")
                (sprintf
                    "%s  string%s%s"
                    (sprintf "--%s" "rest")
                    " (positional args) (can be repeated)"
                    (sprintf " : %s" ("Here's where the rest of the args go")))
            ]
            |> String.concat "\n"

        let arg_3 : string ResizeArray = ResizeArray ()
        let mutable arg_0 : int option = None
        let mutable arg_1 : string option = None
        let mutable arg_2 : bool option = None

        /// Processes the key-value pair, returning Error if no key was matched.
        /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
        /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
        let processKeyValue (key : string) (value : string) : Result<unit, string option> =
            if System.String.Equals (key, sprintf "--%s" "baz", System.StringComparison.OrdinalIgnoreCase) then
                match arg_2 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "baz")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_2 <- value |> (fun x -> System.Boolean.Parse x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if System.String.Equals (key, sprintf "--%s" "bar", System.StringComparison.OrdinalIgnoreCase) then
                match arg_1 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "bar")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_1 <- value |> (fun x -> x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if System.String.Equals (key, sprintf "--%s" "foo", System.StringComparison.OrdinalIgnoreCase) then
                match arg_0 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "foo")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_0 <- value |> (fun x -> System.Int32.Parse x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if System.String.Equals (key, sprintf "--%s" "rest", System.StringComparison.OrdinalIgnoreCase) then
                value |> (fun x -> x) |> arg_3.Add
                () |> Ok
            else
                Error None

        /// Returns false if we didn't set a value.
        let setFlagValue (key : string) : bool =
            if System.String.Equals (key, sprintf "--%s" "baz", System.StringComparison.OrdinalIgnoreCase) then
                match arg_2 with
                | Some x ->
                    sprintf "Flag '%s' was supplied multiple times" (sprintf "--%s" "baz")
                    |> ArgParser_errors.Add

                    true
                | None ->
                    arg_2 <- true |> Some
                    true
            else
                false

        let rec go (state : ParseState_Basic) (args : string list) =
            match args with
            | [] ->
                match state with
                | ParseState_Basic.AwaitingKey -> ()
                | ParseState_Basic.AwaitingValue key ->
                    if setFlagValue key then
                        ()
                    else
                        sprintf
                            "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                            key
                        |> ArgParser_errors.Add
            | "--" :: rest -> arg_3.AddRange (rest |> Seq.map (fun x -> x))
            | arg :: args ->
                match state with
                | ParseState_Basic.AwaitingKey ->
                    if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                        if arg = "--help" then
                            helpText () |> failwithf "Help text requested.\n%s"
                        else
                            let equals = arg.IndexOf (char 61)

                            if equals < 0 then
                                args |> go (ParseState_Basic.AwaitingValue arg)
                            else
                                let key = arg.[0 .. equals - 1]
                                let value = arg.[equals + 1 ..]

                                match processKeyValue key value with
                                | Ok () -> go ParseState_Basic.AwaitingKey args
                                | Error x ->
                                    match x with
                                    | None ->
                                        failwithf "Unable to process argument %s as key %s and value %s" arg key value
                                    | Some msg ->
                                        sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                        go ParseState_Basic.AwaitingKey args
                    else
                        arg |> (fun x -> x) |> arg_3.Add
                        go ParseState_Basic.AwaitingKey args
                | ParseState_Basic.AwaitingValue key ->
                    match processKeyValue key arg with
                    | Ok () -> go ParseState_Basic.AwaitingKey args
                    | Error exc ->
                        if setFlagValue key then
                            go ParseState_Basic.AwaitingKey (arg :: args)
                        else
                            match exc with
                            | None ->
                                failwithf "Unable to process supplied arg %s. Help text follows.\n%s" key (helpText ())
                            | Some msg -> msg |> ArgParser_errors.Add

        go ParseState_Basic.AwaitingKey args
        let arg_3 = arg_3 |> Seq.toList

        let arg_0 =
            match arg_0 with
            | None ->
                sprintf "Required argument '%s' received no value" (sprintf "--%s" "foo")
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>
            | Some x -> x

        let arg_1 =
            match arg_1 with
            | None ->
                sprintf "Required argument '%s' received no value" (sprintf "--%s" "bar")
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>
            | Some x -> x

        let arg_2 =
            match arg_2 with
            | None ->
                sprintf "Required argument '%s' received no value" (sprintf "--%s" "baz")
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>
            | Some x -> x

        if 0 = ArgParser_errors.Count then
            {
                Bar = arg_1
                Baz = arg_2
                Foo = arg_0
                Rest = arg_3
            }
        else
            ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

    let parse (args : string list) : Basic =
        parse' System.Environment.GetEnvironmentVariable args
namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type BasicWithIntPositionals
[<RequireQualifiedAccess ; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BasicWithIntPositionals =
    type private ParseState_BasicWithIntPositionals =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    let parse' (getEnvironmentVariable : string -> string) (args : string list) : BasicWithIntPositionals =
        let ArgParser_errors = ResizeArray ()

        let helpText () =
            [
                (sprintf "%s  int32%s%s" (sprintf "--%s" "foo") "" "")
                (sprintf "%s  string%s%s" (sprintf "--%s" "bar") "" "")
                (sprintf "%s  bool%s%s" (sprintf "--%s" "baz") "" "")
                (sprintf "%s  int32%s%s" (sprintf "--%s" "rest") " (positional args) (can be repeated)" "")
            ]
            |> String.concat "\n"

        let arg_3 : int ResizeArray = ResizeArray ()
        let mutable arg_0 : int option = None
        let mutable arg_1 : string option = None
        let mutable arg_2 : bool option = None

        /// Processes the key-value pair, returning Error if no key was matched.
        /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
        /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
        let processKeyValue (key : string) (value : string) : Result<unit, string option> =
            if System.String.Equals (key, sprintf "--%s" "baz", System.StringComparison.OrdinalIgnoreCase) then
                match arg_2 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "baz")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_2 <- value |> (fun x -> System.Boolean.Parse x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if System.String.Equals (key, sprintf "--%s" "bar", System.StringComparison.OrdinalIgnoreCase) then
                match arg_1 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "bar")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_1 <- value |> (fun x -> x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if System.String.Equals (key, sprintf "--%s" "foo", System.StringComparison.OrdinalIgnoreCase) then
                match arg_0 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "foo")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_0 <- value |> (fun x -> System.Int32.Parse x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if System.String.Equals (key, sprintf "--%s" "rest", System.StringComparison.OrdinalIgnoreCase) then
                value |> (fun x -> System.Int32.Parse x) |> arg_3.Add
                () |> Ok
            else
                Error None

        /// Returns false if we didn't set a value.
        let setFlagValue (key : string) : bool =
            if System.String.Equals (key, sprintf "--%s" "baz", System.StringComparison.OrdinalIgnoreCase) then
                match arg_2 with
                | Some x ->
                    sprintf "Flag '%s' was supplied multiple times" (sprintf "--%s" "baz")
                    |> ArgParser_errors.Add

                    true
                | None ->
                    arg_2 <- true |> Some
                    true
            else
                false

        let rec go (state : ParseState_BasicWithIntPositionals) (args : string list) =
            match args with
            | [] ->
                match state with
                | ParseState_BasicWithIntPositionals.AwaitingKey -> ()
                | ParseState_BasicWithIntPositionals.AwaitingValue key ->
                    if setFlagValue key then
                        ()
                    else
                        sprintf
                            "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                            key
                        |> ArgParser_errors.Add
            | "--" :: rest -> arg_3.AddRange (rest |> Seq.map (fun x -> System.Int32.Parse x))
            | arg :: args ->
                match state with
                | ParseState_BasicWithIntPositionals.AwaitingKey ->
                    if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                        if arg = "--help" then
                            helpText () |> failwithf "Help text requested.\n%s"
                        else
                            let equals = arg.IndexOf (char 61)

                            if equals < 0 then
                                args |> go (ParseState_BasicWithIntPositionals.AwaitingValue arg)
                            else
                                let key = arg.[0 .. equals - 1]
                                let value = arg.[equals + 1 ..]

                                match processKeyValue key value with
                                | Ok () -> go ParseState_BasicWithIntPositionals.AwaitingKey args
                                | Error x ->
                                    match x with
                                    | None ->
                                        failwithf "Unable to process argument %s as key %s and value %s" arg key value
                                    | Some msg ->
                                        sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                        go ParseState_BasicWithIntPositionals.AwaitingKey args
                    else
                        arg |> (fun x -> System.Int32.Parse x) |> arg_3.Add
                        go ParseState_BasicWithIntPositionals.AwaitingKey args
                | ParseState_BasicWithIntPositionals.AwaitingValue key ->
                    match processKeyValue key arg with
                    | Ok () -> go ParseState_BasicWithIntPositionals.AwaitingKey args
                    | Error exc ->
                        if setFlagValue key then
                            go ParseState_BasicWithIntPositionals.AwaitingKey (arg :: args)
                        else
                            match exc with
                            | None ->
                                failwithf "Unable to process supplied arg %s. Help text follows.\n%s" key (helpText ())
                            | Some msg -> msg |> ArgParser_errors.Add

        go ParseState_BasicWithIntPositionals.AwaitingKey args
        let arg_3 = arg_3 |> Seq.toList

        let arg_0 =
            match arg_0 with
            | None ->
                sprintf "Required argument '%s' received no value" (sprintf "--%s" "foo")
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>
            | Some x -> x

        let arg_1 =
            match arg_1 with
            | None ->
                sprintf "Required argument '%s' received no value" (sprintf "--%s" "bar")
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>
            | Some x -> x

        let arg_2 =
            match arg_2 with
            | None ->
                sprintf "Required argument '%s' received no value" (sprintf "--%s" "baz")
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>
            | Some x -> x

        if 0 = ArgParser_errors.Count then
            {
                Bar = arg_1
                Baz = arg_2
                Foo = arg_0
                Rest = arg_3
            }
        else
            ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

    let parse (args : string list) : BasicWithIntPositionals =
        parse' System.Environment.GetEnvironmentVariable args
namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type LoadsOfTypes
[<RequireQualifiedAccess ; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LoadsOfTypes =
    type private ParseState_LoadsOfTypes =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    let parse' (getEnvironmentVariable : string -> string) (args : string list) : LoadsOfTypes =
        let ArgParser_errors = ResizeArray ()

        let helpText () =
            [
                (sprintf "%s  int32%s%s" (sprintf "--%s" "foo") "" "")
                (sprintf "%s  string%s%s" (sprintf "--%s" "bar") "" "")
                (sprintf "%s  bool%s%s" (sprintf "--%s" "baz") "" "")
                (sprintf "%s  FileInfo%s%s" (sprintf "--%s" "some-file") "" "")
                (sprintf "%s  DirectoryInfo%s%s" (sprintf "--%s" "some-directory") "" "")
                (sprintf "%s  DirectoryInfo%s%s" (sprintf "--%s" "some-list") " (can be repeated)" "")
                (sprintf "%s  int32%s%s" (sprintf "--%s" "optional-thing-with-no-default") " (optional)" "")

                (sprintf
                    "%s  bool%s%s"
                    (sprintf "--%s" "optional-thing")
                    (LoadsOfTypes.DefaultOptionalThing ()
                     |> (fun x -> x.ToString ())
                     |> sprintf " (default value: %s)")
                    "")

                (sprintf
                    "%s  int32%s%s"
                    (sprintf "--%s" "another-optional-thing")
                    (LoadsOfTypes.DefaultAnotherOptionalThing ()
                     |> (fun x -> x.ToString ())
                     |> sprintf " (default value: %s)")
                    "")

                (sprintf
                    "%s  string%s%s"
                    (sprintf "--%s" "yet-another-optional-thing")
                    ("CONSUMEPLUGIN_THINGS" |> sprintf " (default value populated from env var %s)")
                    "")
                (sprintf "%s  int32%s%s" (sprintf "--%s" "positionals") " (positional args) (can be repeated)" "")
            ]
            |> String.concat "\n"

        let arg_7 : int ResizeArray = ResizeArray ()
        let mutable arg_0 : int option = None
        let mutable arg_1 : string option = None
        let mutable arg_2 : bool option = None
        let mutable arg_3 : FileInfo option = None
        let mutable arg_4 : DirectoryInfo option = None
        let arg_5 : DirectoryInfo ResizeArray = ResizeArray ()
        let mutable arg_6 : int option = None
        let mutable arg_8 : bool option = None
        let mutable arg_9 : int option = None
        let mutable arg_10 : string option = None

        /// Processes the key-value pair, returning Error if no key was matched.
        /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
        /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
        let processKeyValue (key : string) (value : string) : Result<unit, string option> =
            if
                System.String.Equals (
                    key,
                    sprintf "--%s" "yet-another-optional-thing",
                    System.StringComparison.OrdinalIgnoreCase
                )
            then
                match arg_10 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "yet-another-optional-thing")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_10 <- value |> (fun x -> x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if
                System.String.Equals (
                    key,
                    sprintf "--%s" "another-optional-thing",
                    System.StringComparison.OrdinalIgnoreCase
                )
            then
                match arg_9 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "another-optional-thing")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_9 <- value |> (fun x -> System.Int32.Parse x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if
                System.String.Equals (key, sprintf "--%s" "optional-thing", System.StringComparison.OrdinalIgnoreCase)
            then
                match arg_8 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "optional-thing")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_8 <- value |> (fun x -> System.Boolean.Parse x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if
                System.String.Equals (
                    key,
                    sprintf "--%s" "optional-thing-with-no-default",
                    System.StringComparison.OrdinalIgnoreCase
                )
            then
                match arg_6 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "optional-thing-with-no-default")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_6 <- value |> (fun x -> System.Int32.Parse x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if
                System.String.Equals (key, sprintf "--%s" "some-list", System.StringComparison.OrdinalIgnoreCase)
            then
                value |> (fun x -> System.IO.DirectoryInfo x) |> arg_5.Add
                () |> Ok
            else if
                System.String.Equals (key, sprintf "--%s" "some-directory", System.StringComparison.OrdinalIgnoreCase)
            then
                match arg_4 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "some-directory")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_4 <- value |> (fun x -> System.IO.DirectoryInfo x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if
                System.String.Equals (key, sprintf "--%s" "some-file", System.StringComparison.OrdinalIgnoreCase)
            then
                match arg_3 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "some-file")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_3 <- value |> (fun x -> System.IO.FileInfo x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if System.String.Equals (key, sprintf "--%s" "baz", System.StringComparison.OrdinalIgnoreCase) then
                match arg_2 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "baz")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_2 <- value |> (fun x -> System.Boolean.Parse x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if System.String.Equals (key, sprintf "--%s" "bar", System.StringComparison.OrdinalIgnoreCase) then
                match arg_1 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "bar")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_1 <- value |> (fun x -> x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if System.String.Equals (key, sprintf "--%s" "foo", System.StringComparison.OrdinalIgnoreCase) then
                match arg_0 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "foo")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_0 <- value |> (fun x -> System.Int32.Parse x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if
                System.String.Equals (key, sprintf "--%s" "positionals", System.StringComparison.OrdinalIgnoreCase)
            then
                value |> (fun x -> System.Int32.Parse x) |> arg_7.Add
                () |> Ok
            else
                Error None

        /// Returns false if we didn't set a value.
        let setFlagValue (key : string) : bool =
            if
                System.String.Equals (key, sprintf "--%s" "optional-thing", System.StringComparison.OrdinalIgnoreCase)
            then
                match arg_8 with
                | Some x ->
                    sprintf "Flag '%s' was supplied multiple times" (sprintf "--%s" "optional-thing")
                    |> ArgParser_errors.Add

                    true
                | None ->
                    arg_8 <- true |> Some
                    true
            else if System.String.Equals (key, sprintf "--%s" "baz", System.StringComparison.OrdinalIgnoreCase) then
                match arg_2 with
                | Some x ->
                    sprintf "Flag '%s' was supplied multiple times" (sprintf "--%s" "baz")
                    |> ArgParser_errors.Add

                    true
                | None ->
                    arg_2 <- true |> Some
                    true
            else
                false

        let rec go (state : ParseState_LoadsOfTypes) (args : string list) =
            match args with
            | [] ->
                match state with
                | ParseState_LoadsOfTypes.AwaitingKey -> ()
                | ParseState_LoadsOfTypes.AwaitingValue key ->
                    if setFlagValue key then
                        ()
                    else
                        sprintf
                            "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                            key
                        |> ArgParser_errors.Add
            | "--" :: rest -> arg_7.AddRange (rest |> Seq.map (fun x -> System.Int32.Parse x))
            | arg :: args ->
                match state with
                | ParseState_LoadsOfTypes.AwaitingKey ->
                    if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                        if arg = "--help" then
                            helpText () |> failwithf "Help text requested.\n%s"
                        else
                            let equals = arg.IndexOf (char 61)

                            if equals < 0 then
                                args |> go (ParseState_LoadsOfTypes.AwaitingValue arg)
                            else
                                let key = arg.[0 .. equals - 1]
                                let value = arg.[equals + 1 ..]

                                match processKeyValue key value with
                                | Ok () -> go ParseState_LoadsOfTypes.AwaitingKey args
                                | Error x ->
                                    match x with
                                    | None ->
                                        failwithf "Unable to process argument %s as key %s and value %s" arg key value
                                    | Some msg ->
                                        sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                        go ParseState_LoadsOfTypes.AwaitingKey args
                    else
                        arg |> (fun x -> System.Int32.Parse x) |> arg_7.Add
                        go ParseState_LoadsOfTypes.AwaitingKey args
                | ParseState_LoadsOfTypes.AwaitingValue key ->
                    match processKeyValue key arg with
                    | Ok () -> go ParseState_LoadsOfTypes.AwaitingKey args
                    | Error exc ->
                        if setFlagValue key then
                            go ParseState_LoadsOfTypes.AwaitingKey (arg :: args)
                        else
                            match exc with
                            | None ->
                                failwithf "Unable to process supplied arg %s. Help text follows.\n%s" key (helpText ())
                            | Some msg -> msg |> ArgParser_errors.Add

        go ParseState_LoadsOfTypes.AwaitingKey args
        let arg_7 = arg_7 |> Seq.toList

        let arg_0 =
            match arg_0 with
            | None ->
                sprintf "Required argument '%s' received no value" (sprintf "--%s" "foo")
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>
            | Some x -> x

        let arg_1 =
            match arg_1 with
            | None ->
                sprintf "Required argument '%s' received no value" (sprintf "--%s" "bar")
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>
            | Some x -> x

        let arg_2 =
            match arg_2 with
            | None ->
                sprintf "Required argument '%s' received no value" (sprintf "--%s" "baz")
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>
            | Some x -> x

        let arg_3 =
            match arg_3 with
            | None ->
                sprintf "Required argument '%s' received no value" (sprintf "--%s" "some-file")
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>
            | Some x -> x

        let arg_4 =
            match arg_4 with
            | None ->
                sprintf "Required argument '%s' received no value" (sprintf "--%s" "some-directory")
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>
            | Some x -> x

        let arg_5 = arg_5 |> Seq.toList
        let arg_6 = arg_6

        let arg_8 =
            match arg_8 with
            | None -> LoadsOfTypes.DefaultOptionalThing () |> Choice2Of2
            | Some x -> Choice1Of2 x

        let arg_9 =
            match arg_9 with
            | None -> LoadsOfTypes.DefaultAnotherOptionalThing () |> Choice2Of2
            | Some x -> Choice1Of2 x

        let arg_10 =
            match arg_10 with
            | None ->
                match "CONSUMEPLUGIN_THINGS" |> getEnvironmentVariable with
                | null ->
                    sprintf
                        "No value was supplied for %s, nor was environment variable %s set"
                        (sprintf "--%s" "yet-another-optional-thing")
                        "CONSUMEPLUGIN_THINGS"
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | x -> x |> (fun x -> x)
                |> Choice2Of2
            | Some x -> Choice1Of2 x

        if 0 = ArgParser_errors.Count then
            {
                AnotherOptionalThing = arg_9
                Bar = arg_1
                Baz = arg_2
                Foo = arg_0
                OptionalThing = arg_8
                OptionalThingWithNoDefault = arg_6
                Positionals = arg_7
                SomeDirectory = arg_4
                SomeFile = arg_3
                SomeList = arg_5
                YetAnotherOptionalThing = arg_10
            }
        else
            ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

    let parse (args : string list) : LoadsOfTypes =
        parse' System.Environment.GetEnvironmentVariable args
namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type LoadsOfTypesNoPositionals
[<RequireQualifiedAccess ; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LoadsOfTypesNoPositionals =
    type private ParseState_LoadsOfTypesNoPositionals =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    let parse' (getEnvironmentVariable : string -> string) (args : string list) : LoadsOfTypesNoPositionals =
        let ArgParser_errors = ResizeArray ()

        let helpText () =
            [
                (sprintf "%s  int32%s%s" (sprintf "--%s" "foo") "" "")
                (sprintf "%s  string%s%s" (sprintf "--%s" "bar") "" "")
                (sprintf "%s  bool%s%s" (sprintf "--%s" "baz") "" "")
                (sprintf "%s  FileInfo%s%s" (sprintf "--%s" "some-file") "" "")
                (sprintf "%s  DirectoryInfo%s%s" (sprintf "--%s" "some-directory") "" "")
                (sprintf "%s  DirectoryInfo%s%s" (sprintf "--%s" "some-list") " (can be repeated)" "")
                (sprintf "%s  int32%s%s" (sprintf "--%s" "optional-thing-with-no-default") " (optional)" "")

                (sprintf
                    "%s  bool%s%s"
                    (sprintf "--%s" "optional-thing")
                    (LoadsOfTypesNoPositionals.DefaultOptionalThing ()
                     |> (fun x -> x.ToString ())
                     |> sprintf " (default value: %s)")
                    "")

                (sprintf
                    "%s  int32%s%s"
                    (sprintf "--%s" "another-optional-thing")
                    (LoadsOfTypesNoPositionals.DefaultAnotherOptionalThing ()
                     |> (fun x -> x.ToString ())
                     |> sprintf " (default value: %s)")
                    "")
                (sprintf
                    "%s  string%s%s"
                    (sprintf "--%s" "yet-another-optional-thing")
                    ("CONSUMEPLUGIN_THINGS" |> sprintf " (default value populated from env var %s)")
                    "")
            ]
            |> String.concat "\n"

        let parser_LeftoverArgs : string ResizeArray = ResizeArray ()
        let mutable arg_0 : int option = None
        let mutable arg_1 : string option = None
        let mutable arg_2 : bool option = None
        let mutable arg_3 : FileInfo option = None
        let mutable arg_4 : DirectoryInfo option = None
        let arg_5 : DirectoryInfo ResizeArray = ResizeArray ()
        let mutable arg_6 : int option = None
        let mutable arg_7 : bool option = None
        let mutable arg_8 : int option = None
        let mutable arg_9 : string option = None

        /// Processes the key-value pair, returning Error if no key was matched.
        /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
        /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
        let processKeyValue (key : string) (value : string) : Result<unit, string option> =
            if
                System.String.Equals (
                    key,
                    sprintf "--%s" "yet-another-optional-thing",
                    System.StringComparison.OrdinalIgnoreCase
                )
            then
                match arg_9 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "yet-another-optional-thing")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_9 <- value |> (fun x -> x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if
                System.String.Equals (
                    key,
                    sprintf "--%s" "another-optional-thing",
                    System.StringComparison.OrdinalIgnoreCase
                )
            then
                match arg_8 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "another-optional-thing")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_8 <- value |> (fun x -> System.Int32.Parse x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if
                System.String.Equals (key, sprintf "--%s" "optional-thing", System.StringComparison.OrdinalIgnoreCase)
            then
                match arg_7 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "optional-thing")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_7 <- value |> (fun x -> System.Boolean.Parse x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if
                System.String.Equals (
                    key,
                    sprintf "--%s" "optional-thing-with-no-default",
                    System.StringComparison.OrdinalIgnoreCase
                )
            then
                match arg_6 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "optional-thing-with-no-default")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_6 <- value |> (fun x -> System.Int32.Parse x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if
                System.String.Equals (key, sprintf "--%s" "some-list", System.StringComparison.OrdinalIgnoreCase)
            then
                value |> (fun x -> System.IO.DirectoryInfo x) |> arg_5.Add
                () |> Ok
            else if
                System.String.Equals (key, sprintf "--%s" "some-directory", System.StringComparison.OrdinalIgnoreCase)
            then
                match arg_4 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "some-directory")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_4 <- value |> (fun x -> System.IO.DirectoryInfo x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if
                System.String.Equals (key, sprintf "--%s" "some-file", System.StringComparison.OrdinalIgnoreCase)
            then
                match arg_3 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "some-file")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_3 <- value |> (fun x -> System.IO.FileInfo x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if System.String.Equals (key, sprintf "--%s" "baz", System.StringComparison.OrdinalIgnoreCase) then
                match arg_2 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "baz")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_2 <- value |> (fun x -> System.Boolean.Parse x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if System.String.Equals (key, sprintf "--%s" "bar", System.StringComparison.OrdinalIgnoreCase) then
                match arg_1 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "bar")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_1 <- value |> (fun x -> x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else if System.String.Equals (key, sprintf "--%s" "foo", System.StringComparison.OrdinalIgnoreCase) then
                match arg_0 with
                | Some x ->
                    sprintf
                        "Argument '%s' was supplied multiple times: %s and %s"
                        (sprintf "--%s" "foo")
                        (x.ToString ())
                        (value.ToString ())
                    |> ArgParser_errors.Add

                    Ok ()
                | None ->
                    try
                        arg_0 <- value |> (fun x -> System.Int32.Parse x) |> Some
                        Ok ()
                    with _ as exc ->
                        exc.Message |> Some |> Error
            else
                Error None

        /// Returns false if we didn't set a value.
        let setFlagValue (key : string) : bool =
            if
                System.String.Equals (key, sprintf "--%s" "optional-thing", System.StringComparison.OrdinalIgnoreCase)
            then
                match arg_7 with
                | Some x ->
                    sprintf "Flag '%s' was supplied multiple times" (sprintf "--%s" "optional-thing")
                    |> ArgParser_errors.Add

                    true
                | None ->
                    arg_7 <- true |> Some
                    true
            else if System.String.Equals (key, sprintf "--%s" "baz", System.StringComparison.OrdinalIgnoreCase) then
                match arg_2 with
                | Some x ->
                    sprintf "Flag '%s' was supplied multiple times" (sprintf "--%s" "baz")
                    |> ArgParser_errors.Add

                    true
                | None ->
                    arg_2 <- true |> Some
                    true
            else
                false

        let rec go (state : ParseState_LoadsOfTypesNoPositionals) (args : string list) =
            match args with
            | [] ->
                match state with
                | ParseState_LoadsOfTypesNoPositionals.AwaitingKey -> ()
                | ParseState_LoadsOfTypesNoPositionals.AwaitingValue key ->
                    if setFlagValue key then
                        ()
                    else
                        sprintf
                            "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                            key
                        |> ArgParser_errors.Add
            | "--" :: rest -> parser_LeftoverArgs.AddRange (rest |> Seq.map (fun x -> x))
            | arg :: args ->
                match state with
                | ParseState_LoadsOfTypesNoPositionals.AwaitingKey ->
                    if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                        if arg = "--help" then
                            helpText () |> failwithf "Help text requested.\n%s"
                        else
                            let equals = arg.IndexOf (char 61)

                            if equals < 0 then
                                args |> go (ParseState_LoadsOfTypesNoPositionals.AwaitingValue arg)
                            else
                                let key = arg.[0 .. equals - 1]
                                let value = arg.[equals + 1 ..]

                                match processKeyValue key value with
                                | Ok () -> go ParseState_LoadsOfTypesNoPositionals.AwaitingKey args
                                | Error x ->
                                    match x with
                                    | None ->
                                        failwithf "Unable to process argument %s as key %s and value %s" arg key value
                                    | Some msg ->
                                        sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                        go ParseState_LoadsOfTypesNoPositionals.AwaitingKey args
                    else
                        arg |> (fun x -> x) |> parser_LeftoverArgs.Add
                        go ParseState_LoadsOfTypesNoPositionals.AwaitingKey args
                | ParseState_LoadsOfTypesNoPositionals.AwaitingValue key ->
                    match processKeyValue key arg with
                    | Ok () -> go ParseState_LoadsOfTypesNoPositionals.AwaitingKey args
                    | Error exc ->
                        if setFlagValue key then
                            go ParseState_LoadsOfTypesNoPositionals.AwaitingKey (arg :: args)
                        else
                            match exc with
                            | None ->
                                failwithf "Unable to process supplied arg %s. Help text follows.\n%s" key (helpText ())
                            | Some msg -> msg |> ArgParser_errors.Add

        go ParseState_LoadsOfTypesNoPositionals.AwaitingKey args

        let parser_LeftoverArgs =
            if 0 = parser_LeftoverArgs.Count then
                ()
            else
                parser_LeftoverArgs
                |> String.concat " "
                |> sprintf "There were leftover args: %s"
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>

        let arg_0 =
            match arg_0 with
            | None ->
                sprintf "Required argument '%s' received no value" (sprintf "--%s" "foo")
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>
            | Some x -> x

        let arg_1 =
            match arg_1 with
            | None ->
                sprintf "Required argument '%s' received no value" (sprintf "--%s" "bar")
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>
            | Some x -> x

        let arg_2 =
            match arg_2 with
            | None ->
                sprintf "Required argument '%s' received no value" (sprintf "--%s" "baz")
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>
            | Some x -> x

        let arg_3 =
            match arg_3 with
            | None ->
                sprintf "Required argument '%s' received no value" (sprintf "--%s" "some-file")
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>
            | Some x -> x

        let arg_4 =
            match arg_4 with
            | None ->
                sprintf "Required argument '%s' received no value" (sprintf "--%s" "some-directory")
                |> ArgParser_errors.Add

                Unchecked.defaultof<_>
            | Some x -> x

        let arg_5 = arg_5 |> Seq.toList
        let arg_6 = arg_6

        let arg_7 =
            match arg_7 with
            | None -> LoadsOfTypesNoPositionals.DefaultOptionalThing () |> Choice2Of2
            | Some x -> Choice1Of2 x

        let arg_8 =
            match arg_8 with
            | None -> LoadsOfTypesNoPositionals.DefaultAnotherOptionalThing () |> Choice2Of2
            | Some x -> Choice1Of2 x

        let arg_9 =
            match arg_9 with
            | None ->
                match "CONSUMEPLUGIN_THINGS" |> getEnvironmentVariable with
                | null ->
                    sprintf
                        "No value was supplied for %s, nor was environment variable %s set"
                        (sprintf "--%s" "yet-another-optional-thing")
                        "CONSUMEPLUGIN_THINGS"
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | x -> x |> (fun x -> x)
                |> Choice2Of2
            | Some x -> Choice1Of2 x

        if 0 = ArgParser_errors.Count then
            {
                AnotherOptionalThing = arg_8
                Bar = arg_1
                Baz = arg_2
                Foo = arg_0
                OptionalThing = arg_7
                OptionalThingWithNoDefault = arg_6
                SomeDirectory = arg_4
                SomeFile = arg_3
                SomeList = arg_5
                YetAnotherOptionalThing = arg_9
            }
        else
            ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

    let parse (args : string list) : LoadsOfTypesNoPositionals =
        parse' System.Environment.GetEnvironmentVariable args
namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type DatesAndTimes
[<AutoOpen>]
module DatesAndTimesArgParse =
    type private ParseState_DatesAndTimes =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    /// Extension methods for argument parsing
    type DatesAndTimes with

        static member parse' (getEnvironmentVariable : string -> string) (args : string list) : DatesAndTimes =
            let ArgParser_errors = ResizeArray ()

            let helpText () =
                [
                    (sprintf "%s  TimeSpan%s%s" (sprintf "--%s" "plain") "" "")
                    (sprintf "%s  TimeSpan%s%s" (sprintf "--%s" "invariant") "" "")

                    (sprintf
                        "%s  TimeSpan%s%s"
                        (sprintf "--%s" "exact")
                        ""
                        (sprintf " : %s" (sprintf "%s [Parse format (.NET): %s]" "An exact time please" @"hh\:mm\:ss")))
                    (sprintf
                        "%s  TimeSpan%s%s"
                        (sprintf "--%s" "invariant-exact")
                        ""
                        (sprintf " : %s" (sprintf "[Parse format (.NET): %s]" @"hh\:mm\:ss")))
                ]
                |> String.concat "\n"

            let parser_LeftoverArgs : string ResizeArray = ResizeArray ()
            let mutable arg_0 : TimeSpan option = None
            let mutable arg_1 : TimeSpan option = None
            let mutable arg_2 : TimeSpan option = None
            let mutable arg_3 : TimeSpan option = None

            /// Processes the key-value pair, returning Error if no key was matched.
            /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
            /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
            let processKeyValue (key : string) (value : string) : Result<unit, string option> =
                if
                    System.String.Equals (
                        key,
                        sprintf "--%s" "invariant-exact",
                        System.StringComparison.OrdinalIgnoreCase
                    )
                then
                    match arg_3 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "invariant-exact")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_3 <-
                                value
                                |> (fun x ->
                                    System.TimeSpan.ParseExact (
                                        x,
                                        @"hh\:mm\:ss",
                                        System.Globalization.CultureInfo.InvariantCulture
                                    )
                                )
                                |> Some

                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else if
                    System.String.Equals (key, sprintf "--%s" "exact", System.StringComparison.OrdinalIgnoreCase)
                then
                    match arg_2 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "exact")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_2 <-
                                value
                                |> (fun x ->
                                    System.TimeSpan.ParseExact (
                                        x,
                                        @"hh\:mm\:ss",
                                        System.Globalization.CultureInfo.CurrentCulture
                                    )
                                )
                                |> Some

                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else if
                    System.String.Equals (key, sprintf "--%s" "invariant", System.StringComparison.OrdinalIgnoreCase)
                then
                    match arg_1 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "invariant")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_1 <-
                                value
                                |> (fun x ->
                                    System.TimeSpan.Parse (x, System.Globalization.CultureInfo.InvariantCulture)
                                )
                                |> Some

                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else if
                    System.String.Equals (key, sprintf "--%s" "plain", System.StringComparison.OrdinalIgnoreCase)
                then
                    match arg_0 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "plain")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_0 <- value |> (fun x -> System.TimeSpan.Parse x) |> Some
                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else
                    Error None

            /// Returns false if we didn't set a value.
            let setFlagValue (key : string) : bool = false

            let rec go (state : ParseState_DatesAndTimes) (args : string list) =
                match args with
                | [] ->
                    match state with
                    | ParseState_DatesAndTimes.AwaitingKey -> ()
                    | ParseState_DatesAndTimes.AwaitingValue key ->
                        if setFlagValue key then
                            ()
                        else
                            sprintf
                                "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                                key
                            |> ArgParser_errors.Add
                | "--" :: rest -> parser_LeftoverArgs.AddRange (rest |> Seq.map (fun x -> x))
                | arg :: args ->
                    match state with
                    | ParseState_DatesAndTimes.AwaitingKey ->
                        if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                            if arg = "--help" then
                                helpText () |> failwithf "Help text requested.\n%s"
                            else
                                let equals = arg.IndexOf (char 61)

                                if equals < 0 then
                                    args |> go (ParseState_DatesAndTimes.AwaitingValue arg)
                                else
                                    let key = arg.[0 .. equals - 1]
                                    let value = arg.[equals + 1 ..]

                                    match processKeyValue key value with
                                    | Ok () -> go ParseState_DatesAndTimes.AwaitingKey args
                                    | Error x ->
                                        match x with
                                        | None ->
                                            failwithf
                                                "Unable to process argument %s as key %s and value %s"
                                                arg
                                                key
                                                value
                                        | Some msg ->
                                            sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                            go ParseState_DatesAndTimes.AwaitingKey args
                        else
                            arg |> (fun x -> x) |> parser_LeftoverArgs.Add
                            go ParseState_DatesAndTimes.AwaitingKey args
                    | ParseState_DatesAndTimes.AwaitingValue key ->
                        match processKeyValue key arg with
                        | Ok () -> go ParseState_DatesAndTimes.AwaitingKey args
                        | Error exc ->
                            if setFlagValue key then
                                go ParseState_DatesAndTimes.AwaitingKey (arg :: args)
                            else
                                match exc with
                                | None ->
                                    failwithf
                                        "Unable to process supplied arg %s. Help text follows.\n%s"
                                        key
                                        (helpText ())
                                | Some msg -> msg |> ArgParser_errors.Add

            go ParseState_DatesAndTimes.AwaitingKey args

            let parser_LeftoverArgs =
                if 0 = parser_LeftoverArgs.Count then
                    ()
                else
                    parser_LeftoverArgs
                    |> String.concat " "
                    |> sprintf "There were leftover args: %s"
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>

            let arg_0 =
                match arg_0 with
                | None ->
                    sprintf "Required argument '%s' received no value" (sprintf "--%s" "plain")
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | Some x -> x

            let arg_1 =
                match arg_1 with
                | None ->
                    sprintf "Required argument '%s' received no value" (sprintf "--%s" "invariant")
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | Some x -> x

            let arg_2 =
                match arg_2 with
                | None ->
                    sprintf "Required argument '%s' received no value" (sprintf "--%s" "exact")
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | Some x -> x

            let arg_3 =
                match arg_3 with
                | None ->
                    sprintf "Required argument '%s' received no value" (sprintf "--%s" "invariant-exact")
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | Some x -> x

            if 0 = ArgParser_errors.Count then
                {
                    Exact = arg_2
                    Invariant = arg_1
                    InvariantExact = arg_3
                    Plain = arg_0
                }
            else
                ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

        static member parse (args : string list) : DatesAndTimes =
            DatesAndTimes.parse' System.Environment.GetEnvironmentVariable args
namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type ParentRecord
[<AutoOpen>]
module ParentRecordArgParse =
    type private ParseState_ParentRecord =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    /// Extension methods for argument parsing
    type ParentRecord with

        static member parse' (getEnvironmentVariable : string -> string) (args : string list) : ParentRecord =
            let ArgParser_errors = ResizeArray ()

            let helpText () =
                [
                    (sprintf "%s  int32%s%s" (sprintf "--%s" "thing1") "" "")
                    (sprintf "%s  string%s%s" (sprintf "--%s" "thing2") "" "")
                    (sprintf "%s  bool%s%s" (sprintf "--%s" "and-another") "" "")
                ]
                |> String.concat "\n"

            let parser_LeftoverArgs : string ResizeArray = ResizeArray ()
            let mutable arg_0 : int option = None
            let mutable arg_1 : string option = None
            let mutable arg_2 : bool option = None

            /// Processes the key-value pair, returning Error if no key was matched.
            /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
            /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
            let processKeyValue (key : string) (value : string) : Result<unit, string option> =
                if
                    System.String.Equals (key, sprintf "--%s" "and-another", System.StringComparison.OrdinalIgnoreCase)
                then
                    match arg_2 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "and-another")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_2 <- value |> (fun x -> System.Boolean.Parse x) |> Some
                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else if
                    System.String.Equals (key, sprintf "--%s" "thing2", System.StringComparison.OrdinalIgnoreCase)
                then
                    match arg_1 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "thing2")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_1 <- value |> (fun x -> x) |> Some
                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else if
                    System.String.Equals (key, sprintf "--%s" "thing1", System.StringComparison.OrdinalIgnoreCase)
                then
                    match arg_0 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "thing1")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_0 <- value |> (fun x -> System.Int32.Parse x) |> Some
                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else
                    Error None

            /// Returns false if we didn't set a value.
            let setFlagValue (key : string) : bool =
                if
                    System.String.Equals (key, sprintf "--%s" "and-another", System.StringComparison.OrdinalIgnoreCase)
                then
                    match arg_2 with
                    | Some x ->
                        sprintf "Flag '%s' was supplied multiple times" (sprintf "--%s" "and-another")
                        |> ArgParser_errors.Add

                        true
                    | None ->
                        arg_2 <- true |> Some
                        true
                else
                    false

            let rec go (state : ParseState_ParentRecord) (args : string list) =
                match args with
                | [] ->
                    match state with
                    | ParseState_ParentRecord.AwaitingKey -> ()
                    | ParseState_ParentRecord.AwaitingValue key ->
                        if setFlagValue key then
                            ()
                        else
                            sprintf
                                "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                                key
                            |> ArgParser_errors.Add
                | "--" :: rest -> parser_LeftoverArgs.AddRange (rest |> Seq.map (fun x -> x))
                | arg :: args ->
                    match state with
                    | ParseState_ParentRecord.AwaitingKey ->
                        if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                            if arg = "--help" then
                                helpText () |> failwithf "Help text requested.\n%s"
                            else
                                let equals = arg.IndexOf (char 61)

                                if equals < 0 then
                                    args |> go (ParseState_ParentRecord.AwaitingValue arg)
                                else
                                    let key = arg.[0 .. equals - 1]
                                    let value = arg.[equals + 1 ..]

                                    match processKeyValue key value with
                                    | Ok () -> go ParseState_ParentRecord.AwaitingKey args
                                    | Error x ->
                                        match x with
                                        | None ->
                                            failwithf
                                                "Unable to process argument %s as key %s and value %s"
                                                arg
                                                key
                                                value
                                        | Some msg ->
                                            sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                            go ParseState_ParentRecord.AwaitingKey args
                        else
                            arg |> (fun x -> x) |> parser_LeftoverArgs.Add
                            go ParseState_ParentRecord.AwaitingKey args
                    | ParseState_ParentRecord.AwaitingValue key ->
                        match processKeyValue key arg with
                        | Ok () -> go ParseState_ParentRecord.AwaitingKey args
                        | Error exc ->
                            if setFlagValue key then
                                go ParseState_ParentRecord.AwaitingKey (arg :: args)
                            else
                                match exc with
                                | None ->
                                    failwithf
                                        "Unable to process supplied arg %s. Help text follows.\n%s"
                                        key
                                        (helpText ())
                                | Some msg -> msg |> ArgParser_errors.Add

            go ParseState_ParentRecord.AwaitingKey args

            let parser_LeftoverArgs =
                if 0 = parser_LeftoverArgs.Count then
                    ()
                else
                    parser_LeftoverArgs
                    |> String.concat " "
                    |> sprintf "There were leftover args: %s"
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>

            let arg_0 =
                match arg_0 with
                | None ->
                    sprintf "Required argument '%s' received no value" (sprintf "--%s" "thing1")
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | Some x -> x

            let arg_1 =
                match arg_1 with
                | None ->
                    sprintf "Required argument '%s' received no value" (sprintf "--%s" "thing2")
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | Some x -> x

            let arg_2 =
                match arg_2 with
                | None ->
                    sprintf "Required argument '%s' received no value" (sprintf "--%s" "and-another")
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | Some x -> x

            if 0 = ArgParser_errors.Count then
                {
                    AndAnother = arg_2
                    Child =
                        {
                            Thing1 = arg_0
                            Thing2 = arg_1
                        }
                }
            else
                ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

        static member parse (args : string list) : ParentRecord =
            ParentRecord.parse' System.Environment.GetEnvironmentVariable args
namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type ParentRecordChildPos
[<AutoOpen>]
module ParentRecordChildPosArgParse =
    type private ParseState_ParentRecordChildPos =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    /// Extension methods for argument parsing
    type ParentRecordChildPos with

        static member parse' (getEnvironmentVariable : string -> string) (args : string list) : ParentRecordChildPos =
            let ArgParser_errors = ResizeArray ()

            let helpText () =
                [
                    (sprintf "%s  bool%s%s" (sprintf "--%s" "and-another") "" "")
                    (sprintf "%s  int32%s%s" (sprintf "--%s" "thing1") "" "")
                    (sprintf "%s  URI%s%s" (sprintf "--%s" "thing2") " (positional args) (can be repeated)" "")
                ]
                |> String.concat "\n"

            let arg_1 : Uri ResizeArray = ResizeArray ()
            let mutable arg_2 : bool option = None
            let mutable arg_0 : int option = None

            /// Processes the key-value pair, returning Error if no key was matched.
            /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
            /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
            let processKeyValue (key : string) (value : string) : Result<unit, string option> =
                if System.String.Equals (key, sprintf "--%s" "thing1", System.StringComparison.OrdinalIgnoreCase) then
                    match arg_0 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "thing1")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_0 <- value |> (fun x -> System.Int32.Parse x) |> Some
                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else if
                    System.String.Equals (key, sprintf "--%s" "and-another", System.StringComparison.OrdinalIgnoreCase)
                then
                    match arg_2 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "and-another")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_2 <- value |> (fun x -> System.Boolean.Parse x) |> Some
                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else if
                    System.String.Equals (key, sprintf "--%s" "thing2", System.StringComparison.OrdinalIgnoreCase)
                then
                    value |> (fun x -> System.Uri x) |> arg_1.Add
                    () |> Ok
                else
                    Error None

            /// Returns false if we didn't set a value.
            let setFlagValue (key : string) : bool =
                if
                    System.String.Equals (key, sprintf "--%s" "and-another", System.StringComparison.OrdinalIgnoreCase)
                then
                    match arg_2 with
                    | Some x ->
                        sprintf "Flag '%s' was supplied multiple times" (sprintf "--%s" "and-another")
                        |> ArgParser_errors.Add

                        true
                    | None ->
                        arg_2 <- true |> Some
                        true
                else
                    false

            let rec go (state : ParseState_ParentRecordChildPos) (args : string list) =
                match args with
                | [] ->
                    match state with
                    | ParseState_ParentRecordChildPos.AwaitingKey -> ()
                    | ParseState_ParentRecordChildPos.AwaitingValue key ->
                        if setFlagValue key then
                            ()
                        else
                            sprintf
                                "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                                key
                            |> ArgParser_errors.Add
                | "--" :: rest -> arg_1.AddRange (rest |> Seq.map (fun x -> System.Uri x))
                | arg :: args ->
                    match state with
                    | ParseState_ParentRecordChildPos.AwaitingKey ->
                        if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                            if arg = "--help" then
                                helpText () |> failwithf "Help text requested.\n%s"
                            else
                                let equals = arg.IndexOf (char 61)

                                if equals < 0 then
                                    args |> go (ParseState_ParentRecordChildPos.AwaitingValue arg)
                                else
                                    let key = arg.[0 .. equals - 1]
                                    let value = arg.[equals + 1 ..]

                                    match processKeyValue key value with
                                    | Ok () -> go ParseState_ParentRecordChildPos.AwaitingKey args
                                    | Error x ->
                                        match x with
                                        | None ->
                                            failwithf
                                                "Unable to process argument %s as key %s and value %s"
                                                arg
                                                key
                                                value
                                        | Some msg ->
                                            sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                            go ParseState_ParentRecordChildPos.AwaitingKey args
                        else
                            arg |> (fun x -> System.Uri x) |> arg_1.Add
                            go ParseState_ParentRecordChildPos.AwaitingKey args
                    | ParseState_ParentRecordChildPos.AwaitingValue key ->
                        match processKeyValue key arg with
                        | Ok () -> go ParseState_ParentRecordChildPos.AwaitingKey args
                        | Error exc ->
                            if setFlagValue key then
                                go ParseState_ParentRecordChildPos.AwaitingKey (arg :: args)
                            else
                                match exc with
                                | None ->
                                    failwithf
                                        "Unable to process supplied arg %s. Help text follows.\n%s"
                                        key
                                        (helpText ())
                                | Some msg -> msg |> ArgParser_errors.Add

            go ParseState_ParentRecordChildPos.AwaitingKey args
            let arg_1 = arg_1 |> Seq.toList

            let arg_2 =
                match arg_2 with
                | None ->
                    sprintf "Required argument '%s' received no value" (sprintf "--%s" "and-another")
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | Some x -> x

            let arg_0 =
                match arg_0 with
                | None ->
                    sprintf "Required argument '%s' received no value" (sprintf "--%s" "thing1")
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | Some x -> x

            if 0 = ArgParser_errors.Count then
                {
                    AndAnother = arg_2
                    Child =
                        {
                            Thing1 = arg_0
                            Thing2 = arg_1
                        }
                }
            else
                ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

        static member parse (args : string list) : ParentRecordChildPos =
            ParentRecordChildPos.parse' System.Environment.GetEnvironmentVariable args
namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type ParentRecordSelfPos
[<AutoOpen>]
module ParentRecordSelfPosArgParse =
    type private ParseState_ParentRecordSelfPos =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    /// Extension methods for argument parsing
    type ParentRecordSelfPos with

        static member parse' (getEnvironmentVariable : string -> string) (args : string list) : ParentRecordSelfPos =
            let ArgParser_errors = ResizeArray ()

            let helpText () =
                [
                    (sprintf "%s  int32%s%s" (sprintf "--%s" "thing1") "" "")
                    (sprintf "%s  string%s%s" (sprintf "--%s" "thing2") "" "")
                    (sprintf "%s  bool%s%s" (sprintf "--%s" "and-another") " (positional args) (can be repeated)" "")
                ]
                |> String.concat "\n"

            let arg_2 : bool ResizeArray = ResizeArray ()
            let mutable arg_0 : int option = None
            let mutable arg_1 : string option = None

            /// Processes the key-value pair, returning Error if no key was matched.
            /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
            /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
            let processKeyValue (key : string) (value : string) : Result<unit, string option> =
                if System.String.Equals (key, sprintf "--%s" "thing2", System.StringComparison.OrdinalIgnoreCase) then
                    match arg_1 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "thing2")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_1 <- value |> (fun x -> x) |> Some
                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else if
                    System.String.Equals (key, sprintf "--%s" "thing1", System.StringComparison.OrdinalIgnoreCase)
                then
                    match arg_0 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "thing1")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_0 <- value |> (fun x -> System.Int32.Parse x) |> Some
                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else if
                    System.String.Equals (key, sprintf "--%s" "and-another", System.StringComparison.OrdinalIgnoreCase)
                then
                    value |> (fun x -> System.Boolean.Parse x) |> arg_2.Add
                    () |> Ok
                else
                    Error None

            /// Returns false if we didn't set a value.
            let setFlagValue (key : string) : bool = false

            let rec go (state : ParseState_ParentRecordSelfPos) (args : string list) =
                match args with
                | [] ->
                    match state with
                    | ParseState_ParentRecordSelfPos.AwaitingKey -> ()
                    | ParseState_ParentRecordSelfPos.AwaitingValue key ->
                        if setFlagValue key then
                            ()
                        else
                            sprintf
                                "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                                key
                            |> ArgParser_errors.Add
                | "--" :: rest -> arg_2.AddRange (rest |> Seq.map (fun x -> System.Boolean.Parse x))
                | arg :: args ->
                    match state with
                    | ParseState_ParentRecordSelfPos.AwaitingKey ->
                        if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                            if arg = "--help" then
                                helpText () |> failwithf "Help text requested.\n%s"
                            else
                                let equals = arg.IndexOf (char 61)

                                if equals < 0 then
                                    args |> go (ParseState_ParentRecordSelfPos.AwaitingValue arg)
                                else
                                    let key = arg.[0 .. equals - 1]
                                    let value = arg.[equals + 1 ..]

                                    match processKeyValue key value with
                                    | Ok () -> go ParseState_ParentRecordSelfPos.AwaitingKey args
                                    | Error x ->
                                        match x with
                                        | None ->
                                            failwithf
                                                "Unable to process argument %s as key %s and value %s"
                                                arg
                                                key
                                                value
                                        | Some msg ->
                                            sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                            go ParseState_ParentRecordSelfPos.AwaitingKey args
                        else
                            arg |> (fun x -> System.Boolean.Parse x) |> arg_2.Add
                            go ParseState_ParentRecordSelfPos.AwaitingKey args
                    | ParseState_ParentRecordSelfPos.AwaitingValue key ->
                        match processKeyValue key arg with
                        | Ok () -> go ParseState_ParentRecordSelfPos.AwaitingKey args
                        | Error exc ->
                            if setFlagValue key then
                                go ParseState_ParentRecordSelfPos.AwaitingKey (arg :: args)
                            else
                                match exc with
                                | None ->
                                    failwithf
                                        "Unable to process supplied arg %s. Help text follows.\n%s"
                                        key
                                        (helpText ())
                                | Some msg -> msg |> ArgParser_errors.Add

            go ParseState_ParentRecordSelfPos.AwaitingKey args
            let arg_2 = arg_2 |> Seq.toList

            let arg_0 =
                match arg_0 with
                | None ->
                    sprintf "Required argument '%s' received no value" (sprintf "--%s" "thing1")
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | Some x -> x

            let arg_1 =
                match arg_1 with
                | None ->
                    sprintf "Required argument '%s' received no value" (sprintf "--%s" "thing2")
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | Some x -> x

            if 0 = ArgParser_errors.Count then
                {
                    AndAnother = arg_2
                    Child =
                        {
                            Thing1 = arg_0
                            Thing2 = arg_1
                        }
                }
            else
                ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

        static member parse (args : string list) : ParentRecordSelfPos =
            ParentRecordSelfPos.parse' System.Environment.GetEnvironmentVariable args
namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type ChoicePositionals
[<AutoOpen>]
module ChoicePositionalsArgParse =
    type private ParseState_ChoicePositionals =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    /// Extension methods for argument parsing
    type ChoicePositionals with

        static member parse' (getEnvironmentVariable : string -> string) (args : string list) : ChoicePositionals =
            let ArgParser_errors = ResizeArray ()

            let helpText () =
                [
                    (sprintf "%s  string%s%s" (sprintf "--%s" "args") " (positional args) (can be repeated)" "")
                ]
                |> String.concat "\n"

            let arg_0 : Choice<string, string> ResizeArray = ResizeArray ()

            /// Processes the key-value pair, returning Error if no key was matched.
            /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
            /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
            let processKeyValue (key : string) (value : string) : Result<unit, string option> =
                if System.String.Equals (key, sprintf "--%s" "args", System.StringComparison.OrdinalIgnoreCase) then
                    value |> (fun x -> x) |> Choice1Of2 |> arg_0.Add
                    () |> Ok
                else
                    Error None

            /// Returns false if we didn't set a value.
            let setFlagValue (key : string) : bool = false

            let rec go (state : ParseState_ChoicePositionals) (args : string list) =
                match args with
                | [] ->
                    match state with
                    | ParseState_ChoicePositionals.AwaitingKey -> ()
                    | ParseState_ChoicePositionals.AwaitingValue key ->
                        if setFlagValue key then
                            ()
                        else
                            sprintf
                                "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                                key
                            |> ArgParser_errors.Add
                | "--" :: rest -> arg_0.AddRange (rest |> Seq.map (fun x -> x) |> Seq.map Choice2Of2)
                | arg :: args ->
                    match state with
                    | ParseState_ChoicePositionals.AwaitingKey ->
                        if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                            if arg = "--help" then
                                helpText () |> failwithf "Help text requested.\n%s"
                            else
                                let equals = arg.IndexOf (char 61)

                                if equals < 0 then
                                    args |> go (ParseState_ChoicePositionals.AwaitingValue arg)
                                else
                                    let key = arg.[0 .. equals - 1]
                                    let value = arg.[equals + 1 ..]

                                    match processKeyValue key value with
                                    | Ok () -> go ParseState_ChoicePositionals.AwaitingKey args
                                    | Error x ->
                                        match x with
                                        | None ->
                                            failwithf
                                                "Unable to process argument %s as key %s and value %s"
                                                arg
                                                key
                                                value
                                        | Some msg ->
                                            sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                            go ParseState_ChoicePositionals.AwaitingKey args
                        else
                            arg |> (fun x -> x) |> Choice1Of2 |> arg_0.Add
                            go ParseState_ChoicePositionals.AwaitingKey args
                    | ParseState_ChoicePositionals.AwaitingValue key ->
                        match processKeyValue key arg with
                        | Ok () -> go ParseState_ChoicePositionals.AwaitingKey args
                        | Error exc ->
                            if setFlagValue key then
                                go ParseState_ChoicePositionals.AwaitingKey (arg :: args)
                            else
                                match exc with
                                | None ->
                                    failwithf
                                        "Unable to process supplied arg %s. Help text follows.\n%s"
                                        key
                                        (helpText ())
                                | Some msg -> msg |> ArgParser_errors.Add

            go ParseState_ChoicePositionals.AwaitingKey args
            let arg_0 = arg_0 |> Seq.toList

            if 0 = ArgParser_errors.Count then
                {
                    Args = arg_0
                }
            else
                ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

        static member parse (args : string list) : ChoicePositionals =
            ChoicePositionals.parse' System.Environment.GetEnvironmentVariable args
namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type ContainsBoolEnvVar
[<AutoOpen>]
module ContainsBoolEnvVarArgParse =
    type private ParseState_ContainsBoolEnvVar =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    /// Extension methods for argument parsing
    type ContainsBoolEnvVar with

        static member parse' (getEnvironmentVariable : string -> string) (args : string list) : ContainsBoolEnvVar =
            let ArgParser_errors = ResizeArray ()

            let helpText () =
                [
                    (sprintf
                        "%s  bool%s%s"
                        (sprintf "--%s" "bool-var")
                        ("CONSUMEPLUGIN_THINGS" |> sprintf " (default value populated from env var %s)")
                        "")
                ]
                |> String.concat "\n"

            let parser_LeftoverArgs : string ResizeArray = ResizeArray ()
            let mutable arg_0 : bool option = None

            /// Processes the key-value pair, returning Error if no key was matched.
            /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
            /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
            let processKeyValue (key : string) (value : string) : Result<unit, string option> =
                if System.String.Equals (key, sprintf "--%s" "bool-var", System.StringComparison.OrdinalIgnoreCase) then
                    match arg_0 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "bool-var")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_0 <- value |> (fun x -> System.Boolean.Parse x) |> Some
                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else
                    Error None

            /// Returns false if we didn't set a value.
            let setFlagValue (key : string) : bool =
                if System.String.Equals (key, sprintf "--%s" "bool-var", System.StringComparison.OrdinalIgnoreCase) then
                    match arg_0 with
                    | Some x ->
                        sprintf "Flag '%s' was supplied multiple times" (sprintf "--%s" "bool-var")
                        |> ArgParser_errors.Add

                        true
                    | None ->
                        arg_0 <- true |> Some
                        true
                else
                    false

            let rec go (state : ParseState_ContainsBoolEnvVar) (args : string list) =
                match args with
                | [] ->
                    match state with
                    | ParseState_ContainsBoolEnvVar.AwaitingKey -> ()
                    | ParseState_ContainsBoolEnvVar.AwaitingValue key ->
                        if setFlagValue key then
                            ()
                        else
                            sprintf
                                "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                                key
                            |> ArgParser_errors.Add
                | "--" :: rest -> parser_LeftoverArgs.AddRange (rest |> Seq.map (fun x -> x))
                | arg :: args ->
                    match state with
                    | ParseState_ContainsBoolEnvVar.AwaitingKey ->
                        if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                            if arg = "--help" then
                                helpText () |> failwithf "Help text requested.\n%s"
                            else
                                let equals = arg.IndexOf (char 61)

                                if equals < 0 then
                                    args |> go (ParseState_ContainsBoolEnvVar.AwaitingValue arg)
                                else
                                    let key = arg.[0 .. equals - 1]
                                    let value = arg.[equals + 1 ..]

                                    match processKeyValue key value with
                                    | Ok () -> go ParseState_ContainsBoolEnvVar.AwaitingKey args
                                    | Error x ->
                                        match x with
                                        | None ->
                                            failwithf
                                                "Unable to process argument %s as key %s and value %s"
                                                arg
                                                key
                                                value
                                        | Some msg ->
                                            sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                            go ParseState_ContainsBoolEnvVar.AwaitingKey args
                        else
                            arg |> (fun x -> x) |> parser_LeftoverArgs.Add
                            go ParseState_ContainsBoolEnvVar.AwaitingKey args
                    | ParseState_ContainsBoolEnvVar.AwaitingValue key ->
                        match processKeyValue key arg with
                        | Ok () -> go ParseState_ContainsBoolEnvVar.AwaitingKey args
                        | Error exc ->
                            if setFlagValue key then
                                go ParseState_ContainsBoolEnvVar.AwaitingKey (arg :: args)
                            else
                                match exc with
                                | None ->
                                    failwithf
                                        "Unable to process supplied arg %s. Help text follows.\n%s"
                                        key
                                        (helpText ())
                                | Some msg -> msg |> ArgParser_errors.Add

            go ParseState_ContainsBoolEnvVar.AwaitingKey args

            let parser_LeftoverArgs =
                if 0 = parser_LeftoverArgs.Count then
                    ()
                else
                    parser_LeftoverArgs
                    |> String.concat " "
                    |> sprintf "There were leftover args: %s"
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>

            let arg_0 =
                match arg_0 with
                | None ->
                    match "CONSUMEPLUGIN_THINGS" |> getEnvironmentVariable with
                    | null ->
                        sprintf
                            "No value was supplied for %s, nor was environment variable %s set"
                            (sprintf "--%s" "bool-var")
                            "CONSUMEPLUGIN_THINGS"
                        |> ArgParser_errors.Add

                        Unchecked.defaultof<_>
                    | x ->
                        if System.String.Equals (x, "1", System.StringComparison.OrdinalIgnoreCase) then
                            true
                        else if System.String.Equals (x, "0", System.StringComparison.OrdinalIgnoreCase) then
                            false
                        else
                            x |> (fun x -> System.Boolean.Parse x)
                    |> Choice2Of2
                | Some x -> Choice1Of2 x

            if 0 = ArgParser_errors.Count then
                {
                    BoolVar = arg_0
                }
            else
                ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

        static member parse (args : string list) : ContainsBoolEnvVar =
            ContainsBoolEnvVar.parse' System.Environment.GetEnvironmentVariable args
namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type WithFlagDu
[<AutoOpen>]
module WithFlagDuArgParse =
    type private ParseState_WithFlagDu =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    /// Extension methods for argument parsing
    type WithFlagDu with

        static member parse' (getEnvironmentVariable : string -> string) (args : string list) : WithFlagDu =
            let ArgParser_errors = ResizeArray ()

            let helpText () =
                [ (sprintf "%s  bool%s%s" (sprintf "--%s" "dry-run") "" "") ]
                |> String.concat "\n"

            let parser_LeftoverArgs : string ResizeArray = ResizeArray ()
            let mutable arg_0 : DryRunMode option = None

            /// Processes the key-value pair, returning Error if no key was matched.
            /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
            /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
            let processKeyValue (key : string) (value : string) : Result<unit, string option> =
                if System.String.Equals (key, sprintf "--%s" "dry-run", System.StringComparison.OrdinalIgnoreCase) then
                    match arg_0 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "dry-run")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_0 <-
                                value
                                |> (fun x ->
                                    if System.Boolean.Parse x = Consts.FALSE then
                                        DryRunMode.Wet
                                    else
                                        DryRunMode.Dry
                                )
                                |> Some

                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else
                    Error None

            /// Returns false if we didn't set a value.
            let setFlagValue (key : string) : bool =
                if System.String.Equals (key, sprintf "--%s" "dry-run", System.StringComparison.OrdinalIgnoreCase) then
                    match arg_0 with
                    | Some x ->
                        sprintf "Flag '%s' was supplied multiple times" (sprintf "--%s" "dry-run")
                        |> ArgParser_errors.Add

                        true
                    | None ->
                        arg_0 <-
                            if true = Consts.FALSE then
                                DryRunMode.Wet
                            else
                                DryRunMode.Dry
                            |> Some

                        true
                else
                    false

            let rec go (state : ParseState_WithFlagDu) (args : string list) =
                match args with
                | [] ->
                    match state with
                    | ParseState_WithFlagDu.AwaitingKey -> ()
                    | ParseState_WithFlagDu.AwaitingValue key ->
                        if setFlagValue key then
                            ()
                        else
                            sprintf
                                "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                                key
                            |> ArgParser_errors.Add
                | "--" :: rest -> parser_LeftoverArgs.AddRange (rest |> Seq.map (fun x -> x))
                | arg :: args ->
                    match state with
                    | ParseState_WithFlagDu.AwaitingKey ->
                        if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                            if arg = "--help" then
                                helpText () |> failwithf "Help text requested.\n%s"
                            else
                                let equals = arg.IndexOf (char 61)

                                if equals < 0 then
                                    args |> go (ParseState_WithFlagDu.AwaitingValue arg)
                                else
                                    let key = arg.[0 .. equals - 1]
                                    let value = arg.[equals + 1 ..]

                                    match processKeyValue key value with
                                    | Ok () -> go ParseState_WithFlagDu.AwaitingKey args
                                    | Error x ->
                                        match x with
                                        | None ->
                                            failwithf
                                                "Unable to process argument %s as key %s and value %s"
                                                arg
                                                key
                                                value
                                        | Some msg ->
                                            sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                            go ParseState_WithFlagDu.AwaitingKey args
                        else
                            arg |> (fun x -> x) |> parser_LeftoverArgs.Add
                            go ParseState_WithFlagDu.AwaitingKey args
                    | ParseState_WithFlagDu.AwaitingValue key ->
                        match processKeyValue key arg with
                        | Ok () -> go ParseState_WithFlagDu.AwaitingKey args
                        | Error exc ->
                            if setFlagValue key then
                                go ParseState_WithFlagDu.AwaitingKey (arg :: args)
                            else
                                match exc with
                                | None ->
                                    failwithf
                                        "Unable to process supplied arg %s. Help text follows.\n%s"
                                        key
                                        (helpText ())
                                | Some msg -> msg |> ArgParser_errors.Add

            go ParseState_WithFlagDu.AwaitingKey args

            let parser_LeftoverArgs =
                if 0 = parser_LeftoverArgs.Count then
                    ()
                else
                    parser_LeftoverArgs
                    |> String.concat " "
                    |> sprintf "There were leftover args: %s"
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>

            let arg_0 =
                match arg_0 with
                | None ->
                    sprintf "Required argument '%s' received no value" (sprintf "--%s" "dry-run")
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | Some x -> x

            if 0 = ArgParser_errors.Count then
                {
                    DryRun = arg_0
                }
            else
                ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

        static member parse (args : string list) : WithFlagDu =
            WithFlagDu.parse' System.Environment.GetEnvironmentVariable args
namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type ContainsFlagEnvVar
[<AutoOpen>]
module ContainsFlagEnvVarArgParse =
    type private ParseState_ContainsFlagEnvVar =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    /// Extension methods for argument parsing
    type ContainsFlagEnvVar with

        static member parse' (getEnvironmentVariable : string -> string) (args : string list) : ContainsFlagEnvVar =
            let ArgParser_errors = ResizeArray ()

            let helpText () =
                [
                    (sprintf
                        "%s  bool%s%s"
                        (sprintf "--%s" "dry-run")
                        ("CONSUMEPLUGIN_THINGS" |> sprintf " (default value populated from env var %s)")
                        "")
                ]
                |> String.concat "\n"

            let parser_LeftoverArgs : string ResizeArray = ResizeArray ()
            let mutable arg_0 : DryRunMode option = None

            /// Processes the key-value pair, returning Error if no key was matched.
            /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
            /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
            let processKeyValue (key : string) (value : string) : Result<unit, string option> =
                if System.String.Equals (key, sprintf "--%s" "dry-run", System.StringComparison.OrdinalIgnoreCase) then
                    match arg_0 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "dry-run")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_0 <-
                                value
                                |> (fun x ->
                                    if System.Boolean.Parse x = Consts.FALSE then
                                        DryRunMode.Wet
                                    else
                                        DryRunMode.Dry
                                )
                                |> Some

                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else
                    Error None

            /// Returns false if we didn't set a value.
            let setFlagValue (key : string) : bool =
                if System.String.Equals (key, sprintf "--%s" "dry-run", System.StringComparison.OrdinalIgnoreCase) then
                    match arg_0 with
                    | Some x ->
                        sprintf "Flag '%s' was supplied multiple times" (sprintf "--%s" "dry-run")
                        |> ArgParser_errors.Add

                        true
                    | None ->
                        arg_0 <-
                            if true = Consts.FALSE then
                                DryRunMode.Wet
                            else
                                DryRunMode.Dry
                            |> Some

                        true
                else
                    false

            let rec go (state : ParseState_ContainsFlagEnvVar) (args : string list) =
                match args with
                | [] ->
                    match state with
                    | ParseState_ContainsFlagEnvVar.AwaitingKey -> ()
                    | ParseState_ContainsFlagEnvVar.AwaitingValue key ->
                        if setFlagValue key then
                            ()
                        else
                            sprintf
                                "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                                key
                            |> ArgParser_errors.Add
                | "--" :: rest -> parser_LeftoverArgs.AddRange (rest |> Seq.map (fun x -> x))
                | arg :: args ->
                    match state with
                    | ParseState_ContainsFlagEnvVar.AwaitingKey ->
                        if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                            if arg = "--help" then
                                helpText () |> failwithf "Help text requested.\n%s"
                            else
                                let equals = arg.IndexOf (char 61)

                                if equals < 0 then
                                    args |> go (ParseState_ContainsFlagEnvVar.AwaitingValue arg)
                                else
                                    let key = arg.[0 .. equals - 1]
                                    let value = arg.[equals + 1 ..]

                                    match processKeyValue key value with
                                    | Ok () -> go ParseState_ContainsFlagEnvVar.AwaitingKey args
                                    | Error x ->
                                        match x with
                                        | None ->
                                            failwithf
                                                "Unable to process argument %s as key %s and value %s"
                                                arg
                                                key
                                                value
                                        | Some msg ->
                                            sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                            go ParseState_ContainsFlagEnvVar.AwaitingKey args
                        else
                            arg |> (fun x -> x) |> parser_LeftoverArgs.Add
                            go ParseState_ContainsFlagEnvVar.AwaitingKey args
                    | ParseState_ContainsFlagEnvVar.AwaitingValue key ->
                        match processKeyValue key arg with
                        | Ok () -> go ParseState_ContainsFlagEnvVar.AwaitingKey args
                        | Error exc ->
                            if setFlagValue key then
                                go ParseState_ContainsFlagEnvVar.AwaitingKey (arg :: args)
                            else
                                match exc with
                                | None ->
                                    failwithf
                                        "Unable to process supplied arg %s. Help text follows.\n%s"
                                        key
                                        (helpText ())
                                | Some msg -> msg |> ArgParser_errors.Add

            go ParseState_ContainsFlagEnvVar.AwaitingKey args

            let parser_LeftoverArgs =
                if 0 = parser_LeftoverArgs.Count then
                    ()
                else
                    parser_LeftoverArgs
                    |> String.concat " "
                    |> sprintf "There were leftover args: %s"
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>

            let arg_0 =
                match arg_0 with
                | None ->
                    match "CONSUMEPLUGIN_THINGS" |> getEnvironmentVariable with
                    | null ->
                        sprintf
                            "No value was supplied for %s, nor was environment variable %s set"
                            (sprintf "--%s" "dry-run")
                            "CONSUMEPLUGIN_THINGS"
                        |> ArgParser_errors.Add

                        Unchecked.defaultof<_>
                    | x ->
                        if System.String.Equals (x, "1", System.StringComparison.OrdinalIgnoreCase) then
                            if true = Consts.FALSE then
                                DryRunMode.Wet
                            else
                                DryRunMode.Dry
                        else if System.String.Equals (x, "0", System.StringComparison.OrdinalIgnoreCase) then
                            if false = Consts.FALSE then
                                DryRunMode.Wet
                            else
                                DryRunMode.Dry
                        else
                            x
                            |> (fun x ->
                                if System.Boolean.Parse x = Consts.FALSE then
                                    DryRunMode.Wet
                                else
                                    DryRunMode.Dry
                            )
                    |> Choice2Of2
                | Some x -> Choice1Of2 x

            if 0 = ArgParser_errors.Count then
                {
                    DryRun = arg_0
                }
            else
                ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

        static member parse (args : string list) : ContainsFlagEnvVar =
            ContainsFlagEnvVar.parse' System.Environment.GetEnvironmentVariable args
namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type ContainsFlagDefaultValue
[<AutoOpen>]
module ContainsFlagDefaultValueArgParse =
    type private ParseState_ContainsFlagDefaultValue =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    /// Extension methods for argument parsing
    type ContainsFlagDefaultValue with

        static member parse'
            (getEnvironmentVariable : string -> string)
            (args : string list)
            : ContainsFlagDefaultValue
            =
            let ArgParser_errors = ResizeArray ()

            let helpText () =
                [
                    (sprintf
                        "%s  bool%s%s"
                        (sprintf "--%s" "dry-run")
                        (match ContainsFlagDefaultValue.DefaultDryRun () with
                         | DryRunMode.Wet -> if Consts.FALSE = true then "true" else "false"
                         | DryRunMode.Dry -> if true = true then "true" else "false"
                         |> (fun x -> x.ToString ())
                         |> sprintf " (default value: %s)")
                        "")
                ]
                |> String.concat "\n"

            let parser_LeftoverArgs : string ResizeArray = ResizeArray ()
            let mutable arg_0 : DryRunMode option = None

            /// Processes the key-value pair, returning Error if no key was matched.
            /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
            /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
            let processKeyValue (key : string) (value : string) : Result<unit, string option> =
                if System.String.Equals (key, sprintf "--%s" "dry-run", System.StringComparison.OrdinalIgnoreCase) then
                    match arg_0 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "dry-run")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_0 <-
                                value
                                |> (fun x ->
                                    if System.Boolean.Parse x = Consts.FALSE then
                                        DryRunMode.Wet
                                    else
                                        DryRunMode.Dry
                                )
                                |> Some

                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else
                    Error None

            /// Returns false if we didn't set a value.
            let setFlagValue (key : string) : bool =
                if System.String.Equals (key, sprintf "--%s" "dry-run", System.StringComparison.OrdinalIgnoreCase) then
                    match arg_0 with
                    | Some x ->
                        sprintf "Flag '%s' was supplied multiple times" (sprintf "--%s" "dry-run")
                        |> ArgParser_errors.Add

                        true
                    | None ->
                        arg_0 <-
                            if true = Consts.FALSE then
                                DryRunMode.Wet
                            else
                                DryRunMode.Dry
                            |> Some

                        true
                else
                    false

            let rec go (state : ParseState_ContainsFlagDefaultValue) (args : string list) =
                match args with
                | [] ->
                    match state with
                    | ParseState_ContainsFlagDefaultValue.AwaitingKey -> ()
                    | ParseState_ContainsFlagDefaultValue.AwaitingValue key ->
                        if setFlagValue key then
                            ()
                        else
                            sprintf
                                "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                                key
                            |> ArgParser_errors.Add
                | "--" :: rest -> parser_LeftoverArgs.AddRange (rest |> Seq.map (fun x -> x))
                | arg :: args ->
                    match state with
                    | ParseState_ContainsFlagDefaultValue.AwaitingKey ->
                        if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                            if arg = "--help" then
                                helpText () |> failwithf "Help text requested.\n%s"
                            else
                                let equals = arg.IndexOf (char 61)

                                if equals < 0 then
                                    args |> go (ParseState_ContainsFlagDefaultValue.AwaitingValue arg)
                                else
                                    let key = arg.[0 .. equals - 1]
                                    let value = arg.[equals + 1 ..]

                                    match processKeyValue key value with
                                    | Ok () -> go ParseState_ContainsFlagDefaultValue.AwaitingKey args
                                    | Error x ->
                                        match x with
                                        | None ->
                                            failwithf
                                                "Unable to process argument %s as key %s and value %s"
                                                arg
                                                key
                                                value
                                        | Some msg ->
                                            sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                            go ParseState_ContainsFlagDefaultValue.AwaitingKey args
                        else
                            arg |> (fun x -> x) |> parser_LeftoverArgs.Add
                            go ParseState_ContainsFlagDefaultValue.AwaitingKey args
                    | ParseState_ContainsFlagDefaultValue.AwaitingValue key ->
                        match processKeyValue key arg with
                        | Ok () -> go ParseState_ContainsFlagDefaultValue.AwaitingKey args
                        | Error exc ->
                            if setFlagValue key then
                                go ParseState_ContainsFlagDefaultValue.AwaitingKey (arg :: args)
                            else
                                match exc with
                                | None ->
                                    failwithf
                                        "Unable to process supplied arg %s. Help text follows.\n%s"
                                        key
                                        (helpText ())
                                | Some msg -> msg |> ArgParser_errors.Add

            go ParseState_ContainsFlagDefaultValue.AwaitingKey args

            let parser_LeftoverArgs =
                if 0 = parser_LeftoverArgs.Count then
                    ()
                else
                    parser_LeftoverArgs
                    |> String.concat " "
                    |> sprintf "There were leftover args: %s"
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>

            let arg_0 =
                match arg_0 with
                | None -> ContainsFlagDefaultValue.DefaultDryRun () |> Choice2Of2
                | Some x -> Choice1Of2 x

            if 0 = ArgParser_errors.Count then
                {
                    DryRun = arg_0
                }
            else
                ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

        static member parse (args : string list) : ContainsFlagDefaultValue =
            ContainsFlagDefaultValue.parse' System.Environment.GetEnvironmentVariable args
namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type ManyLongForms
[<AutoOpen>]
module ManyLongFormsArgParse =
    type private ParseState_ManyLongForms =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    /// Extension methods for argument parsing
    type ManyLongForms with

        static member parse' (getEnvironmentVariable : string -> string) (args : string list) : ManyLongForms =
            let ArgParser_errors = ResizeArray ()

            let helpText () =
                [
                    (sprintf "%s  string%s%s" (sprintf "--%s / --%s" "do-something-else" "anotherarg") "" "")
                    (sprintf "%s  bool%s%s" (sprintf "--%s / --%s" "turn-it-on" "dont-turn-it-off") "" "")
                ]
                |> String.concat "\n"

            let parser_LeftoverArgs : string ResizeArray = ResizeArray ()
            let mutable arg_0 : string option = None
            let mutable arg_1 : bool option = None

            /// Processes the key-value pair, returning Error if no key was matched.
            /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
            /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
            let processKeyValue (key : string) (value : string) : Result<unit, string option> =
                if
                    System.String.Equals (
                        key,
                        sprintf "--%s" "dont-turn-it-off",
                        System.StringComparison.OrdinalIgnoreCase
                    )
                then
                    match arg_1 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s / --%s" "turn-it-on" "dont-turn-it-off")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_1 <- value |> (fun x -> System.Boolean.Parse x) |> Some
                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else if
                    System.String.Equals (key, sprintf "--%s" "turn-it-on", System.StringComparison.OrdinalIgnoreCase)
                then
                    match arg_1 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s / --%s" "turn-it-on" "dont-turn-it-off")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_1 <- value |> (fun x -> System.Boolean.Parse x) |> Some
                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else if
                    System.String.Equals (key, sprintf "--%s" "anotherarg", System.StringComparison.OrdinalIgnoreCase)
                then
                    match arg_0 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s / --%s" "do-something-else" "anotherarg")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_0 <- value |> (fun x -> x) |> Some
                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else if
                    System.String.Equals (
                        key,
                        sprintf "--%s" "do-something-else",
                        System.StringComparison.OrdinalIgnoreCase
                    )
                then
                    match arg_0 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s / --%s" "do-something-else" "anotherarg")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_0 <- value |> (fun x -> x) |> Some
                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else
                    Error None

            /// Returns false if we didn't set a value.
            let setFlagValue (key : string) : bool =
                if
                    System.String.Equals (
                        key,
                        sprintf "--%s" "dont-turn-it-off",
                        System.StringComparison.OrdinalIgnoreCase
                    )
                then
                    match arg_1 with
                    | Some x ->
                        sprintf
                            "Flag '%s' was supplied multiple times"
                            (sprintf "--%s / --%s" "turn-it-on" "dont-turn-it-off")
                        |> ArgParser_errors.Add

                        true
                    | None ->
                        arg_1 <- true |> Some
                        true
                else if
                    System.String.Equals (key, sprintf "--%s" "turn-it-on", System.StringComparison.OrdinalIgnoreCase)
                then
                    match arg_1 with
                    | Some x ->
                        sprintf
                            "Flag '%s' was supplied multiple times"
                            (sprintf "--%s / --%s" "turn-it-on" "dont-turn-it-off")
                        |> ArgParser_errors.Add

                        true
                    | None ->
                        arg_1 <- true |> Some
                        true
                else
                    false

            let rec go (state : ParseState_ManyLongForms) (args : string list) =
                match args with
                | [] ->
                    match state with
                    | ParseState_ManyLongForms.AwaitingKey -> ()
                    | ParseState_ManyLongForms.AwaitingValue key ->
                        if setFlagValue key then
                            ()
                        else
                            sprintf
                                "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                                key
                            |> ArgParser_errors.Add
                | "--" :: rest -> parser_LeftoverArgs.AddRange (rest |> Seq.map (fun x -> x))
                | arg :: args ->
                    match state with
                    | ParseState_ManyLongForms.AwaitingKey ->
                        if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                            if arg = "--help" then
                                helpText () |> failwithf "Help text requested.\n%s"
                            else
                                let equals = arg.IndexOf (char 61)

                                if equals < 0 then
                                    args |> go (ParseState_ManyLongForms.AwaitingValue arg)
                                else
                                    let key = arg.[0 .. equals - 1]
                                    let value = arg.[equals + 1 ..]

                                    match processKeyValue key value with
                                    | Ok () -> go ParseState_ManyLongForms.AwaitingKey args
                                    | Error x ->
                                        match x with
                                        | None ->
                                            failwithf
                                                "Unable to process argument %s as key %s and value %s"
                                                arg
                                                key
                                                value
                                        | Some msg ->
                                            sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                            go ParseState_ManyLongForms.AwaitingKey args
                        else
                            arg |> (fun x -> x) |> parser_LeftoverArgs.Add
                            go ParseState_ManyLongForms.AwaitingKey args
                    | ParseState_ManyLongForms.AwaitingValue key ->
                        match processKeyValue key arg with
                        | Ok () -> go ParseState_ManyLongForms.AwaitingKey args
                        | Error exc ->
                            if setFlagValue key then
                                go ParseState_ManyLongForms.AwaitingKey (arg :: args)
                            else
                                match exc with
                                | None ->
                                    failwithf
                                        "Unable to process supplied arg %s. Help text follows.\n%s"
                                        key
                                        (helpText ())
                                | Some msg -> msg |> ArgParser_errors.Add

            go ParseState_ManyLongForms.AwaitingKey args

            let parser_LeftoverArgs =
                if 0 = parser_LeftoverArgs.Count then
                    ()
                else
                    parser_LeftoverArgs
                    |> String.concat " "
                    |> sprintf "There were leftover args: %s"
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>

            let arg_0 =
                match arg_0 with
                | None ->
                    sprintf
                        "Required argument '%s' received no value"
                        (sprintf "--%s / --%s" "do-something-else" "anotherarg")
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | Some x -> x

            let arg_1 =
                match arg_1 with
                | None ->
                    sprintf
                        "Required argument '%s' received no value"
                        (sprintf "--%s / --%s" "turn-it-on" "dont-turn-it-off")
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | Some x -> x

            if 0 = ArgParser_errors.Count then
                {
                    DoTheThing = arg_0
                    SomeFlag = arg_1
                }
            else
                ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

        static member parse (args : string list) : ManyLongForms =
            ManyLongForms.parse' System.Environment.GetEnvironmentVariable args
namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type FlagsIntoPositionalArgs
[<AutoOpen>]
module FlagsIntoPositionalArgsArgParse =
    type private ParseState_FlagsIntoPositionalArgs =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    /// Extension methods for argument parsing
    type FlagsIntoPositionalArgs with

        static member parse'
            (getEnvironmentVariable : string -> string)
            (args : string list)
            : FlagsIntoPositionalArgs
            =
            let ArgParser_errors = ResizeArray ()

            let helpText () =
                [
                    (sprintf "%s  string%s%s" (sprintf "--%s" "a") "" "")
                    (sprintf
                        "%s  string%s%s"
                        (sprintf "--%s" "grab-everything")
                        " (positional args) (can be repeated)"
                        "")
                ]
                |> String.concat "\n"

            let arg_1 : string ResizeArray = ResizeArray ()
            let mutable arg_0 : string option = None

            /// Processes the key-value pair, returning Error if no key was matched.
            /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
            /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
            let processKeyValue (key : string) (value : string) : Result<unit, string option> =
                if System.String.Equals (key, sprintf "--%s" "a", System.StringComparison.OrdinalIgnoreCase) then
                    match arg_0 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "a")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_0 <- value |> (fun x -> x) |> Some
                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else if
                    System.String.Equals (
                        key,
                        sprintf "--%s" "grab-everything",
                        System.StringComparison.OrdinalIgnoreCase
                    )
                then
                    value |> (fun x -> x) |> arg_1.Add
                    () |> Ok
                else
                    Error None

            /// Returns false if we didn't set a value.
            let setFlagValue (key : string) : bool = false

            let rec go (state : ParseState_FlagsIntoPositionalArgs) (args : string list) =
                match args with
                | [] ->
                    match state with
                    | ParseState_FlagsIntoPositionalArgs.AwaitingKey -> ()
                    | ParseState_FlagsIntoPositionalArgs.AwaitingValue key ->
                        if setFlagValue key then
                            ()
                        else
                            sprintf
                                "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                                key
                            |> ArgParser_errors.Add
                | "--" :: rest -> arg_1.AddRange (rest |> Seq.map (fun x -> x))
                | arg :: args ->
                    match state with
                    | ParseState_FlagsIntoPositionalArgs.AwaitingKey ->
                        if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                            if arg = "--help" then
                                helpText () |> failwithf "Help text requested.\n%s"
                            else
                                let equals = arg.IndexOf (char 61)

                                if equals < 0 then
                                    args |> go (ParseState_FlagsIntoPositionalArgs.AwaitingValue arg)
                                else
                                    let key = arg.[0 .. equals - 1]
                                    let value = arg.[equals + 1 ..]

                                    match processKeyValue key value with
                                    | Ok () -> go ParseState_FlagsIntoPositionalArgs.AwaitingKey args
                                    | Error x ->
                                        if true then
                                            arg |> (fun x -> x) |> arg_1.Add
                                            go ParseState_FlagsIntoPositionalArgs.AwaitingKey args
                                        else
                                            match x with
                                            | None ->
                                                failwithf
                                                    "Unable to process argument %s as key %s and value %s"
                                                    arg
                                                    key
                                                    value
                                            | Some msg ->
                                                sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                                go ParseState_FlagsIntoPositionalArgs.AwaitingKey args
                        else
                            arg |> (fun x -> x) |> arg_1.Add
                            go ParseState_FlagsIntoPositionalArgs.AwaitingKey args
                    | ParseState_FlagsIntoPositionalArgs.AwaitingValue key ->
                        match processKeyValue key arg with
                        | Ok () -> go ParseState_FlagsIntoPositionalArgs.AwaitingKey args
                        | Error exc ->
                            if setFlagValue key then
                                go ParseState_FlagsIntoPositionalArgs.AwaitingKey (arg :: args)
                            else if true then
                                key |> (fun x -> x) |> arg_1.Add
                                go ParseState_FlagsIntoPositionalArgs.AwaitingKey (arg :: args)
                            else
                                match exc with
                                | None ->
                                    failwithf
                                        "Unable to process supplied arg %s. Help text follows.\n%s"
                                        key
                                        (helpText ())
                                | Some msg -> msg |> ArgParser_errors.Add

            go ParseState_FlagsIntoPositionalArgs.AwaitingKey args
            let arg_1 = arg_1 |> Seq.toList

            let arg_0 =
                match arg_0 with
                | None ->
                    sprintf "Required argument '%s' received no value" (sprintf "--%s" "a")
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | Some x -> x

            if 0 = ArgParser_errors.Count then
                {
                    A = arg_0
                    GrabEverything = arg_1
                }
            else
                ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

        static member parse (args : string list) : FlagsIntoPositionalArgs =
            FlagsIntoPositionalArgs.parse' System.Environment.GetEnvironmentVariable args
namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type FlagsIntoPositionalArgsChoice
[<AutoOpen>]
module FlagsIntoPositionalArgsChoiceArgParse =
    type private ParseState_FlagsIntoPositionalArgsChoice =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    /// Extension methods for argument parsing
    type FlagsIntoPositionalArgsChoice with

        static member parse'
            (getEnvironmentVariable : string -> string)
            (args : string list)
            : FlagsIntoPositionalArgsChoice
            =
            let ArgParser_errors = ResizeArray ()

            let helpText () =
                [
                    (sprintf "%s  string%s%s" (sprintf "--%s" "a") "" "")
                    (sprintf
                        "%s  string%s%s"
                        (sprintf "--%s" "grab-everything")
                        " (positional args) (can be repeated)"
                        "")
                ]
                |> String.concat "\n"

            let arg_1 : Choice<string, string> ResizeArray = ResizeArray ()
            let mutable arg_0 : string option = None

            /// Processes the key-value pair, returning Error if no key was matched.
            /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
            /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
            let processKeyValue (key : string) (value : string) : Result<unit, string option> =
                if System.String.Equals (key, sprintf "--%s" "a", System.StringComparison.OrdinalIgnoreCase) then
                    match arg_0 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "a")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_0 <- value |> (fun x -> x) |> Some
                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else if
                    System.String.Equals (
                        key,
                        sprintf "--%s" "grab-everything",
                        System.StringComparison.OrdinalIgnoreCase
                    )
                then
                    value |> (fun x -> x) |> Choice1Of2 |> arg_1.Add
                    () |> Ok
                else
                    Error None

            /// Returns false if we didn't set a value.
            let setFlagValue (key : string) : bool = false

            let rec go (state : ParseState_FlagsIntoPositionalArgsChoice) (args : string list) =
                match args with
                | [] ->
                    match state with
                    | ParseState_FlagsIntoPositionalArgsChoice.AwaitingKey -> ()
                    | ParseState_FlagsIntoPositionalArgsChoice.AwaitingValue key ->
                        if setFlagValue key then
                            ()
                        else
                            sprintf
                                "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                                key
                            |> ArgParser_errors.Add
                | "--" :: rest -> arg_1.AddRange (rest |> Seq.map (fun x -> x) |> Seq.map Choice2Of2)
                | arg :: args ->
                    match state with
                    | ParseState_FlagsIntoPositionalArgsChoice.AwaitingKey ->
                        if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                            if arg = "--help" then
                                helpText () |> failwithf "Help text requested.\n%s"
                            else
                                let equals = arg.IndexOf (char 61)

                                if equals < 0 then
                                    args |> go (ParseState_FlagsIntoPositionalArgsChoice.AwaitingValue arg)
                                else
                                    let key = arg.[0 .. equals - 1]
                                    let value = arg.[equals + 1 ..]

                                    match processKeyValue key value with
                                    | Ok () -> go ParseState_FlagsIntoPositionalArgsChoice.AwaitingKey args
                                    | Error x ->
                                        if true then
                                            arg |> (fun x -> x) |> Choice1Of2 |> arg_1.Add
                                            go ParseState_FlagsIntoPositionalArgsChoice.AwaitingKey args
                                        else
                                            match x with
                                            | None ->
                                                failwithf
                                                    "Unable to process argument %s as key %s and value %s"
                                                    arg
                                                    key
                                                    value
                                            | Some msg ->
                                                sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                                go ParseState_FlagsIntoPositionalArgsChoice.AwaitingKey args
                        else
                            arg |> (fun x -> x) |> Choice1Of2 |> arg_1.Add
                            go ParseState_FlagsIntoPositionalArgsChoice.AwaitingKey args
                    | ParseState_FlagsIntoPositionalArgsChoice.AwaitingValue key ->
                        match processKeyValue key arg with
                        | Ok () -> go ParseState_FlagsIntoPositionalArgsChoice.AwaitingKey args
                        | Error exc ->
                            if setFlagValue key then
                                go ParseState_FlagsIntoPositionalArgsChoice.AwaitingKey (arg :: args)
                            else if true then
                                key |> (fun x -> x) |> Choice1Of2 |> arg_1.Add
                                go ParseState_FlagsIntoPositionalArgsChoice.AwaitingKey (arg :: args)
                            else
                                match exc with
                                | None ->
                                    failwithf
                                        "Unable to process supplied arg %s. Help text follows.\n%s"
                                        key
                                        (helpText ())
                                | Some msg -> msg |> ArgParser_errors.Add

            go ParseState_FlagsIntoPositionalArgsChoice.AwaitingKey args
            let arg_1 = arg_1 |> Seq.toList

            let arg_0 =
                match arg_0 with
                | None ->
                    sprintf "Required argument '%s' received no value" (sprintf "--%s" "a")
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | Some x -> x

            if 0 = ArgParser_errors.Count then
                {
                    A = arg_0
                    GrabEverything = arg_1
                }
            else
                ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

        static member parse (args : string list) : FlagsIntoPositionalArgsChoice =
            FlagsIntoPositionalArgsChoice.parse' System.Environment.GetEnvironmentVariable args
namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type FlagsIntoPositionalArgsInt
[<AutoOpen>]
module FlagsIntoPositionalArgsIntArgParse =
    type private ParseState_FlagsIntoPositionalArgsInt =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    /// Extension methods for argument parsing
    type FlagsIntoPositionalArgsInt with

        static member parse'
            (getEnvironmentVariable : string -> string)
            (args : string list)
            : FlagsIntoPositionalArgsInt
            =
            let ArgParser_errors = ResizeArray ()

            let helpText () =
                [
                    (sprintf "%s  string%s%s" (sprintf "--%s" "a") "" "")
                    (sprintf
                        "%s  int32%s%s"
                        (sprintf "--%s" "grab-everything")
                        " (positional args) (can be repeated)"
                        "")
                ]
                |> String.concat "\n"

            let arg_1 : int ResizeArray = ResizeArray ()
            let mutable arg_0 : string option = None

            /// Processes the key-value pair, returning Error if no key was matched.
            /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
            /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
            let processKeyValue (key : string) (value : string) : Result<unit, string option> =
                if System.String.Equals (key, sprintf "--%s" "a", System.StringComparison.OrdinalIgnoreCase) then
                    match arg_0 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "a")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_0 <- value |> (fun x -> x) |> Some
                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else if
                    System.String.Equals (
                        key,
                        sprintf "--%s" "grab-everything",
                        System.StringComparison.OrdinalIgnoreCase
                    )
                then
                    value |> (fun x -> System.Int32.Parse x) |> arg_1.Add
                    () |> Ok
                else
                    Error None

            /// Returns false if we didn't set a value.
            let setFlagValue (key : string) : bool = false

            let rec go (state : ParseState_FlagsIntoPositionalArgsInt) (args : string list) =
                match args with
                | [] ->
                    match state with
                    | ParseState_FlagsIntoPositionalArgsInt.AwaitingKey -> ()
                    | ParseState_FlagsIntoPositionalArgsInt.AwaitingValue key ->
                        if setFlagValue key then
                            ()
                        else
                            sprintf
                                "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                                key
                            |> ArgParser_errors.Add
                | "--" :: rest -> arg_1.AddRange (rest |> Seq.map (fun x -> System.Int32.Parse x))
                | arg :: args ->
                    match state with
                    | ParseState_FlagsIntoPositionalArgsInt.AwaitingKey ->
                        if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                            if arg = "--help" then
                                helpText () |> failwithf "Help text requested.\n%s"
                            else
                                let equals = arg.IndexOf (char 61)

                                if equals < 0 then
                                    args |> go (ParseState_FlagsIntoPositionalArgsInt.AwaitingValue arg)
                                else
                                    let key = arg.[0 .. equals - 1]
                                    let value = arg.[equals + 1 ..]

                                    match processKeyValue key value with
                                    | Ok () -> go ParseState_FlagsIntoPositionalArgsInt.AwaitingKey args
                                    | Error x ->
                                        if true then
                                            arg |> (fun x -> System.Int32.Parse x) |> arg_1.Add
                                            go ParseState_FlagsIntoPositionalArgsInt.AwaitingKey args
                                        else
                                            match x with
                                            | None ->
                                                failwithf
                                                    "Unable to process argument %s as key %s and value %s"
                                                    arg
                                                    key
                                                    value
                                            | Some msg ->
                                                sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                                go ParseState_FlagsIntoPositionalArgsInt.AwaitingKey args
                        else
                            arg |> (fun x -> System.Int32.Parse x) |> arg_1.Add
                            go ParseState_FlagsIntoPositionalArgsInt.AwaitingKey args
                    | ParseState_FlagsIntoPositionalArgsInt.AwaitingValue key ->
                        match processKeyValue key arg with
                        | Ok () -> go ParseState_FlagsIntoPositionalArgsInt.AwaitingKey args
                        | Error exc ->
                            if setFlagValue key then
                                go ParseState_FlagsIntoPositionalArgsInt.AwaitingKey (arg :: args)
                            else if true then
                                key |> (fun x -> System.Int32.Parse x) |> arg_1.Add
                                go ParseState_FlagsIntoPositionalArgsInt.AwaitingKey (arg :: args)
                            else
                                match exc with
                                | None ->
                                    failwithf
                                        "Unable to process supplied arg %s. Help text follows.\n%s"
                                        key
                                        (helpText ())
                                | Some msg -> msg |> ArgParser_errors.Add

            go ParseState_FlagsIntoPositionalArgsInt.AwaitingKey args
            let arg_1 = arg_1 |> Seq.toList

            let arg_0 =
                match arg_0 with
                | None ->
                    sprintf "Required argument '%s' received no value" (sprintf "--%s" "a")
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | Some x -> x

            if 0 = ArgParser_errors.Count then
                {
                    A = arg_0
                    GrabEverything = arg_1
                }
            else
                ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

        static member parse (args : string list) : FlagsIntoPositionalArgsInt =
            FlagsIntoPositionalArgsInt.parse' System.Environment.GetEnvironmentVariable args
namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type FlagsIntoPositionalArgsIntChoice
[<AutoOpen>]
module FlagsIntoPositionalArgsIntChoiceArgParse =
    type private ParseState_FlagsIntoPositionalArgsIntChoice =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    /// Extension methods for argument parsing
    type FlagsIntoPositionalArgsIntChoice with

        static member parse'
            (getEnvironmentVariable : string -> string)
            (args : string list)
            : FlagsIntoPositionalArgsIntChoice
            =
            let ArgParser_errors = ResizeArray ()

            let helpText () =
                [
                    (sprintf "%s  string%s%s" (sprintf "--%s" "a") "" "")
                    (sprintf
                        "%s  int32%s%s"
                        (sprintf "--%s" "grab-everything")
                        " (positional args) (can be repeated)"
                        "")
                ]
                |> String.concat "\n"

            let arg_1 : Choice<int, int> ResizeArray = ResizeArray ()
            let mutable arg_0 : string option = None

            /// Processes the key-value pair, returning Error if no key was matched.
            /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
            /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
            let processKeyValue (key : string) (value : string) : Result<unit, string option> =
                if System.String.Equals (key, sprintf "--%s" "a", System.StringComparison.OrdinalIgnoreCase) then
                    match arg_0 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "a")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_0 <- value |> (fun x -> x) |> Some
                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else if
                    System.String.Equals (
                        key,
                        sprintf "--%s" "grab-everything",
                        System.StringComparison.OrdinalIgnoreCase
                    )
                then
                    value |> (fun x -> System.Int32.Parse x) |> Choice1Of2 |> arg_1.Add
                    () |> Ok
                else
                    Error None

            /// Returns false if we didn't set a value.
            let setFlagValue (key : string) : bool = false

            let rec go (state : ParseState_FlagsIntoPositionalArgsIntChoice) (args : string list) =
                match args with
                | [] ->
                    match state with
                    | ParseState_FlagsIntoPositionalArgsIntChoice.AwaitingKey -> ()
                    | ParseState_FlagsIntoPositionalArgsIntChoice.AwaitingValue key ->
                        if setFlagValue key then
                            ()
                        else
                            sprintf
                                "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                                key
                            |> ArgParser_errors.Add
                | "--" :: rest -> arg_1.AddRange (rest |> Seq.map (fun x -> System.Int32.Parse x) |> Seq.map Choice2Of2)
                | arg :: args ->
                    match state with
                    | ParseState_FlagsIntoPositionalArgsIntChoice.AwaitingKey ->
                        if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                            if arg = "--help" then
                                helpText () |> failwithf "Help text requested.\n%s"
                            else
                                let equals = arg.IndexOf (char 61)

                                if equals < 0 then
                                    args |> go (ParseState_FlagsIntoPositionalArgsIntChoice.AwaitingValue arg)
                                else
                                    let key = arg.[0 .. equals - 1]
                                    let value = arg.[equals + 1 ..]

                                    match processKeyValue key value with
                                    | Ok () -> go ParseState_FlagsIntoPositionalArgsIntChoice.AwaitingKey args
                                    | Error x ->
                                        if true then
                                            arg |> (fun x -> System.Int32.Parse x) |> Choice1Of2 |> arg_1.Add
                                            go ParseState_FlagsIntoPositionalArgsIntChoice.AwaitingKey args
                                        else
                                            match x with
                                            | None ->
                                                failwithf
                                                    "Unable to process argument %s as key %s and value %s"
                                                    arg
                                                    key
                                                    value
                                            | Some msg ->
                                                sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                                go ParseState_FlagsIntoPositionalArgsIntChoice.AwaitingKey args
                        else
                            arg |> (fun x -> System.Int32.Parse x) |> Choice1Of2 |> arg_1.Add
                            go ParseState_FlagsIntoPositionalArgsIntChoice.AwaitingKey args
                    | ParseState_FlagsIntoPositionalArgsIntChoice.AwaitingValue key ->
                        match processKeyValue key arg with
                        | Ok () -> go ParseState_FlagsIntoPositionalArgsIntChoice.AwaitingKey args
                        | Error exc ->
                            if setFlagValue key then
                                go ParseState_FlagsIntoPositionalArgsIntChoice.AwaitingKey (arg :: args)
                            else if true then
                                key |> (fun x -> System.Int32.Parse x) |> Choice1Of2 |> arg_1.Add
                                go ParseState_FlagsIntoPositionalArgsIntChoice.AwaitingKey (arg :: args)
                            else
                                match exc with
                                | None ->
                                    failwithf
                                        "Unable to process supplied arg %s. Help text follows.\n%s"
                                        key
                                        (helpText ())
                                | Some msg -> msg |> ArgParser_errors.Add

            go ParseState_FlagsIntoPositionalArgsIntChoice.AwaitingKey args
            let arg_1 = arg_1 |> Seq.toList

            let arg_0 =
                match arg_0 with
                | None ->
                    sprintf "Required argument '%s' received no value" (sprintf "--%s" "a")
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | Some x -> x

            if 0 = ArgParser_errors.Count then
                {
                    A = arg_0
                    GrabEverything = arg_1
                }
            else
                ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

        static member parse (args : string list) : FlagsIntoPositionalArgsIntChoice =
            FlagsIntoPositionalArgsIntChoice.parse' System.Environment.GetEnvironmentVariable args
namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type FlagsIntoPositionalArgs'
[<AutoOpen>]
module FlagsIntoPositionalArgs'ArgParse =
    type private ParseState_FlagsIntoPositionalArgs' =
        /// Ready to consume a key or positional arg
        | AwaitingKey
        /// Waiting to receive a value for the key we've already consumed
        | AwaitingValue of key : string

    /// Extension methods for argument parsing
    type FlagsIntoPositionalArgs' with

        static member parse'
            (getEnvironmentVariable : string -> string)
            (args : string list)
            : FlagsIntoPositionalArgs'
            =
            let ArgParser_errors = ResizeArray ()

            let helpText () =
                [
                    (sprintf "%s  string%s%s" (sprintf "--%s" "a") "" "")
                    (sprintf
                        "%s  string%s%s"
                        (sprintf "--%s" "dont-grab-everything")
                        " (positional args) (can be repeated)"
                        "")
                ]
                |> String.concat "\n"

            let arg_1 : string ResizeArray = ResizeArray ()
            let mutable arg_0 : string option = None

            /// Processes the key-value pair, returning Error if no key was matched.
            /// If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>).
            /// This can nevertheless be a successful parse, e.g. when the key may have arity 0.
            let processKeyValue (key : string) (value : string) : Result<unit, string option> =
                if System.String.Equals (key, sprintf "--%s" "a", System.StringComparison.OrdinalIgnoreCase) then
                    match arg_0 with
                    | Some x ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (sprintf "--%s" "a")
                            (x.ToString ())
                            (value.ToString ())
                        |> ArgParser_errors.Add

                        Ok ()
                    | None ->
                        try
                            arg_0 <- value |> (fun x -> x) |> Some
                            Ok ()
                        with _ as exc ->
                            exc.Message |> Some |> Error
                else if
                    System.String.Equals (
                        key,
                        sprintf "--%s" "dont-grab-everything",
                        System.StringComparison.OrdinalIgnoreCase
                    )
                then
                    value |> (fun x -> x) |> arg_1.Add
                    () |> Ok
                else
                    Error None

            /// Returns false if we didn't set a value.
            let setFlagValue (key : string) : bool = false

            let rec go (state : ParseState_FlagsIntoPositionalArgs') (args : string list) =
                match args with
                | [] ->
                    match state with
                    | ParseState_FlagsIntoPositionalArgs'.AwaitingKey -> ()
                    | ParseState_FlagsIntoPositionalArgs'.AwaitingValue key ->
                        if setFlagValue key then
                            ()
                        else
                            sprintf
                                "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                                key
                            |> ArgParser_errors.Add
                | "--" :: rest -> arg_1.AddRange (rest |> Seq.map (fun x -> x))
                | arg :: args ->
                    match state with
                    | ParseState_FlagsIntoPositionalArgs'.AwaitingKey ->
                        if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                            if arg = "--help" then
                                helpText () |> failwithf "Help text requested.\n%s"
                            else
                                let equals = arg.IndexOf (char 61)

                                if equals < 0 then
                                    args |> go (ParseState_FlagsIntoPositionalArgs'.AwaitingValue arg)
                                else
                                    let key = arg.[0 .. equals - 1]
                                    let value = arg.[equals + 1 ..]

                                    match processKeyValue key value with
                                    | Ok () -> go ParseState_FlagsIntoPositionalArgs'.AwaitingKey args
                                    | Error x ->
                                        if false then
                                            arg |> (fun x -> x) |> arg_1.Add
                                            go ParseState_FlagsIntoPositionalArgs'.AwaitingKey args
                                        else
                                            match x with
                                            | None ->
                                                failwithf
                                                    "Unable to process argument %s as key %s and value %s"
                                                    arg
                                                    key
                                                    value
                                            | Some msg ->
                                                sprintf "%s (at arg %s)" msg arg |> ArgParser_errors.Add
                                                go ParseState_FlagsIntoPositionalArgs'.AwaitingKey args
                        else
                            arg |> (fun x -> x) |> arg_1.Add
                            go ParseState_FlagsIntoPositionalArgs'.AwaitingKey args
                    | ParseState_FlagsIntoPositionalArgs'.AwaitingValue key ->
                        match processKeyValue key arg with
                        | Ok () -> go ParseState_FlagsIntoPositionalArgs'.AwaitingKey args
                        | Error exc ->
                            if setFlagValue key then
                                go ParseState_FlagsIntoPositionalArgs'.AwaitingKey (arg :: args)
                            else if false then
                                key |> (fun x -> x) |> arg_1.Add
                                go ParseState_FlagsIntoPositionalArgs'.AwaitingKey (arg :: args)
                            else
                                match exc with
                                | None ->
                                    failwithf
                                        "Unable to process supplied arg %s. Help text follows.\n%s"
                                        key
                                        (helpText ())
                                | Some msg -> msg |> ArgParser_errors.Add

            go ParseState_FlagsIntoPositionalArgs'.AwaitingKey args
            let arg_1 = arg_1 |> Seq.toList

            let arg_0 =
                match arg_0 with
                | None ->
                    sprintf "Required argument '%s' received no value" (sprintf "--%s" "a")
                    |> ArgParser_errors.Add

                    Unchecked.defaultof<_>
                | Some x -> x

            if 0 = ArgParser_errors.Count then
                {
                    A = arg_0
                    DontGrabEverything = arg_1
                }
            else
                ArgParser_errors |> String.concat "\n" |> failwithf "Errors during parse!\n%s"

        static member parse (args : string list) : FlagsIntoPositionalArgs' =
            FlagsIntoPositionalArgs'.parse' System.Environment.GetEnvironmentVariable args
