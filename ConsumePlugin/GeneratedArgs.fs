//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------






namespace ConsumePlugin

open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type Args
[<RequireQualifiedAccess ; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Args =
    type ParseState =
        | AwaitingKey
        | AwaitingValue of key : string

    let parse (args : string list) : Args =
        let Positionals : int ResizeArray = ResizeArray ()
        let mutable Foo : int option = None
        let mutable Bar : string option = None
        let mutable Baz : bool option = None
        let mutable SomeFile : FileInfo option = None
        let mutable SomeDirectory : DirectoryInfo option = None
        let SomeList : DirectoryInfo ResizeArray = ResizeArray ()
        let mutable OptionalThingWithNoDefault : int option = None

        /// Processes the key-value pair, returning false if no key was matched. Also throws if an invalid value was received.
        let processKeyValue (key : string) (value : string) : bool =
            if
                System.String.Equals (
                    key,
                    "---optional-thing-with-no-default",
                    System.StringComparison.OrdinalIgnoreCase
                )
            then
                match OptionalThingWithNoDefault with
                | Some x ->
                    failwithf
                        "Argument '%s' was supplied multiple times: %O and %O"
                        "---optional-thing-with-no-default"
                        x
                        value
                | None ->
                    OptionalThingWithNoDefault <- value |> (fun x -> System.Int32.Parse x) |> Some
                    true
            else if System.String.Equals (key, "---some-list", System.StringComparison.OrdinalIgnoreCase) then
                (fun x -> System.IO.DirectoryInfo x) value |> SomeList.Add
                true
            else if System.String.Equals (key, "---some-directory", System.StringComparison.OrdinalIgnoreCase) then
                match SomeDirectory with
                | Some x -> failwithf "Argument '%s' was supplied multiple times: %O and %O" "---some-directory" x value
                | None ->
                    SomeDirectory <- value |> (fun x -> System.IO.DirectoryInfo x) |> Some
                    true
            else if System.String.Equals (key, "---some-file", System.StringComparison.OrdinalIgnoreCase) then
                match SomeFile with
                | Some x -> failwithf "Argument '%s' was supplied multiple times: %O and %O" "---some-file" x value
                | None ->
                    SomeFile <- value |> (fun x -> System.IO.FileInfo x) |> Some
                    true
            else if System.String.Equals (key, "---baz", System.StringComparison.OrdinalIgnoreCase) then
                match Baz with
                | Some x -> failwithf "Argument '%s' was supplied multiple times: %O and %O" "---baz" x value
                | None ->
                    Baz <- value |> (fun x -> System.Boolean.Parse x) |> Some
                    true
            else if System.String.Equals (key, "---bar", System.StringComparison.OrdinalIgnoreCase) then
                match Bar with
                | Some x -> failwithf "Argument '%s' was supplied multiple times: %O and %O" "---bar" x value
                | None ->
                    Bar <- value |> (fun x -> x) |> Some
                    true
            else if System.String.Equals (key, "---foo", System.StringComparison.OrdinalIgnoreCase) then
                match Foo with
                | Some x -> failwithf "Argument '%s' was supplied multiple times: %O and %O" "---foo" x value
                | None ->
                    Foo <- value |> (fun x -> System.Int32.Parse x) |> Some
                    true
            else
                false

        /// Returns false if we didn't set a value.
        let setFlagValue (key : string) : bool =
            if System.String.Equals (key, "---baz", System.StringComparison.OrdinalIgnoreCase) then
                match Baz with
                | Some x -> failwithf "Flag '%s' was supplied multiple times: %O and %O" "---baz" x x
                | None ->
                    Baz <- Some true
                    true
            else
                false

        let rec go (state : ParseState) (args : string list) =
            match args with
            | [] -> ()
            | "--" :: rest -> Positionals.AddRange (rest |> Seq.map (fun x -> System.Int32.Parse x))
            | arg :: args ->
                match state with
                | ParseState.AwaitingKey ->
                    if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                        let equals = arg.IndexOf (char 61)

                        if equals > 0 then
                            go ParseState.AwaitingKey args
                        else
                            let key = arg.[0 .. equals - 1]
                            let value = arg.[equals + 1 ..]

                            if processKeyValue key value then
                                go ParseState.AwaitingKey args
                            else
                                failwithf "Unable to process argument %s as key %s and value %s" arg key value
                    else
                        arg |> (fun x -> System.Int32.Parse x) |> Positionals.Add
                        go ParseState.AwaitingKey args
                | ParseState.AwaitingValue key ->
                    if processKeyValue key arg then
                        go ParseState.AwaitingKey args
                    else if setFlagValue key then
                        go ParseState.AwaitingKey (arg :: args)
                    else
                        failwithf "Unable to process value %s for arg %s" arg key

        go ParseState.AwaitingKey args
        let Positionals = Positionals |> Seq.toList

        let Foo =
            match Foo with
            | None -> failwithf "Required argument '%s' was missing" "---foo"
            | Some x -> x

        let Bar =
            match Bar with
            | None -> failwithf "Required argument '%s' was missing" "---bar"
            | Some x -> x

        let Baz =
            match Baz with
            | None -> failwithf "Required argument '%s' was missing" "---baz"
            | Some x -> x

        let SomeFile =
            match SomeFile with
            | None -> failwithf "Required argument '%s' was missing" "---some-file"
            | Some x -> x

        let SomeDirectory =
            match SomeDirectory with
            | None -> failwithf "Required argument '%s' was missing" "---some-directory"
            | Some x -> x

        let SomeList = SomeList |> Seq.toList
        let OptionalThingWithNoDefault = OptionalThingWithNoDefault

        {
            Positionals = Positionals
            Foo = Foo
            Bar = Bar
            Baz = Baz
            SomeFile = SomeFile
            SomeDirectory = SomeDirectory
            SomeList = SomeList
            OptionalThingWithNoDefault = OptionalThingWithNoDefault
        }
namespace ConsumePlugin

open System.IO
open WoofWare.Myriad.Plugins

/// Methods to parse arguments for the type ArgsNoPositionals
[<RequireQualifiedAccess ; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ArgsNoPositionals =
    type ParseState =
        | AwaitingKey
        | AwaitingValue of key : string

    let parse (args : string list) : ArgsNoPositionals =
        let parser_LeftoverArgs : string ResizeArray = ResizeArray ()
        let mutable Foo : int option = None
        let mutable Bar : string option = None
        let mutable Baz : bool option = None
        let mutable SomeFile : FileInfo option = None
        let mutable SomeDirectory : DirectoryInfo option = None
        let SomeList : DirectoryInfo ResizeArray = ResizeArray ()
        let mutable OptionalThingWithNoDefault : int option = None

        /// Processes the key-value pair, returning false if no key was matched. Also throws if an invalid value was received.
        let processKeyValue (key : string) (value : string) : bool =
            if
                System.String.Equals (
                    key,
                    "---optional-thing-with-no-default",
                    System.StringComparison.OrdinalIgnoreCase
                )
            then
                match OptionalThingWithNoDefault with
                | Some x ->
                    failwithf
                        "Argument '%s' was supplied multiple times: %O and %O"
                        "---optional-thing-with-no-default"
                        x
                        value
                | None ->
                    OptionalThingWithNoDefault <- value |> (fun x -> System.Int32.Parse x) |> Some
                    true
            else if System.String.Equals (key, "---some-list", System.StringComparison.OrdinalIgnoreCase) then
                (fun x -> System.IO.DirectoryInfo x) value |> SomeList.Add
                true
            else if System.String.Equals (key, "---some-directory", System.StringComparison.OrdinalIgnoreCase) then
                match SomeDirectory with
                | Some x -> failwithf "Argument '%s' was supplied multiple times: %O and %O" "---some-directory" x value
                | None ->
                    SomeDirectory <- value |> (fun x -> System.IO.DirectoryInfo x) |> Some
                    true
            else if System.String.Equals (key, "---some-file", System.StringComparison.OrdinalIgnoreCase) then
                match SomeFile with
                | Some x -> failwithf "Argument '%s' was supplied multiple times: %O and %O" "---some-file" x value
                | None ->
                    SomeFile <- value |> (fun x -> System.IO.FileInfo x) |> Some
                    true
            else if System.String.Equals (key, "---baz", System.StringComparison.OrdinalIgnoreCase) then
                match Baz with
                | Some x -> failwithf "Argument '%s' was supplied multiple times: %O and %O" "---baz" x value
                | None ->
                    Baz <- value |> (fun x -> System.Boolean.Parse x) |> Some
                    true
            else if System.String.Equals (key, "---bar", System.StringComparison.OrdinalIgnoreCase) then
                match Bar with
                | Some x -> failwithf "Argument '%s' was supplied multiple times: %O and %O" "---bar" x value
                | None ->
                    Bar <- value |> (fun x -> x) |> Some
                    true
            else if System.String.Equals (key, "---foo", System.StringComparison.OrdinalIgnoreCase) then
                match Foo with
                | Some x -> failwithf "Argument '%s' was supplied multiple times: %O and %O" "---foo" x value
                | None ->
                    Foo <- value |> (fun x -> System.Int32.Parse x) |> Some
                    true
            else
                false

        /// Returns false if we didn't set a value.
        let setFlagValue (key : string) : bool =
            if System.String.Equals (key, "---baz", System.StringComparison.OrdinalIgnoreCase) then
                match Baz with
                | Some x -> failwithf "Flag '%s' was supplied multiple times: %O and %O" "---baz" x x
                | None ->
                    Baz <- Some true
                    true
            else
                false

        let rec go (state : ParseState) (args : string list) =
            match args with
            | [] -> ()
            | "--" :: rest -> parser_LeftoverArgs.AddRange (rest |> Seq.map (fun x -> x))
            | arg :: args ->
                match state with
                | ParseState.AwaitingKey ->
                    if arg.StartsWith ("--", System.StringComparison.Ordinal) then
                        let equals = arg.IndexOf (char 61)

                        if equals > 0 then
                            go ParseState.AwaitingKey args
                        else
                            let key = arg.[0 .. equals - 1]
                            let value = arg.[equals + 1 ..]

                            if processKeyValue key value then
                                go ParseState.AwaitingKey args
                            else
                                failwithf "Unable to process argument %s as key %s and value %s" arg key value
                    else
                        arg |> (fun x -> x) |> parser_LeftoverArgs.Add
                        go ParseState.AwaitingKey args
                | ParseState.AwaitingValue key ->
                    if processKeyValue key arg then
                        go ParseState.AwaitingKey args
                    else if setFlagValue key then
                        go ParseState.AwaitingKey (arg :: args)
                    else
                        failwithf "Unable to process value %s for arg %s" arg key

        go ParseState.AwaitingKey args

        let parser_LeftoverArgs =
            if 0 = parser_LeftoverArgs.Count then
                ()
            else
                parser_LeftoverArgs
                |> String.concat " "
                |> failwithf "There were leftover args: %s"

        let Foo =
            match Foo with
            | None -> failwithf "Required argument '%s' was missing" "---foo"
            | Some x -> x

        let Bar =
            match Bar with
            | None -> failwithf "Required argument '%s' was missing" "---bar"
            | Some x -> x

        let Baz =
            match Baz with
            | None -> failwithf "Required argument '%s' was missing" "---baz"
            | Some x -> x

        let SomeFile =
            match SomeFile with
            | None -> failwithf "Required argument '%s' was missing" "---some-file"
            | Some x -> x

        let SomeDirectory =
            match SomeDirectory with
            | None -> failwithf "Required argument '%s' was missing" "---some-directory"
            | Some x -> x

        let SomeList = SomeList |> Seq.toList
        let OptionalThingWithNoDefault = OptionalThingWithNoDefault

        {
            Foo = Foo
            Bar = Bar
            Baz = Baz
            SomeFile = SomeFile
            SomeDirectory = SomeDirectory
            SomeList = SomeList
            OptionalThingWithNoDefault = OptionalThingWithNoDefault
        }
