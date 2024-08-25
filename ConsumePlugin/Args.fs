namespace ConsumePlugin

open System.IO
open WoofWare.Myriad.Plugins

[<ArgParser>]
type BasicNoPositionals =
    {
        Foo : int
        Bar : string
        Baz : bool
        Rest : int list
    }

[<ArgParser>]
type Basic =
    {
        [<ArgumentHelpText "This is a foo!">]
        Foo : int
        Bar : string
        Baz : bool
        [<ArgumentHelpText "Here's where the rest of the args go">]
        [<PositionalArgs>]
        Rest : string list
    }

[<ArgParser>]
type BasicWithIntPositionals =
    {
        Foo : int
        Bar : string
        Baz : bool
        [<PositionalArgs>]
        Rest : int list
    }

[<ArgParser>]
type LoadsOfTypes =
    {
        Foo : int
        Bar : string
        Baz : bool
        SomeFile : FileInfo
        SomeDirectory : DirectoryInfo
        SomeList : DirectoryInfo list
        OptionalThingWithNoDefault : int option
        [<PositionalArgs>]
        Positionals : int list
        [<ArgumentDefaultFunction>]
        OptionalThing : Choice<bool, bool>
        [<ArgumentDefaultFunction>]
        AnotherOptionalThing : Choice<int, int>
        [<ArgumentDefaultEnvironmentVariable "CONSUMEPLUGIN_THINGS">]
        YetAnotherOptionalThing : Choice<string, string>
    }

    static member DefaultOptionalThing () = true

    static member DefaultAnotherOptionalThing () = 3

[<ArgParser>]
type LoadsOfTypesNoPositionals =
    {
        Foo : int
        Bar : string
        Baz : bool
        SomeFile : FileInfo
        SomeDirectory : DirectoryInfo
        SomeList : DirectoryInfo list
        OptionalThingWithNoDefault : int option
        [<ArgumentDefaultFunction>]
        OptionalThing : Choice<bool, bool>
        [<ArgumentDefaultFunction>]
        AnotherOptionalThing : Choice<int, int>
        [<ArgumentDefaultEnvironmentVariable "CONSUMEPLUGIN_THINGS">]
        YetAnotherOptionalThing : Choice<string, string>
    }

    static member DefaultOptionalThing () = false

    static member DefaultAnotherOptionalThing () = 3
