namespace ConsumePlugin.ArgsWithUnions

open System
open System.IO
open WoofWare.Myriad.Plugins

type BasicNoPositionals =
    {
        Foo : int
        Bar : string
        Baz : bool
        Rest : int list
    }

type UsernamePasswordAuth =
    {
        Username : string
        Password : string
    }

type TokenAuth =
    {
        Token : string
    }

type AuthOptions =
    | UsernamePassword of UsernamePasswordAuth
    | Token of TokenAuth

[<ArgParser>]
type DoTheThing =
    {
        Basics : BasicNoPositionals
        Auth : AuthOptions
    }
