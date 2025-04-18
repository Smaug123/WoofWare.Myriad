namespace Playground

open System
open WoofWare.Myriad.Plugins

[<ArgParser>]
type SubMode1 =
    {
        Info1 : int
        Info2 : string
        Rest : string list
    }

[<ArgParser>]
type SubMode2 =
    {
        Info1 : int
        Info2 : string
        Rest : int list
    }

[<ArgParser>]
type Mode1 =
    {
        Things : SubMode1
        Whatnot : int
    }

[<ArgParser>]
type Mode2 =
    {
        Things : SubMode2
        Whatnot : DateTime
    }

[<ArgParser>]
type Modes =
    | Mode1 of Mode1
    | Mode2 of Mode2

[<ArgParser>]
type Args =
    {
        WhatToDo : Modes
        [<PositionalArgs>]
        OtherArgs : string list
    }
