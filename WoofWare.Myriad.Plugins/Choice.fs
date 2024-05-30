namespace WoofWare.Myriad.Plugins

[<RequireQualifiedAccess>]
module internal Choice =
    let get<'a> (choice : Choice<'a, 'a>) : 'a =
        match choice with
        | Choice1Of2 a -> a
        | Choice2Of2 a -> a
