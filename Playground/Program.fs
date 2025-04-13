namespace Playground

open GeneratedParsers

module Program =
    [<EntryPoint>]
    let main argv =
        ["--whatnot=2024-01-12";"--info1=4";"--info2=hi"]
        |> ArgsModule.parse
        |> printfn "%O"
        0
