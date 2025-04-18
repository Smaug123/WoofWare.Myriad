namespace Playground

module Program =
    [<EntryPoint>]
    let main argv =
        [ "--whatnot=2024-01-12" ; "--info1=4" ; "--info2=hi" ]
        |> Args.parse
        |> printfn "%O"

        0
