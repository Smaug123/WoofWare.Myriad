namespace UsePlugin

module Program =
    let f : RecordType =
        {
            A = Some 3
            B = "hello"
            C = [ 0.3 ]
        }

    let g : RecordType.Short =
        {
            A = 3
            B = "hello"
            C = [ 0.3 ]
        }

    [<EntryPoint>]
    let main _ = 0
