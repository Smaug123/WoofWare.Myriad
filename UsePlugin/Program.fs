namespace UsePlugin

module Program =
    let f : RecordType =
        {
            A = Some 300
            B = "hello"
            C = [ 0.3 ]
        }

    let g = RecordType.shorten f

    [<EntryPoint>]
    let main _ =
        if not (f.B = g.B && f.C = g.C) then
            failwith "Non-optional fields differed"
        match f.A with
        | None ->
            if g.A <> RecordType.DefaultA () then
                failwith "Couldn't acquire default"
        | Some a ->
            if a <> g.A then
                failwith "Didn't match existing f.A"
        0
