namespace WoofWare.Myriad.Plugins

[<RequireQualifiedAccess>]
module private List =
    let partitionChoice<'a, 'b> (xs : Choice<'a, 'b> list) : 'a list * 'b list =
        let xs, ys =
            (([], []), xs)
            ||> List.fold (fun (xs, ys) v ->
                match v with
                | Choice1Of2 x -> x :: xs, ys
                | Choice2Of2 y -> xs, y :: ys
            )

        List.rev xs, List.rev ys

    let allSome<'a> (l : 'a option list) : 'a list option =
        let rec go acc (l : 'a option list) =
            match l with
            | [] -> Some (List.rev acc)
            | None :: _ -> None
            | Some head :: tail -> go (head :: acc) tail

        go [] l
