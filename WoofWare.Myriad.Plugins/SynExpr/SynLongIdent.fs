namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text.Range
open Fantomas.FCS.Syntax

[<RequireQualifiedAccess>]
module internal SynLongIdent =

    let geq =
        SynLongIdent.SynLongIdent (
            [ Ident.create "op_GreaterThanOrEqual" ],
            [],
            [ Some (IdentTrivia.OriginalNotation ">=") ]
        )

    let leq =
        SynLongIdent.SynLongIdent (
            [ Ident.create "op_LessThanOrEqual" ],
            [],
            [ Some (IdentTrivia.OriginalNotation "<=") ]
        )

    let gt =
        SynLongIdent.SynLongIdent ([ Ident.create "op_GreaterThan" ], [], [ Some (IdentTrivia.OriginalNotation ">") ])

    let lt =
        SynLongIdent.SynLongIdent ([ Ident.create "op_LessThan" ], [], [ Some (IdentTrivia.OriginalNotation "<") ])

    let sub =
        SynLongIdent.SynLongIdent ([ Ident.create "op_Subtraction" ], [], [ Some (IdentTrivia.OriginalNotation "-") ])

    let eq =
        SynLongIdent.SynLongIdent ([ Ident.create "op_Equality" ], [], [ Some (IdentTrivia.OriginalNotation "=") ])

    let booleanAnd =
        SynLongIdent.SynLongIdent ([ Ident.create "op_BooleanAnd" ], [], [ Some (IdentTrivia.OriginalNotation "&&") ])

    let booleanOr =
        SynLongIdent.SynLongIdent ([ Ident.create "op_BooleanOr" ], [], [ Some (IdentTrivia.OriginalNotation "||") ])

    let plus =
        SynLongIdent.SynLongIdent ([ Ident.create "op_Addition" ], [], [ Some (IdentTrivia.OriginalNotation "+") ])

    let times =
        SynLongIdent.SynLongIdent ([ Ident.create "op_Multiply" ], [], [ Some (IdentTrivia.OriginalNotation "*") ])

    let pipe =
        SynLongIdent.SynLongIdent ([ Ident.create "op_PipeRight" ], [], [ Some (IdentTrivia.OriginalNotation "|>") ])

    let toString (sli : SynLongIdent) : string =
        sli.LongIdent |> List.map _.idText |> String.concat "."

    let create (ident : LongIdent) : SynLongIdent =
        let commas =
            match ident with
            | [] -> []
            | _ :: commas -> commas |> List.map (fun _ -> range0)

        SynLongIdent.SynLongIdent (ident, commas, List.replicate ident.Length None)

    let inline createI (i : Ident) : SynLongIdent = create [ i ]

    let inline createS (s : string) : SynLongIdent = createI (Ident (s, range0))

    let inline createS' (s : string list) : SynLongIdent =
        create (s |> List.map (fun i -> Ident (i, range0)))

    let isUnit (ident : SynLongIdent) : bool =
        match ident.LongIdent with
        | [ i ] when System.String.Equals (i.idText, "unit", System.StringComparison.OrdinalIgnoreCase) -> true
        | _ -> false

    let isList (ident : SynLongIdent) : bool =
        match ident.LongIdent with
        | [ i ] when System.String.Equals (i.idText, "list", System.StringComparison.OrdinalIgnoreCase) -> true
        // TODO: consider FSharpList or whatever it is
        | _ -> false

    let isArray (ident : SynLongIdent) : bool =
        match ident.LongIdent with
        | [ i ] when
            System.String.Equals (i.idText, "array", System.StringComparison.OrdinalIgnoreCase)
            || System.String.Equals (i.idText, "[]", System.StringComparison.Ordinal)
            ->
            true
        | _ -> false

    let isOption (ident : SynLongIdent) : bool =
        match ident.LongIdent with
        | [ i ] when System.String.Equals (i.idText, "option", System.StringComparison.OrdinalIgnoreCase) -> true
        // TODO: consider Microsoft.FSharp.Option or whatever it is
        | _ -> false

    let isChoice (ident : SynLongIdent) : bool =
        match ident.LongIdent with
        | [ i ] when System.String.Equals (i.idText, "Choice", System.StringComparison.Ordinal) -> true
        // TODO: consider Microsoft.FSharp.Choice or whatever it is
        | _ -> false

    let isNullable (ident : SynLongIdent) : bool =
        match ident.LongIdent |> List.map _.idText with
        | [ "System" ; "Nullable" ]
        | [ "Nullable" ] -> true
        | _ -> false

    let isResponse (ident : SynLongIdent) : bool =
        match ident.LongIdent |> List.map _.idText with
        | [ "Response" ]
        | [ "RestEase" ; "Response" ] -> true
        | _ -> false

    let isMap (ident : SynLongIdent) : bool =
        match ident.LongIdent |> List.map _.idText with
        | [ "Map" ] -> true
        | _ -> false

    let isReadOnlyDictionary (ident : SynLongIdent) : bool =
        match ident.LongIdent |> List.map _.idText with
        | [ "IReadOnlyDictionary" ]
        | [ "Generic" ; "IReadOnlyDictionary" ]
        | [ "Collections" ; "Generic" ; "IReadOnlyDictionary" ]
        | [ "System" ; "Collections" ; "Generic" ; "IReadOnlyDictionary" ] -> true
        | _ -> false

    let isDictionary (ident : SynLongIdent) : bool =
        match ident.LongIdent |> List.map _.idText with
        | [ "Dictionary" ]
        | [ "Generic" ; "Dictionary" ]
        | [ "Collections" ; "Generic" ; "Dictionary" ]
        | [ "System" ; "Collections" ; "Generic" ; "Dictionary" ] -> true
        | _ -> false

    let isIDictionary (ident : SynLongIdent) : bool =
        match ident.LongIdent |> List.map _.idText with
        | [ "IDictionary" ]
        | [ "Generic" ; "IDictionary" ]
        | [ "Collections" ; "Generic" ; "IDictionary" ]
        | [ "System" ; "Collections" ; "Generic" ; "IDictionary" ] -> true
        | _ -> false
