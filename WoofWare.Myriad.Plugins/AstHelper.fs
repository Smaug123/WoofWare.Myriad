namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax

/// Anything that is part of an ADT.
/// A record is a product of stuff; this type represents one of those stuffs.
type internal AdtNode =
    {
        Type : SynType
        Name : Ident option
        /// An ordered list, so you can look up any given generic within `this.Type`
        /// to discover what its index is in the parent DU which defined it.
        GenericsOfParent : SynTyparDecl list
    }

/// A DU is a sum of products (e.g. `type Thing = Foo of a * b`);
/// similarly a record is a product.
/// This type represents a product in that sense.
type internal AdtProduct =
    {
        Name : SynIdent
        Fields : AdtNode list
        /// This AdtProduct represents a product in which there might be
        /// some bound type parameters. This field lists the bound
        /// type parameters in the order they appeared on the parent type.
        Generics : SynTyparDecl list
    }

module internal AstHelper =
    let getUnionCases
        (SynTypeDefn.SynTypeDefn (info, repr, _, _, _, _))
        : AdtProduct list * SynTyparDecl list * SynAccess option
        =
        let typars, access =
            match info with
            | SynComponentInfo (_, typars, _, _, _, _, access, _) -> typars, access

        let typars =
            match typars with
            | None -> []
            | Some (SynTyparDecls.PrefixList (decls, _)) -> decls
            | Some (SynTyparDecls.SinglePrefix (l, _)) -> [ l ]
            | Some (SynTyparDecls.PostfixList (decls, constraints, _)) ->
                if not constraints.IsEmpty then
                    failwith "Constrained type parameters not currently supported"

                decls

        match repr with
        | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union (_, cases, _), _) ->
            let cases =
                cases
                |> List.map (fun (SynUnionCase.SynUnionCase (_, ident, kind, _, _, _, _)) ->
                    match kind with
                    | SynUnionCaseKind.FullType _ -> failwith "FullType union cases not supported"
                    | SynUnionCaseKind.Fields fields ->
                        {
                            Name = ident
                            Fields =
                                fields
                                |> List.map (fun (SynField.SynField (_, _, id, ty, _, _, _, _, _)) ->
                                    {
                                        Type = ty
                                        Name = id
                                        GenericsOfParent = typars
                                    }
                                )
                            Generics = typars
                        }
                )

            cases, typars, access
        | _ -> failwithf "Failed to get union cases for type that was: %+A" repr

    let getRecordFields (SynTypeDefn.SynTypeDefn (typeInfo, repr, _, _, _, _)) : AdtNode list =
        let (SynComponentInfo.SynComponentInfo (typeParams = typars)) = typeInfo

        let typars =
            match typars with
            | None -> []
            | Some (SynTyparDecls.PrefixList (decls, _)) -> decls
            | Some (SynTyparDecls.SinglePrefix (l, _)) -> [ l ]
            | Some (SynTyparDecls.PostfixList (decls, constraints, _)) ->
                if not constraints.IsEmpty then
                    failwith "Constrained type parameters not currently supported"

                decls

        match repr with
        | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (_, fields, _), _) ->
            fields
            |> List.map (fun (SynField.SynField (_, _, ident, ty, _, _, _, _, _)) ->
                {
                    Name = ident
                    Type = ty
                    GenericsOfParent = typars
                }
            )
        | _ -> failwithf "Failed to get record elements for type that was: %+A" repr
