namespace WoofWare.Myriad.Plugins

type internal DesiredGenerator =
    | InterfaceMock of isInternal : bool option
    | JsonParse of extensionMethod : bool option
    | JsonSerialize of extensionMethod : bool option
    | HttpClient of extensionMethod : bool option

    static member Parse (s : string) =
        match s with
        | "GenerateMock" -> DesiredGenerator.InterfaceMock None
        | "GenerateMock(true)" -> DesiredGenerator.InterfaceMock (Some true)
        | "GenerateMock(false)" -> DesiredGenerator.InterfaceMock (Some false)
        | "JsonParse" -> DesiredGenerator.JsonParse None
        | "JsonParse(true)" -> DesiredGenerator.JsonParse (Some true)
        | "JsonParse(false)" -> DesiredGenerator.JsonParse (Some false)
        | "JsonSerialize" -> DesiredGenerator.JsonSerialize None
        | "JsonSerialize(true)" -> DesiredGenerator.JsonSerialize (Some true)
        | "JsonSerialize(false)" -> DesiredGenerator.JsonSerialize (Some false)
        | "HttpClient" -> DesiredGenerator.HttpClient None
        | "HttpClient(true)" -> DesiredGenerator.HttpClient (Some true)
        | "HttpClient(false)" -> DesiredGenerator.HttpClient (Some false)
        | _ -> failwith $"Failed to parse as a generator specification: %s{s}"
