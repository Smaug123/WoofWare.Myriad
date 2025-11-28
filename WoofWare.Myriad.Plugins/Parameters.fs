namespace WoofWare.Myriad.Plugins

open System

type internal DesiredGenerator =
    | InterfaceMock of isInternal : bool option
    | CapturingInterfaceMock of isInternal : bool option
    | JsonParse of extensionMethod : bool option
    | JsonSerialize of extensionMethod : bool option
    | HttpClient of extensionMethod : bool option
    | CreateCatamorphism of typeName : string

    static member Parse (s : string) =
        match s with
        | "GenerateMock" -> DesiredGenerator.InterfaceMock None
        | "GenerateMock(true)" -> DesiredGenerator.InterfaceMock (Some true)
        | "GenerateMock(false)" -> DesiredGenerator.InterfaceMock (Some false)
        | "GenerateCapturingMock" -> DesiredGenerator.CapturingInterfaceMock None
        | "GenerateCapturingMock(true)" -> DesiredGenerator.CapturingInterfaceMock (Some true)
        | "GenerateCapturingMock(false)" -> DesiredGenerator.CapturingInterfaceMock (Some false)
        | "JsonParse" -> DesiredGenerator.JsonParse None
        | "JsonParse(true)" -> DesiredGenerator.JsonParse (Some true)
        | "JsonParse(false)" -> DesiredGenerator.JsonParse (Some false)
        | "JsonSerialize" -> DesiredGenerator.JsonSerialize None
        | "JsonSerialize(true)" -> DesiredGenerator.JsonSerialize (Some true)
        | "JsonSerialize(false)" -> DesiredGenerator.JsonSerialize (Some false)
        | "HttpClient" -> DesiredGenerator.HttpClient None
        | "HttpClient(true)" -> DesiredGenerator.HttpClient (Some true)
        | "HttpClient(false)" -> DesiredGenerator.HttpClient (Some false)
        | _ ->
            let prefix = "CreateCatamorphism("

            if s.StartsWith (prefix, StringComparison.Ordinal) && s.EndsWith ')' then
                DesiredGenerator.CreateCatamorphism (s.Substring (prefix.Length, s.Length - prefix.Length - 1))
            else
                failwith $"Failed to parse as a generator specification: %s{s}"
