namespace ConsumePlugin

[<WoofWare.Myriad.Plugins.JsonParse>]
type InnerStruct =
    {
        A : int
    }

[<WoofWare.Myriad.Plugins.JsonParse>]
type ArrayOfInnerStruct =
    {
        B : InnerStruct array
    }
