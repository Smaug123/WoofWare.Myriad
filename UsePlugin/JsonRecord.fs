namespace UsePlugin

[<MyriadPlugin.JsonParse>]
type InnerType =
    {
        Thing : string
    }

/// My whatnot
[<MyriadPlugin.JsonParse>]
type JsonRecordType =
    {
        /// A thing!
        A : int
        /// Another thing!
        B : string
        C : int list
        D : InnerType
    }
