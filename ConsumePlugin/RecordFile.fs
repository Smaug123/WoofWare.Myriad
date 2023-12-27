namespace ConsumePlugin

type ParseState =
    | AwaitingKey
    | AwaitingValue of string

/// My whatnot
[<WoofWare.Myriad.Plugins.RemoveOptions>]
type RecordType =
    {
        /// A thing!
        A : int option
        /// Another thing!
        B : string
        /// Yet another thing!
        C : float list
    }

    static member DefaultA () : int = 3
