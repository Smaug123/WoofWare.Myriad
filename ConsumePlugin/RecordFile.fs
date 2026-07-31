namespace ConsumePlugin

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
        /// A field whose name needs backticks, and which is optional, so the generator has to
        /// reconstruct the `Default`-prefixed member name to supply its default.
        ``d thing`` : int option
        /// A field whose name needs backticks but which is not optional, so it exercises the
        /// declaration and accessor sites rather than the default-member one.
        ``e thing`` : string
    }

    static member DefaultA () : int = 3

    static member ``Defaultd thing`` () : int = 5
