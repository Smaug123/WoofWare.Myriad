namespace UsePlugin

/// My whatnot
[<MyriadPlugin.RemoveOptions>]
type RecordType =
    {
        /// A thing!
        A : int option
        /// Another thing!
        B : string
        /// Yet another thing!
        C : float list
    }
