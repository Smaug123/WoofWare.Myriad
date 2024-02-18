//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------





namespace ConsumePlugin

open WoofWare.Myriad.Plugins

/// Description of how to combine cases during a fold
type FileSystemItemCataCase<'FileSystemItem> =
    /// How to operate on the Directory case
    abstract Directory : name : string -> dirSize : int -> contents : 'FileSystemItem list -> 'FileSystemItem
    /// How to operate on the File case
    abstract File : File -> 'FileSystemItem

/// Specifies how to perform a fold (catamorphism) over the type FileSystemItem and its friends.
type FileSystemCata<'FileSystemItem> =
    {
        /// How to perform a fold (catamorphism) over the type FileSystemItem
        FileSystemItem : FileSystemItemCataCase<'FileSystemItem>
    }

/// Methods to perform a catamorphism over the type FileSystemItem
[<RequireQualifiedAccess>]
module FileSystemItemCata =
    [<RequireQualifiedAccess>]
    type private Instruction =
        | Process__FileSystemItem of FileSystemItem
        | FileSystemItem_Directory of string * int * int

    let private loop (cata : FileSystemCata<_>) (instructions : ResizeArray<Instruction>) =
        let fileSystemItemStack = ResizeArray ()

        while instructions.Count > 0 do
            let currentInstruction = instructions.[instructions.Count - 1]
            instructions.RemoveAt (instructions.Count - 1)

            match currentInstruction with
            | Instruction.Process__FileSystemItem x ->
                match x with
                | FileSystemItem.Directory ({
                                                Name = name
                                                DirSize = dirSize
                                                Contents = contents
                                            }) ->
                    instructions.Add (Instruction.FileSystemItem_Directory (name, dirSize, (List.length contents)))

                    for elt in contents do
                        instructions.Add (Instruction.Process__FileSystemItem elt)
                | FileSystemItem.File (arg0_0) -> cata.FileSystemItem.File arg0_0 |> fileSystemItemStack.Add
            | Instruction.FileSystemItem_Directory (name, dirSize, contents) ->
                let contents_len = contents

                let contents =
                    seq {
                        for i = fileSystemItemStack.Count - 1 downto fileSystemItemStack.Count - contents do
                            yield fileSystemItemStack.[i]
                    }
                    |> Seq.toList

                fileSystemItemStack.RemoveRange (fileSystemItemStack.Count - contents_len, contents_len)
                cata.FileSystemItem.Directory name dirSize contents |> fileSystemItemStack.Add

        fileSystemItemStack

    /// Execute the catamorphism.
    let runFileSystemItem (cata : FileSystemCata<'FileSystemItemRet>) (x : FileSystemItem) : 'FileSystemItemRet =
        let instructions = ResizeArray ()
        instructions.Add (Instruction.Process__FileSystemItem x)
        let fileSystemItemRetStack = loop cata instructions
        Seq.exactlyOne fileSystemItemRetStack
namespace ConsumePlugin

open WoofWare.Myriad.Plugins

/// Description of how to combine cases during a fold
type GiftCataCase<'Gift> =
    /// How to operate on the Book case
    abstract Book : Book -> 'Gift
    /// How to operate on the Chocolate case
    abstract Chocolate : Chocolate -> 'Gift
    /// How to operate on the Wrapped case
    abstract Wrapped : 'Gift -> WrappingPaperStyle -> 'Gift
    /// How to operate on the Boxed case
    abstract Boxed : 'Gift -> 'Gift
    /// How to operate on the WithACard case
    abstract WithACard : 'Gift -> message : string -> 'Gift

/// Specifies how to perform a fold (catamorphism) over the type Gift and its friends.
type GiftCata<'Gift> =
    {
        /// How to perform a fold (catamorphism) over the type Gift
        Gift : GiftCataCase<'Gift>
    }

/// Methods to perform a catamorphism over the type Gift
[<RequireQualifiedAccess>]
module GiftCata =
    [<RequireQualifiedAccess>]
    type private Instruction =
        | Process__Gift of Gift
        | Gift_Wrapped of WrappingPaperStyle
        | Gift_Boxed
        | Gift_WithACard of string

    let private loop (cata : GiftCata<_>) (instructions : ResizeArray<Instruction>) =
        let giftStack = ResizeArray ()

        while instructions.Count > 0 do
            let currentInstruction = instructions.[instructions.Count - 1]
            instructions.RemoveAt (instructions.Count - 1)

            match currentInstruction with
            | Instruction.Process__Gift x ->
                match x with
                | Gift.Book (arg0_0) -> cata.Gift.Book arg0_0 |> giftStack.Add
                | Gift.Chocolate (arg0_0) -> cata.Gift.Chocolate arg0_0 |> giftStack.Add
                | Gift.Wrapped (arg0_0, arg1_0) ->
                    instructions.Add (Instruction.Gift_Wrapped (arg1_0))
                    instructions.Add (Instruction.Process__Gift arg0_0)
                | Gift.Boxed (arg0_0) ->
                    instructions.Add Instruction.Gift_Boxed
                    instructions.Add (Instruction.Process__Gift arg0_0)
                | Gift.WithACard (arg0_0, message) ->
                    instructions.Add (Instruction.Gift_WithACard (message))
                    instructions.Add (Instruction.Process__Gift arg0_0)
            | Instruction.Gift_Wrapped (arg1_0) ->
                let arg0_0 = giftStack.[giftStack.Count - 1]
                giftStack.RemoveAt (giftStack.Count - 1)
                cata.Gift.Wrapped arg0_0 arg1_0 |> giftStack.Add
            | Instruction.Gift_Boxed ->
                let arg0_0 = giftStack.[giftStack.Count - 1]
                giftStack.RemoveAt (giftStack.Count - 1)
                cata.Gift.Boxed arg0_0 |> giftStack.Add
            | Instruction.Gift_WithACard (message) ->
                let arg0_0 = giftStack.[giftStack.Count - 1]
                giftStack.RemoveAt (giftStack.Count - 1)
                cata.Gift.WithACard arg0_0 message |> giftStack.Add

        giftStack

    /// Execute the catamorphism.
    let runGift (cata : GiftCata<'GiftRet>) (x : Gift) : 'GiftRet =
        let instructions = ResizeArray ()
        instructions.Add (Instruction.Process__Gift x)
        let giftRetStack = loop cata instructions
        Seq.exactlyOne giftRetStack
