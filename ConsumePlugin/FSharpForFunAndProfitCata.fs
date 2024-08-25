namespace ConsumePlugin

open WoofWare.Myriad.Plugins

type File =
    {
        Name : string
        FileSize : int
    }

type Directory =
    {
        Name : string
        DirSize : int
        Contents : FileSystemItem list
    }

and [<CreateCatamorphism "FileSystemCata">] FileSystemItem =
    | Directory of Directory
    | File of File

type Book =
    {
        title : string
        price : decimal
    }

type ChocolateType =
    | Dark
    | Milk
    | SeventyPercent

    override this.ToString () =
        match this with
        | ChocolateType.Dark -> "Dark"
        | ChocolateType.Milk -> "Milk"
        | ChocolateType.SeventyPercent -> "SeventyPercent"

type Chocolate =
    {
        chocType : ChocolateType
        price : decimal
    }

    override this.ToString () = this.chocType.ToString ()

type WrappingPaperStyle =
    | HappyBirthday
    | HappyHolidays
    | SolidColor

    override this.ToString () =
        match this with
        | WrappingPaperStyle.HappyBirthday -> "HappyBirthday"
        | WrappingPaperStyle.HappyHolidays -> "HappyHolidays"
        | WrappingPaperStyle.SolidColor -> "SolidColor"

[<CreateCatamorphism "GiftCata">]
type Gift =
    | Book of Book
    | Chocolate of Chocolate
    | Wrapped of Gift * WrappingPaperStyle
    | Boxed of Gift
    | WithACard of Gift * message : string
