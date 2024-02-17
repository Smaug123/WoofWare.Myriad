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
