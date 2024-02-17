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
    | File of File
    | Directory of Directory
