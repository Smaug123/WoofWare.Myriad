namespace WoofWare.Myriad.Plugins.Test

open NUnit.Framework
open ConsumePlugin
open FsCheck

[<TestFixture>]
module TestDirectory =
    let idCata : FileSystemCata<_> =
        {
            FileSystemItem =
                { new FileSystemItemCataCase<_> with
                    member _.File file = FileSystemItem.File file

                    member _.Directory name dirSize results =
                        FileSystemItem.Directory
                            {
                                Name = name
                                DirSize = dirSize
                                Contents = results
                            }
                }

        }

    [<Test>]
    let ``Cata works`` () =
        let property (x : FileSystemItem) =
            FileSystemItemCata.runFileSystemItem idCata x = x

        Check.QuickThrowOnFailure property
