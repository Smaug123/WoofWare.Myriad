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

    // Note: this file is preserved as an example of writing an identity cata.
    // Don't add anything else to this file, because that will muddy the example.

    [<Test>]
    let ``Cata works`` () =
        let property (x : FileSystemItem) =
            FileSystemItemCata.runFileSystemItem idCata x = x

        Check.QuickThrowOnFailure property

// Note: this file is preserved as an example of writing an identity cata.
// Don't add anything else to this file, because that will muddy the example.
