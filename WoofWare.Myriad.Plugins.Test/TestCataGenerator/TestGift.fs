namespace WoofWare.Myriad.Plugins.Test

open NUnit.Framework
open ConsumePlugin
open FsCheck
open FsUnitTyped

[<TestFixture>]
module TestGift =

    let idCata : GiftCata<_> =
        {
            Gift =
                { new GiftCataCase<_> with
                    member _.Book b = Gift.Book b
                    member _.Boxed g = Gift.Boxed g
                    member _.Chocolate g = Gift.Chocolate g
                    member _.WithACard g message = Gift.WithACard (g, message)
                    member _.Wrapped g paper = Gift.Wrapped (g, paper)
                }
        }

    let totalCostCata : GiftCata<_> =
        {
            Gift =
                { new GiftCataCase<_> with
                    member _.Book b = b.price
                    member _.Boxed g = g + 1.0m
                    member _.Chocolate c = c.price
                    member _.WithACard g message = g + 2.0m
                    member _.Wrapped g paper = g + 0.5m
                }
        }

    let descriptionCata : GiftCata<_> =
        {
            Gift =
                { new GiftCataCase<_> with
                    member _.Book b = b.title
                    member _.Boxed g = $"%s{g} in a box"
                    member _.Chocolate c = $"%O{c} chocolate"

                    member _.WithACard g message =
                        $"%s{g} with a card saying '%s{message}'"

                    member _.Wrapped g paper = $"%s{g} wrapped in %A{paper} paper"
                }
        }

    [<Test>]
    let ``Cata works`` () =
        let property (x : Gift) = GiftCata.runGift idCata x = x

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``Example from docs`` () =
        let wolfHall =
            {
                title = "Wolf Hall"
                price = 20m
            }

        let yummyChoc =
            {
                chocType = SeventyPercent
                price = 5m
            }

        let birthdayPresent =
            WithACard (Wrapped (Book wolfHall, HappyBirthday), "Happy Birthday")

        let christmasPresent = Wrapped (Boxed (Chocolate yummyChoc), HappyHolidays)

        GiftCata.runGift totalCostCata birthdayPresent |> shouldEqual 22.5m

        GiftCata.runGift descriptionCata christmasPresent
        |> shouldEqual "SeventyPercent chocolate in a box wrapped in HappyHolidays paper"
