namespace ConsumePlugin

open WoofWare.Myriad.Plugins

[<CreateCatamorphism "MyListCata">]
type MyList<'a> =
    | Nil
    | Cons of ConsCase<'a>

and ConsCase<'a> =
    {
        Head : 'a
        Tail : MyList<'a>
    }

[<CreateCatamorphism "MyList2Cata">]
type MyList2<'a> =
    | Nil
    | Cons of 'a * MyList2<'a>
