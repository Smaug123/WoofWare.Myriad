namespace ConsumePlugin

open WoofWare.Myriad.Plugins

/// Description of how to combine cases during a fold
type MyListCataCase<'a, 'MyList> =
    /// How to operate on the Nil case
    abstract Nil : 'MyList
    /// How to operate on the Cons case
    abstract Cons : head : 'a -> tail : 'MyList -> 'MyList

/// Specifies how to perform a fold (catamorphism) over the type MyList and its friends.
type MyListCata<'a, 'MyList> =
    {
        /// How to perform a fold (catamorphism) over the type MyList
        MyList : MyListCataCase<'a, 'MyList>
    }

/// Methods to perform a catamorphism over the type MyList
[<RequireQualifiedAccess>]
module MyListCata =
    [<RequireQualifiedAccess>]
    type private Instruction<'a> =
        | Process__MyList of MyList<'a>
        | MyList_Cons of head : 'a

    let private loop (cata : MyListCata<'a, 'MyList>) (instructions : ResizeArray<Instruction<'a>>) =
        let myListStack = ResizeArray<'MyList> ()

        while instructions.Count > 0 do
            let currentInstruction = instructions.[instructions.Count - 1]
            instructions.RemoveAt (instructions.Count - 1)

            match currentInstruction with
            | Instruction.Process__MyList x ->
                match x with
                | MyList.Nil -> cata.MyList.Nil |> myListStack.Add
                | MyList.Cons ({
                                   Head = head
                                   Tail = tail
                               }) ->
                    instructions.Add (Instruction.MyList_Cons (head))
                    instructions.Add (Instruction.Process__MyList tail)
            | Instruction.MyList_Cons head ->
                let tail = myListStack.[myListStack.Count - 1]
                myListStack.RemoveAt (myListStack.Count - 1)
                cata.MyList.Cons head tail |> myListStack.Add

        myListStack

    /// Execute the catamorphism.
    let runMyList (cata : MyListCata<'a, 'MyListRet>) (x : MyList<'a>) : 'MyListRet =
        let instructions = ResizeArray ()
        instructions.Add (Instruction.Process__MyList x)
        let myListRetStack = loop cata instructions
        Seq.exactlyOne myListRetStack
namespace ConsumePlugin

open WoofWare.Myriad.Plugins

/// Description of how to combine cases during a fold
type MyList2CataCase<'a, 'MyList2> =
    /// How to operate on the Nil case
    abstract Nil : 'MyList2
    /// How to operate on the Cons case
    abstract Cons : 'a -> 'MyList2 -> 'MyList2

/// Specifies how to perform a fold (catamorphism) over the type MyList2 and its friends.
type MyList2Cata<'a, 'MyList2> =
    {
        /// How to perform a fold (catamorphism) over the type MyList2
        MyList2 : MyList2CataCase<'a, 'MyList2>
    }

/// Methods to perform a catamorphism over the type MyList2
[<RequireQualifiedAccess>]
module MyList2Cata =
    [<RequireQualifiedAccess>]
    type private Instruction<'a> =
        | Process__MyList2 of MyList2<'a>
        | MyList2_Cons of 'a

    let private loop (cata : MyList2Cata<'a, 'MyList2>) (instructions : ResizeArray<Instruction<'a>>) =
        let myList2Stack = ResizeArray<'MyList2> ()

        while instructions.Count > 0 do
            let currentInstruction = instructions.[instructions.Count - 1]
            instructions.RemoveAt (instructions.Count - 1)

            match currentInstruction with
            | Instruction.Process__MyList2 x ->
                match x with
                | MyList2.Nil -> cata.MyList2.Nil |> myList2Stack.Add
                | MyList2.Cons (arg0_0, arg1_0) ->
                    instructions.Add (Instruction.MyList2_Cons (arg0_0))
                    instructions.Add (Instruction.Process__MyList2 arg1_0)
            | Instruction.MyList2_Cons arg0_0 ->
                let arg1_0 = myList2Stack.[myList2Stack.Count - 1]
                myList2Stack.RemoveAt (myList2Stack.Count - 1)
                cata.MyList2.Cons arg0_0 arg1_0 |> myList2Stack.Add

        myList2Stack

    /// Execute the catamorphism.
    let runMyList2 (cata : MyList2Cata<'a, 'MyList2Ret>) (x : MyList2<'a>) : 'MyList2Ret =
        let instructions = ResizeArray ()
        instructions.Add (Instruction.Process__MyList2 x)
        let myList2RetStack = loop cata instructions
        Seq.exactlyOne myList2RetStack
