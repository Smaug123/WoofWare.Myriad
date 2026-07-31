namespace WoofWare.Myriad.Plugins.Test

open System
open NUnit.Framework
open FsUnitTyped
open ConsumePlugin

/// A field whose type is `SomeArgs option` contributes a whole group of arguments which need not
/// be supplied. The group is present exactly when at least one argument beneath it was supplied,
/// which is the same rule by which a union's case is selected; when it is present, its own
/// required arguments are enforced as usual.
[<TestFixture>]
module TestArgParserOptionalGroup =

    let noEnv (_ : string) : string option = None

    [<Test>]
    let ``An unmentioned group is absent`` () =
        ParentRecordOptionalChild.parse' noEnv [ "--and-another=true" ]
        |> shouldEqual
            {
                Child = None
                AndAnother = true
            }

    [<Test>]
    let ``Supplying the group's arguments makes it present`` () =
        ParentRecordOptionalChild.parse' noEnv [ "--and-another=false" ; "--thing1=3" ; "--thing2=hi" ]
        |> shouldEqual
            {
                Child =
                    Some
                        {
                            Thing1 = 3
                            Thing2 = "hi"
                        }
                AndAnother = false
            }

    /// The point of the whole design: touching the group at all commits to it, so the arguments
    /// it did not receive are reported in the ordinary vocabulary rather than being quietly
    /// treated as an absent group.
    [<Test>]
    let ``Supplying part of the group demands the rest of it`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                ParentRecordOptionalChild.parse' noEnv [ "--and-another=true" ; "--thing1=3" ]
                |> ignore<ParentRecordOptionalChild>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
Required argument '--thing2' received no value"""

    /// Absence of the group does not excuse the arguments outside it.
    [<Test>]
    let ``An absent group does not make its siblings optional`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                ParentRecordOptionalChild.parse' noEnv [] |> ignore<ParentRecordOptionalChild>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
Required argument '--and-another' received no value"""

    [<Test>]
    let ``An optional group reads as a group in help text, not as an alternation`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                ParentRecordOptionalChild.parse' noEnv [ "--help" ]
                |> ignore<ParentRecordOptionalChild>
            )

        exc.Message
        |> shouldEqual
            """Help text requested.
Child (optional):
  --thing1  int32
  --thing2  string
--and-another  bool"""

    // A group containing a positional sink. The sink accepts zero tokens, but `Thing1` is
    // required, so the group is still distinguishable from its own absence.

    [<Test>]
    let ``A group containing a positional sink can be absent`` () =
        ParentRecordOptionalChildPos.parse' noEnv []
        |> shouldEqual
            {
                Child = None
            }

    [<Test>]
    let ``A group containing a positional sink can be present`` () =
        ParentRecordOptionalChildPos.parse' noEnv [ "--thing1=3" ; "http://example.com/" ]
        |> shouldEqual
            {
                Child =
                    Some
                        {
                            Thing1 = 3
                            Thing2 = [ Uri "http://example.com/" ]
                        }
            }

    /// A bare positional token is enough to touch the group, exactly as a named argument is: the
    /// sink is reachable only through the group, so consuming a token means the group is present.
    [<Test>]
    let ``A positional token alone selects the group, and its required arguments are then demanded`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                ParentRecordOptionalChildPos.parse' noEnv [ "http://example.com/" ]
                |> ignore<ParentRecordOptionalChildPos>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
Required argument '--thing1' received no value"""

    [<Test>]
    let ``A group's help text annotates the header the field's [<ArgumentHelpText>] provides`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                ParentRecordOptionalChildPos.parse' noEnv [ "--help" ]
                |> ignore<ParentRecordOptionalChildPos>
            )

        exc.Message
        |> shouldEqual
            """Help text requested.
Child (optional): Settings for the child thing
  --thing1  int32
  --thing2  URI (positional args) (can be repeated)"""

    // A union of alternative argument sets is a group like any other, so it too may be optional:
    // the choice among its cases need not be made at all.

    [<Test>]
    let ``An unmentioned union group is absent`` () =
        WithOptionalTransformArgs.parse' noEnv [ "--verbose=true" ]
        |> shouldEqual
            {
                Verbose = true
                Transform = None
            }

    [<Test>]
    let ``Selecting a case of an optional union group makes it present`` () =
        WithOptionalTransformArgs.parse' noEnv [ "--verbose=false" ; "--level=9" ]
        |> shouldEqual
            {
                Verbose = false
                Transform =
                    Some (
                        Transform.Compress
                            {
                                Level = 9
                            }
                    )
            }

    [<Test>]
    let ``The cases of an optional union group remain exclusive`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                WithOptionalTransformArgs.parse' noEnv [ "--verbose=true" ; "--level=9" ; "--recipient=me" ]
                |> ignore<WithOptionalTransformArgs>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
Arguments select more than one alternative: Compress (via --level=9), Encrypt (via --recipient=me)"""

    /// The optional group's own two alternatives are ours rather than the author's, so the help
    /// text must not present them: only the union the author actually wrote is an alternation.
    [<Test>]
    let ``An optional union group nests its own alternation under the group header`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                WithOptionalTransformArgs.parse' noEnv [ "--help" ]
                |> ignore<WithOptionalTransformArgs>
            )

        exc.Message
        |> shouldEqual
            """Help text requested.
--verbose  bool
Transform (optional):
  exactly one of the following sets of arguments:
  Compress:
    --level  int32
  Encrypt:
    --recipient  string"""

    // A defaulted group. Omitting it means a particular value rather than no value, and the
    // Choice reports which happened -- exactly as it does for a defaulted leaf.

    [<Test>]
    let ``An unmentioned defaulted group takes its default`` () =
        ParentRecordDefaultedChild.parse' noEnv [ "--and-another=true" ]
        |> shouldEqual
            {
                Child =
                    Choice2Of2
                        {
                            Thing1 = 42
                            Thing2 = "from the default"
                        }
                AndAnother = true
            }

    [<Test>]
    let ``Supplying a defaulted group's arguments overrides the default wholesale`` () =
        ParentRecordDefaultedChild.parse' noEnv [ "--and-another=true" ; "--thing1=3" ; "--thing2=hi" ]
        |> shouldEqual
            {
                Child =
                    Choice1Of2
                        {
                            Thing1 = 3
                            Thing2 = "hi"
                        }
                AndAnother = true
            }

    /// The default is all-or-nothing: it is not merged field-by-field with what was supplied, so
    /// touching the group still demands the whole of it.
    [<Test>]
    let ``Supplying part of a defaulted group demands the rest rather than defaulting it`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                ParentRecordDefaultedChild.parse' noEnv [ "--and-another=true" ; "--thing1=3" ]
                |> ignore<ParentRecordDefaultedChild>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
Required argument '--thing2' received no value"""

    /// There is no single token which supplies a whole group, so there is nothing to render the
    /// default as; the help says only that one exists.
    [<Test>]
    let ``A defaulted group says a default exists without spelling it`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                ParentRecordDefaultedChild.parse' noEnv [ "--help" ]
                |> ignore<ParentRecordDefaultedChild>
            )

        exc.Message
        |> shouldEqual
            """Help text requested.
Child (optional; a default is used if omitted):
  --thing1  int32
  --thing2  string
--and-another  bool"""

    [<Test>]
    let ``An unmentioned defaulted union group takes its default`` () =
        WithDefaultedTransformArgs.parse' noEnv [ "--verbose=false" ]
        |> shouldEqual
            {
                Verbose = false
                Transform =
                    Choice2Of2 (
                        Transform.Compress
                            {
                                Level = 6
                            }
                    )
            }

    [<Test>]
    let ``Selecting a case of a defaulted union group overrides the default`` () =
        WithDefaultedTransformArgs.parse' noEnv [ "--verbose=false" ; "--recipient=me" ]
        |> shouldEqual
            {
                Verbose = false
                Transform =
                    Choice1Of2 (
                        Transform.Encrypt
                            {
                                Recipient = "me"
                            }
                    )
            }
