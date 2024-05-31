namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text.Range
open Fantomas.FCS.Xml

[<RequireQualifiedAccess>]
module internal SynMemberDefn =
    let private interfaceMemberSlotFlags =
        {
            SynMemberFlags.IsInstance = true
            SynMemberFlags.IsDispatchSlot = true
            SynMemberFlags.IsOverrideOrExplicitImpl = false
            SynMemberFlags.IsFinal = false
            SynMemberFlags.GetterOrSetterIsCompilerGenerated = false
            SynMemberFlags.MemberKind = SynMemberKind.Member
        }


    let abstractMember
        (ident : SynIdent)
        (typars : SynTyparDecls option)
        (arity : SynValInfo)
        (xmlDoc : PreXmlDoc)
        (returnType : SynType)
        : SynMemberDefn
        =
        let slot =
            SynValSig.SynValSig (
                [],
                ident,
                SynValTyparDecls.SynValTyparDecls (typars, true),
                returnType,
                arity,
                false,
                false,
                xmlDoc,
                None,
                None,
                range0,
                {
                    EqualsRange = None
                    WithKeyword = None
                    InlineKeyword = None
                    LeadingKeyword = SynLeadingKeyword.Abstract range0
                }
            )

        SynMemberDefn.AbstractSlot (
            slot,
            interfaceMemberSlotFlags,
            range0,
            {
                GetSetKeywords = None
            }
        )

    let staticMember (binding : SynBinding) : SynMemberDefn =
        let binding = SynBinding.makeStaticMember binding
        SynMemberDefn.Member (binding, range0)
