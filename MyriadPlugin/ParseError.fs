namespace MyriadPlugin

type ParseError =
    | NoArgs
    | UnrecognisedKey of string
    | MoreThanOnce of string
    | UnfilledValue of string
    | CannotBeEmpty of key : string * disallowedType : string
    | CouldNotParse of key : string * value : string
