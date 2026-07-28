namespace ConsumePlugin

open WoofWare.Myriad.Plugins

/// A data-free union used as a `Map` key. No case name contains a `:`, so with `:` as the
/// key-value separator every `Map<Severity, _>` is expressible on a command line.
type Severity =
    | Low
    | High

/// `Map` fields accumulate across occurrences, like lists do, but each occurrence carries
/// key-value entries rather than bare values.
[<ArgParser>]
type MapArgs =
    {
        /// No entry separator, so each occurrence carries exactly one entry. The entry splits at
        /// the *first* `:`, so a value may contain `:` (and anything else) while a key may not.
        [<ArgumentKeyValueSeparator ':'>]
        [<ArgumentHelpText "Labels to attach">]
        Labels : Map<string, string>

        /// An entry separator lets one occurrence carry several entries, at the cost of
        /// forbidding `,` in keys *and* values.
        [<ArgumentKeyValueSeparator '='>]
        [<ArgumentMapEntrySeparator ','>]
        Env : Map<string, string>

        /// Keys and values are parsed by the same machinery as any other leaf, so they may be
        /// any scalar argument type — here an enumerated value and an int.
        [<ArgumentKeyValueSeparator ':'>]
        Thresholds : Map<Severity, int>

        /// A boolean *value* must not make the field itself boolean-like: an occurrence of a map
        /// always carries an encoded entry, so `--switches` alone is missing its value rather
        /// than meaning "true".
        [<ArgumentKeyValueSeparator ':'>]
        Switches : Map<string, bool>
    }

type DeployArgs =
    {
        [<ArgumentKeyValueSeparator ':'>]
        Tags : Map<string, string>
    }

type RollbackArgs =
    {
        [<ArgumentLongForm "to">]
        Target : string
    }

/// A `Map` inside a union case payload. A `Map` is satisfiable with no arguments (it defaults to
/// empty, as a list does), so `Deploy` is the empty command line's fallback.
[<ArgParser>]
type MapInUnion =
    | Deploy of DeployArgs
    | Rollback of RollbackArgs
