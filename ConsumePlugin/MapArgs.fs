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

/// A flag-valued union: its values are spelled `true` and `false`, so help text must advertise
/// `bool` rather than the union's name.
type Enabled =
    | [<ArgumentFlag true>] Yes
    | [<ArgumentFlag false>] No

/// An enumerated type with a case containing a cased letter, so that a cased letter can be used
/// as a separator without making the case unspellable.
type Alpha =
    | Apple
    | Pear

/// Help text must describe each side of a map in the syntax that side actually accepts, and a
/// cased separator must not be mistaken for one which makes a case unrepresentable.
[<ArgParser>]
type MapDisplayArgs =
    {
        /// An enumerated key and a flag-valued value: `--features=Low:true`.
        [<ArgumentKeyValueSeparator ':'>]
        Features : Map<Severity, Enabled>

        /// `Apple` contains an `A`, but enumerated values are matched case-insensitively, so the
        /// key may be spelled `apple` — which avoids the separator. Every `Map<Alpha, _>` is
        /// therefore expressible, e.g. `--casing=appleAvalue`.
        [<ArgumentKeyValueSeparator 'A'>]
        Casing : Map<Alpha, string>
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
