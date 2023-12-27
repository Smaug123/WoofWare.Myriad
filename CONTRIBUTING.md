# Contributing

The main project fork lives [on GitHub](https://github.com/Smaug123/WoofWare.Myriad).

Contributions are welcome, but I am generally very opinionated about both style and content.
I also can't commit to looking at anything in a particularly timely manner (or at all, though I expect I will try).

In general my aesthetics lead me to accept correctness fixes much more readily than other changes.

## Issues

Please raise bug reports and feature requests as Issues on [the main GitHub project](https://github.com/Smaug123/WoofWare.Myriad/issues).

## Pull requests

Before embarking on a large change, I strongly recommend checking via a GitHub Issue first that I'm likely to accept it.

You may find that the following guidelines will help you produce a change that I accept:

* Keep your change as small and focused as is practical.
* Ensure that your change is thoroughly tested.
* Document any choices you make which are not immediately obvious.
* Explain why your change is necessary or desirable.

## On your first checkout

There are pull request checks on this repo, enforcing [Fantomas](https://github.com/fsprojects/fantomas/)-compliant formatting according to the [G-Research style guidelines](https://github.com/G-Research/fsharp-formatting-conventions/).
After checking out the repo, you may wish to add a pre-push hook to ensure locally that formatting is complete, rather than having to wait for the CI checks to tell you that you haven't formatted your code.
Consider performing the following command to set this up in the repo:
```bash
git config core.hooksPath hooks/
```
Before your first push (but only once), you will need to install the [.NET local tools](https://docs.microsoft.com/en-us/dotnet/core/tools/local-tools-how-to-use) which form part of the pre-push hook:
```bash
dotnet tool restore
```

In future, some commits (such as big-bang formatting commits) may be recorded for convenience in `.git-blame-ignore-revs`.
Consider performing the following command to have `git blame` ignore these commits, when we ever create any:
```bash
git config blame.ignoreRevsFile .git-blame-ignore-revs
```

## Dependencies

I try to keep this repository's dependencies as few as possible, because (for example) any consumer of the source generator will also consume this project via the attributes.
When adding dependencies, you will need to `nix run .#fetchDeps` to obtain a new copy of [the dependency lockfile](./nix/deps.nix).

## Branch strategy

Releases are made from the `main` branch.

## License

This project is licensed with the MIT license, a copy of which you can find at the repository root.
