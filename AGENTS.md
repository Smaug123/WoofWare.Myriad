A set of Myriad plugins.

# Gotchas

A build (including as invoked through `dotnet test`) regenerates the generated files.
We commit those files so that they serve as partial documentation in the GitHub repo, but Myriad's formatting differs from the repo style, so we have to `dotnet fantomas .` after a build to make the files safe to commit.

Versioning is NerdBank.GitVersioning-controlled, independently for the WoofWare.Myriad.Plugins.Attributes project and WoofWare.Myriad.Plugins itself.
The Attributes project is intentionally extremely minimal.
