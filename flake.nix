{
  description = "Myriad plugins to help with argument parsing";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      pname = "WoofWare.Myriad.Plugins";
      dotnet-sdk = pkgs.dotnet-sdk_8;
      dotnet-runtime = pkgs.dotnetCorePackages.runtime_8_0;
      version = "0.1";
      dotnetTool = dllOverride: toolName: toolVersion: sha256:
        pkgs.stdenvNoCC.mkDerivation rec {
          name = toolName;
          version = toolVersion;
          nativeBuildInputs = [pkgs.makeWrapper];
          src = pkgs.fetchNuGet {
            pname = name;
            version = version;
            sha256 = sha256;
            installPhase = ''mkdir -p $out/bin && cp -r tools/net6.0/any/* $out/bin'';
          };
          installPhase = let
            dll =
              if dllOverride == null
              then name
              else dllOverride;
          in ''
            runHook preInstall
            mkdir -p "$out/lib"
            cp -r ./bin/* "$out/lib"
            makeWrapper "${dotnet-runtime}/bin/dotnet" "$out/bin/${name}" --add-flags "$out/lib/${dllOverride}.dll"
            runHook postInstall
          '';
        };
    in {
      packages = {
        fantomas = dotnetTool null "fantomas" (builtins.fromJSON (builtins.readFile ./.config/dotnet-tools.json)).tools.fantomas.version "sha256-Jmo7s8JMdQ8SxvNvPnryfE7n24mIgKi5cbgNwcQw3yU=";
        fsharp-analyzers = dotnetTool "FSharp.Analyzers.Cli" "fsharp-analyzers" (builtins.fromJSON (builtins.readFile ./.config/dotnet-tools.json)).tools.fsharp-analyzers.version "sha256-yGc4oV1fEAfeWUf/y60VUeps3lJWnvVjomWFeVEyXVE=";
        fetchDeps = let
          flags = [];
          runtimeIds = ["win-x64"] ++ map (system: pkgs.dotnetCorePackages.systemToDotnetRid system) dotnet-sdk.meta.platforms;
        in
          pkgs.writeShellScriptBin "fetch-${pname}-deps" (builtins.readFile (pkgs.substituteAll {
            src = ./nix/fetchDeps.sh;
            pname = pname;
            binPath = pkgs.lib.makeBinPath [pkgs.coreutils dotnet-sdk (pkgs.nuget-to-nix.override {inherit dotnet-sdk;})];
            projectFiles = toString ["./WoofWare.Myriad.Plugins/WoofWare.Myriad.Plugins.fsproj" "./ConsumePlugin/ConsumePlugin.fsproj"];
            testProjectFiles = ["./WoofWare.Myriad.Plugins.Test/WoofWare.Myriad.Plugins.Test.fsproj"];
            rids = pkgs.lib.concatStringsSep "\" \"" runtimeIds;
            packages = dotnet-sdk.packages;
            storeSrc = pkgs.srcOnly {
              src = ./.;
              pname = pname;
              version = version;
            };
          }));
        default = pkgs.buildDotnetModule {
          pname = pname;
          name = "WoofWare.Myriad.Plugins";
          version = version;
          src = ./.;
          projectFile = "./WoofWare.Myriad.Plugins/WoofWare.Myriad.Plugins.fsproj";
          nugetDeps = ./nix/deps.nix;
          doCheck = true;
          dotnet-sdk = dotnet-sdk;
          dotnet-runtime = dotnet-runtime;
        };
      };
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [
          (with dotnetCorePackages;
            combinePackages [
              dotnet-sdk_8
              dotnetPackages.Nuget
            ])
        ];
        packages = [
          pkgs.alejandra
          pkgs.nodePackages.markdown-link-check
          pkgs.shellcheck
        ];
      };
    });
}
