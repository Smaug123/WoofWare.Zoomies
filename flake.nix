{
  description = "Jane Street's incremental reactive programming system Bonsai, but in F#";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      pname = "WoofWare.Zoomies";
      dotnet-sdk = pkgs.dotnetCorePackages.sdk_9_0;
      dotnet-runtime = pkgs.dotnetCorePackages.runtime_9_0;
      version = "0.1";
      dotnetTool = dllOverride: toolName: toolVersion: hash:
        pkgs.stdenvNoCC.mkDerivation rec {
          name = toolName;
          version = toolVersion;
          nativeBuildInputs = [pkgs.makeWrapper];
          src = pkgs.fetchNuGet {
            pname = name;
            version = version;
            hash = hash;
            installPhase = ''mkdir -p $out/bin && cp -r tools/net*/any/* $out/bin'';
          };
          installPhase = let
            dll =
              if isNull dllOverride
              then name
              else dllOverride;
          in
            # fsharp-analyzers requires the .NET SDK at runtime, so we use that instead of dotnet-runtime.
            ''
              runHook preInstall
              mkdir -p "$out/lib"
              cp -r ./bin/* "$out/lib"
              makeWrapper "${dotnet-sdk}/bin/dotnet" "$out/bin/${name}" --set DOTNET_HOST_PATH "${dotnet-sdk}/bin/dotnet" --add-flags "$out/lib/${dll}.dll"
              runHook postInstall
            '';
        };
    in {
      packages = let
        deps = builtins.fromJSON (builtins.readFile ./nix/deps.json);
      in {
        fantomas = dotnetTool null "fantomas" (builtins.fromJSON (builtins.readFile ./.config/dotnet-tools.json)).tools.fantomas.version (builtins.head (builtins.filter (elem: elem.pname == "fantomas") deps)).hash;
        fsharp-analyzers = dotnetTool "FSharp.Analyzers.Cli" "fsharp-analyzers" (builtins.fromJSON (builtins.readFile ./.config/dotnet-tools.json)).tools.fsharp-analyzers.version (builtins.head (builtins.filter (elem: elem.pname == "fsharp-analyzers") deps)).hash;
        default = pkgs.buildDotnetModule {
          inherit pname version dotnet-sdk dotnet-runtime;
          name = "WoofWare.Zoomies";
          src = ./.;
          projectFile = "./WoofWare.Zoomies/WoofWare.Zoomies.fsproj";
          testProjectFile = "./WoofWare.Zoomies.Test/WoofWare.Zoomies.Test.fsproj";
          nugetDeps = ./nix/deps.json; # `nix build .#default.fetch-deps && ./result nix/deps.json`
          doCheck = true;
        };
      };
      devShell = pkgs.mkShell {
        buildInputs = [dotnet-sdk];
        DOTNET_CLI_TELEMETRY_OPTOUT = "1";
        packages = [
          pkgs.alejandra
          pkgs.nodePackages.markdown-link-check
          pkgs.shellcheck
          pkgs.xmlstarlet
          pkgs.claude-code
        ];
      };
    });
}
