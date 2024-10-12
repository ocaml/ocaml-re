{
  description = "ocaml-re flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nix-ocaml/nix-overlays";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";
        extraBuildInputs = [ pkgs.ocamlPackages.core_bench ];
        checkInputs = with pkgs.ocamlPackages; [
          ounit
          js_of_ocaml
          ppx_expect
          pkgs.nodejs-slim
        ];
        devInputs = with pkgs.ocamlPackages; [
          ocaml-lsp
          pkgs.ocamlformat_0_26_2
        ];
        inherit (pkgs.ocamlPackages) buildDunePackage;
      in rec {
        packages = rec {
          default = re;
          re = buildDunePackage {
            pname = "re";
            version = "n/a";
            src = ./.;
            duneVersion = "3";
            propagatedBuildInputs = with pkgs.ocamlPackages; [ seq ];
            # Other check deps depend on re itself
            checkInputs = with pkgs.ocamlPackages; [ ounit ];
            doCheck = true;
          };
        };
        devShells.test = pkgs.mkShell {
          inputsFrom = pkgs.lib.attrValues packages;
          buildInputs = extraBuildInputs ++ checkInputs;
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = pkgs.lib.attrValues packages;
          buildInputs = extraBuildInputs ++ devInputs;
        };
      });
}
