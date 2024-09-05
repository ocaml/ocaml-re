{
  description = "ocaml-re flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nix-ocaml/nix-overlays";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        extraBuildInputs = pkgs:
          with pkgs.ocamlPackages; [
            core_bench
            memtrace
          ];
        checkInputs = pkgs:
          with pkgs.ocamlPackages; [
            ounit
            js_of_ocaml
            ppx_expect
            pkgs.nodejs-slim
          ];
        devInputs = pkgs:
          with pkgs.ocamlPackages; [
            ocaml-lsp
            pkgs.ocamlformat_0_26_2
            csv
            pkgs.tabview
          ];
        makePackages = pkgs: rec {
          default = re;
          re = pkgs.ocamlPackages.buildDunePackage {
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
        ocamlVersionOverlay =
          (ocaml: self: super: { ocamlPackages = ocaml super.ocaml-ng; });
        framePointers = ocaml: ocaml.override { framePointerSupport = true; };
        framePointersOverlay = self: super: {
          ocamlPackages = super.ocamlPackages.overrideScope
            (oself: osuper: { ocaml = framePointers osuper.ocaml; });
        };
        makeNixpkgs = ocaml:
          nixpkgs.legacyPackages.${system}.appendOverlays
          [ (ocamlVersionOverlay ocaml) ];
      in rec {
        devShells.test = let
          pkgs = makeNixpkgs (ocaml: ocaml.ocamlPackages_5_2);
          packages = makePackages pkgs;
        in pkgs.mkShell {
          inputsFrom = pkgs.lib.attrValues packages;
          buildInputs = extraBuildInputs pkgs ++ checkInputs pkgs;
        };
        devShells.default = let
          pkgs = makeNixpkgs (ocaml: ocaml.ocamlPackages_5_2);
          packages = makePackages pkgs;
        in pkgs.mkShell {
          inputsFrom = pkgs.lib.attrValues packages;
          buildInputs = extraBuildInputs pkgs ++ devInputs pkgs
            ++ checkInputs pkgs;
        };
        devShells.fp = let
          pkgs = (makeNixpkgs (ocaml: ocaml.ocamlPackages_5_2)).appendOverlays
            [ framePointersOverlay ];
          packages = makePackages pkgs;
        in pkgs.mkShell {
          inputsFrom = pkgs.lib.attrValues packages;
          buildInputs = extraBuildInputs pkgs ++ devInputs pkgs;
        };
        devShells.memtrace = let
          pkgs = makeNixpkgs (ocaml: ocaml.ocamlPackages_4_14);
          packages = makePackages pkgs;
        in pkgs.mkShell {
          inputsFrom = pkgs.lib.attrValues packages;
          buildInputs = extraBuildInputs pkgs
            ++ [ pkgs.ocamlPackages.memtrace_viewer ];
        };
      });
}
