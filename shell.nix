with import ./default.nix {};
let
  run-ormolu = pkgs.callPackage ./nix/run-ormolu.nix {};
  refreshScript = pkgs.writeShellScriptBin "ref"
    ''
    hpack .
    cabal new-build
    '';
  ghcidScript = pkgs.writeShellScriptBin "dev"
    ''
    hpack .
    cabal new-build
    ghcid --command 'cabal new-repl lib:codata' --allow-eval --warnings -o ghcid.txt
    '';
  ghcidTestScript = pkgs.writeShellScriptBin "dev-test"
    ''
    hpack .
    cabal new-build
    ghcid --command 'cabal new-repl test:test' --allow-eval --warnings -o ghcid.txt
    '';
  runScript = pkgs.writeShellScriptBin "run" "cabal run exe:demo";
  formatScript = pkgs.writeShellScriptBin "format" "${run-ormolu}/bin/run-ormolu inplace $(find . -name '*.hs' ! -path '**/dist-newstyle/**')";

in hsPkgs.shellFor {
    packages = myHsPkgs: [
      myHsPkgs.codata
    ];
    # withHoogle = true;
    buildInputs = with pkgs; [
      cabal-install # Cabal, haskell build tool
      cabal2nix # Utility to download Haskell packages into Nix format
      haskell-language-server # Language server
      hsPkgs.ghcid # Haskell repl with hot reloading
      hsPkgs.hpack # Generate cabal file from package.yaml
      hsPkgs.ormolu # Formatter
      hsPkgs.hlint # Linter

      # Scripts
      refreshScript
      ghcidScript
      ghcidTestScript
      runScript
      formatScript
    ];
}

