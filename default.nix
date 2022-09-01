{ compiler ? "ghc924" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "kademlia" = hself.callCabal2nix "kademlia" (gitignore ./.) { };
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [ p."kademlia" ];
    buildInputs = [
      myHaskellPackages.haskell-language-server
      pkgs.haskellPackages.cabal-fmt
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.hasktags
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.ormolu
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."kademlia");

  docker = pkgs.dockerTools.buildImage {
    name = "kademlia";
    config.Cmd = [ "${exe}/bin/kademlia" ];
  };
in {
  inherit shell;
  inherit exe;
  inherit docker;
  inherit myHaskellPackages;
  "kademlia" = myHaskellPackages."kademlia";
}
