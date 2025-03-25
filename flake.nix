{
  description = "Bellman's GAP compiler";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    systems,
    nixpkgs,
    nixpkgs-unstable,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem
    (system: let 
      pkgs = import nixpkgs {
      inherit system;
      overlays = [
        # make unstable packages available via overlay
        (final: prev: {
          unstable = nixpkgs-unstable.legacyPackages.${prev.system};
        })
      ];
    }; 
    in {
      packages.default = pkgs.callPackage ./package.nix {};
      devShells.default = import ./shell.nix {inherit pkgs;};
    });
}
