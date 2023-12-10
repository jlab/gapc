{
  description = "Bellmans Gapc in Nix Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages = rec {
          default = let
            version = "2023.07.05";
          in
            pkgs.stdenv.mkDerivation {
              name = "gapc-${version}";

              src = pkgs.fetchFromGitHub {
                owner = "jlab";
                repo = "gapc";
                rev = "${version}";
                sha256 = "sha256-OzGf8Z4BzHEPmonlOvvvg5G1y1mtrkWSxOHPfBJU7kU=";
              };

              buildInputs = with pkgs; [
                gnumake
                flex
                bison
                gsl
                boost
                mercurial
              ];

              configureFlags = ["--with-boost-libdir=${pkgs.boost.out}/lib"];
            };
        };
      }
    );
}
