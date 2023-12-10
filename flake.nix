{
  description = "Bellmans Gapc in Nix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    # Systems supported
    allSystems = [
      "x86_64-linux" # 64-bit Intel/AMD Linux
      "aarch64-linux" # 64-bit ARM Linux
    ];

    # Helper to provide system-specific attributes
    forAllSystems = f:
      nixpkgs.lib.genAttrs allSystems (system:
        f {
          pkgs = import nixpkgs {inherit system;};
        });
  in {
    packages = forAllSystems ({pkgs}: {
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
    });
  };
}
