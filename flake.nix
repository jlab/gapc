{
  packages = forAllSystems ({pkgs}: {
    default = pkgs.stdenv.mkDerivation {
      name = "gapc-${version}";
      version = "2023.07.05";

      src = fetchFromGitHub {
        owner = "jlab";
        repo = "gapc";
        rev = "${version}";
        sha256 = "sha256-OzGf8Z4BzHEPmonlOvvvg5G1y1mtrkWSxOHPfBJU7kU=";
      };

      buildInputs = [
        gnumake
        flex
        bison
        boost
        mercurial
        gsl
      ];

      configureFlags = ["--with-boost-libdir=${pkgs.boost.out}/lib"];
    };
  });
}
