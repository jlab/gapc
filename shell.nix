{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    nativeBuildInputs = [flex bison gsl];
    buildInputs = [
      nixpkgs-fmt
      (callPackage ./package.nix {})
      gdb
      gcc
      gpp
      lldb
      clang
      boost
      flex
      bison
      gsl
      mercurial
    ];

    configureFlags = ["BOOST_LDFLAGS=-L${boost}/lib"];
    shellHook = ''
      ./configure --prefix=$(pwd)/debug --with-boost=yes
    '';
  }
