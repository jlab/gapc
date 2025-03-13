{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    nativeBuildInputs = [flex bison gsl];
    buildInputs = [
      nixpkgs-fmt
      #(callPackage ./package.nix {})
      cpplint
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
      export BOOST_LDFLAGS=-L${boost}/lib
      # ./configure --prefix=$(pwd)/debug --with-boost=yes
    '';
  }
