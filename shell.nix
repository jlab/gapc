{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    buildInputs = [
      nixpkgs-fmt
      # (callPackage ./package.nix {})
      gdb
      gcc
      gpp
      lldb
      clang
      boost.dev
      boost.out
      flex
      bison
      bisoncpp
      gsl
      mercurial
    ];

    BOOST_LDFLAGS = "-L${boost}/lib";
    shellHook = ''
      # ./configure --prefix=$(pwd)/debug --with-boost=yes
    '';
  }
