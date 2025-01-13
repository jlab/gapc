{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    buildInputs = [
      nixpkgs-fmt
      time
      cmake

      llvmPackages.openmp
      llvmPackages.lldb
      llvmPackages.clang

      # (callPackage ./package.nix {})
      gdb
      gcc
      gpp
      boost.dev
      boost.out
      flex
      bison
      bisoncpp
      gsl
      mercurial
      valgrind
    ];

    CXX = "clang++";
    CC = "clang";
    OMP_NUM_THREADS=8;

    BOOST_LDFLAGS = "-L${boost}/lib";
    shellHook = ''
      export CXX=clang++;
      export CC=clang
      # ./configure --prefix=$(pwd)/debug --with-boost=yes
    '';
  }
