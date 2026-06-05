![example branch parameter](https://github.com/jlab/gapc/actions/workflows/c-cpp.yml/badge.svg)
[![Anaconda-Server Badge](https://anaconda.org/bioconda/bellmans-gapc/badges/version.svg)](https://anaconda.org/bioconda/bellmans-gapc)
<a href="https://launchpad.net/~janssenlab/+archive/ubuntu/software" style="border: none;"><img src="http://media.launchpad.net/lp-badge-kit/launchpad-badge-w160px.png" alt="Launchpad logo" style="border: none;" height="20px;"/></a>

```
  ____       _ _                       _        _____          _____
 |  _ \     | | |                     ( )      / ____|   /\   |  __ \
 | |_) | ___| | |_ __ ___   __ _ _ __ |/ ___  | |  __   /  \  | |__) |
 |  _ < / _ \ | | '_ ` _ \ / _` | '_ \  / __| | | |_ | / /\ \ |  ___/
 | |_) |  __/ | | | | | | | (_| | | | | \__ \ | |__| |/ ____ \| |
 |____/ \___|_|_|_| |_| |_|\__,_|_| |_| |___/  \_____/_/    \_\_|
```

## Dependencies

Bellman's GAP was tested on the following dependencies.

compile time:

- C++ compiler (GCC g++ for example)
- C compiler (GCC for example)
- Flex >= 2.5.34
- GNU bison >= 2.4.1
- GNU make >= 3.81
- Mercurial >= 0.9.5
- boost >= 1.36 (1.34 without the Accumulators Framework)
  - unittest framework (libboost-test-dev)
  - pool
  - program options (libboost-program-options-dev)
  - cstdint
  - the accumulator framework (with -DSTATS)
- ksh93 - or - bash >= 2.03.0(1) (only needed for test scripts)

runtime:

- boost >= 1.36 (1.34 without the Accumulators Framework)
  - pool
  - program options
  - cstdint
  - the accumulator framework (with -DSTATS)

## SOURCES

Always get the latest sources from github:
`git clone https://github.com/jlab/gapc.git`

## INSTALLATION

### from source

To install Bellman's GAP from source call:

1. `./configure --prefix=<install-path>`
2. `make`
3. `make install`

If `--prefix` is not set the path defaults to `/usr/local`

More options for `./configure` are:

```
CXX=<g++ path>
CC=<gcc path>
SED=<sed path>
FLEX=<flex path>
BISON=<bison path>
--with-boost=<path to booth installation>
--with-boost-program-options=<path to boost library program options> and --with-boost-unit-test-framework=<path to boost library unit test>
```

### conda

You can find [conda](https://bioconda.github.io/user/install.html) packages for linux-64 and osx-64 in the [bioconda channel: https://anaconda.org/bioconda/bellmans-gapc](https://anaconda.org/bioconda/bellmans-gapc) for easy installation into your conda environment:

```
conda install -c bioconda bellmans-gapc
```

### Ubuntu

Bellman's GAP is available as a pre-compiled Debian package via Ubuntus launchpad system, a Ubuntu Personal Package Archive (PPA).
Packages for Ubuntu versions 16.04 (Xenial) and newer can be obtained from janssenlab/software.
On your command line, execute the three following commands

```
sudo add-apt-repository ppa:janssenlab/software
sudo apt-get update
sudo apt-get install bellmansgapc
```

For older Ubuntu releases, have a look at bibi-help/bibitools (Ubuntu versions 10.04, 11.10, 12.04, 12.10, 13.04, 13.10, 14.04, 14.10, 15.04)

```
sudo add-apt-repository ppa:bibi-help/bibitools
sudo apt-get update
sudo apt-get install bellmansgapc
```

### Nix

Bellman's GAP is available as a flake the current version of the build can be installed by executing the following command:

```
nix build github:jlab/gapc
```

### MacPorts

Under Mac OS X you may want to use MacPorts to install the compiler. There is a (most likely outdated) ports description on the BiBiServ MacPorts repository. You can install GAP-C via:

`$ sudo port install http://bibiserv.techfak.uni-bielefeld.de/resources/macports/ports/lang/gapc.tgz`
