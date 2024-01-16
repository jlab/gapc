{
  stdenv,
  fetchFromGitHub,
  flex,
  bison,
  boost,
  gsl,
}:
stdenv.mkDerivation {
  name = "gapc-unstable";
  version = "2023-07-05";

  src = fetchFromGitHub {
    owner = "jlab";
    repo = "gapc";
    rev = "2023.07.05";
    sha256 = "sha256-OzGf8Z4BzHEPmonlOvvvg5G1y1mtrkWSxOHPfBJU7kU=";
  };

  nativeBuildInputs = [flex bison gsl];
  buildInputs = [boost];
  configureFlags = ["BOOST_LDFLAGS=-L${boost}/lib"];
}
