let
  nixpkgs = import <nixpkgs> {};
in
with nixpkgs;
stdenv.mkDerivation {
  name = "sbcl";
  buildInputs = [ sbcl ];
}
