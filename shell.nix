let nixpkgs = import <nixpkgs> {};
in with nixpkgs; stdenv.mkDerivation {
  name = "common-lisp";
  # clasp-common-lisp - could not build
  buildInputs = [ sbcl ecl ccl clisp abcl ];
}
