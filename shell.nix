let nixpkgs = import <nixpkgs> {};
in with nixpkgs; stdenv.mkDerivation {
  name = "common-lisp";
  # clasp-common-lisp - could not build
  # abcl does not work
  buildInputs = [ sbcl ecl ccl clisp ];
}
