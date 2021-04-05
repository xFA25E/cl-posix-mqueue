let
  quickstart = builtins.fetchurl https://beta.quicklisp.org/quicklisp.lisp;
  data-home = "\${XDG_DATA_HOME:-$HOME/.local/share}";
in
with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "common-lisp";
  buildInputs = [ sbcl ecl ccl clisp ];
  setupLispDev = writeShellScript "setupLispDev" ''
    sbcl --non-interactive --load "${quickstart}" \
         --eval "(quicklisp-quickstart:install :path #p\"${data-home}/quicklisp/\")"
  '';
  updateLispDev = writeShellScript "updateLispDev" ''
    sbcl --non-interactive --eval "(ql:update-client)" --eval "(ql:update-dist \"quicklisp\")"
  '';
  shellHook = ''
    '';
}
