let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
in {
  weechat-el = stdenv.mkDerivation rec {
    name = "weechat-el";
    src = ./.;
    
    buildInputs = with pkgs; with pkgs.emacs24Packages; [
      emacs24
      git
      s
      cl-lib
      tracking];

    phases = "unpackPhase buildPhase installPhase testPhase";

    buildPhase = ''
      make package
    '';

    installPhase = ''
      mkdir -p $out
      cp -r * $out/
    '';

    testPhase = ''
      cd $out
      make test
    '';
  };
}
