let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
in {
  weechat-el = stdenv.mkDerivation rec {
    name = "weechat-el";
    src = ./.;
    
    buildInputs = with pkgs; [ emacs24 git ];

    phases = "unpackPhase buildPhase installPhsae testPhase";

    buildPhase = ''
      make package
    '';

    installPhsae = ''
      mkdir -p $out
      cp -r * $out/
    '';

    testPhase = ''
      cd $out
      make test
    '';
  };
}
