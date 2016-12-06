with (import <nixpkgs> {});

callPackage (

{ stdenv, texlive, emacs }:

let

latex = texlive.combine { inherit (texlive)
  scheme-basic
  beamer
  ms
  lm
  amsmath
  ulem
  float
  wrapfig
  collection-fontsrecommended
;};

in

stdenv.mkDerivation rec {
  name = "learn-ctl-presentation";
  src = ./.;
  buildInputs = [ emacs latex ];
  buildPhase = ''
    HOME=.
    emacs presentation.org --batch -f org-beamer-export-to-latex --kill
    pdflatex presentation.tex
    pdflatex presentation.tex
  '';
  installPhase = ''
    mkdir $out
    cp -v presentation.pdf $out/
  '';
}

){}
