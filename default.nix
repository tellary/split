let pkgs = import
  ( fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/25cf937a30bf0801447f6bf544fc7486c6309234.tar.gz";
    sha256 = "16incdl8chihc1aw7i18mhv8k848iv7ib4wyn5qn485241c19z82";
  }) { };
  reflex-platform = import (pkgs.fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "v1.2.0.0";
    sha256 = "E9Gkx0KPCvlMBQvim4F09zV/iEMg2hYh1ATcIUgXBCw=";
  }) { };
  deps = (pkgs: with pkgs; [
      reflex-dom
      split
      hashtables
      ieee
      pretty-simple
      Decimal
      extra
      monadlist
      file-embed
      utf8-string
      regex-tdfa
    ]);
  ghc = reflex-platform.ghc.ghcWithPackages deps;
  ghcjs = reflex-platform.ghcjs.ghcWithPackages deps;
  closure-compiler = pkgs.closure-compiler;
  ghcjsFor = execName : pkgs.stdenv.mkDerivation {
    name = "money-split";
    buildInputs = [ghcjs pkgs.closurecompiler];
    src = ./.;
    phases = [ "unpackPhase" "buildPhase" "installPhase" "postInstall" ];
    unpackPhase = ''
      cp $src/*.hs $src/automerge.js $src/split.css $src/publish_report.sh $src/website.json .'';
    buildPhase = ''
      ghcjs ${execName}.hs
    '';
    installPhase = ''
      mkdir $out/
      cp -r ${execName}.jsexe $out/
      pushd $out/${execName}.jsexe
      closure-compiler -W QUIET --js all.js --js_output_file all.min.js
      popd
      cp publish_report.sh website.json $out/
      cp automerge.js $out/${execName}.jsexe/
    '';
    postInstall = "cp $src/html/* $out/${execName}.jsexe/";
  };
in {
  ghc = pkgs.stdenv.mkDerivation {
    name = "money-split";
    buildInputs = [ghc];
    src = ./.;
  };

  # nix-shell -A ghcjs ./default.nix &
  ghcjs = ghcjsFor "split";

  ghcjsOysters202408 = ghcjsFor "oysters202408";

  ghcjs202408oyestersAndCrouton = ghcjsFor "202408oyestersAndCrouton";

  ghcjs2024coimbra = ghcjsFor "2024coimbra";
}
