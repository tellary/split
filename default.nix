let pkgs = import
  ( fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/25cf937a30bf0801447f6bf544fc7486c6309234.tar.gz"
  ) { };
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
    ]);
  ghc = reflex-platform.ghc.ghcWithPackages deps;
  ghcjs = reflex-platform.ghcjs.ghcWithPackages deps;
in {
  ghc = pkgs.stdenv.mkDerivation {
    name = "money-split";
    buildInputs = [ghc];
    src = ./.;
  };

  ghcjs = pkgs.stdenv.mkDerivation {
    name = "money-split";
    buildInputs = [ghcjs];
    src = ./.;
  };
}
