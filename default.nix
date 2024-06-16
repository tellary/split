let pkgs = import
  ( fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/25cf937a30bf0801447f6bf544fc7486c6309234.tar.gz"
  ) { };
    ghc = pkgs.haskell.packages.ghc943.ghcWithPackages
    (pkgs: with pkgs; [
      split
      hashtables
      ieee
      pretty-simple
      Decimal
      extra
    ]);
in
  pkgs.stdenv.mkDerivation {
    name = "money-split";
    buildInputs = [ghc];
  }
