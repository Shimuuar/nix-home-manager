self: pkgs:
let
  # Extra haskell packages
  haskOverrides = {
    overrides = hsPkgNew: hsPkgOld: rec {
      spiderment = hsPkgOld.callPackage ./pkgs/haskell/spiderment.nix {};
    };
  };
in
{
  # Nix tools
  fetchhgPrivate = pkgs.callPackage ./pkgs/fetchhgPrivate.nix {};
  # Haskell stuff  
  haskell  = pkgs.haskell // {
    packages = pkgs.haskell.packages // {
      ghc843 = pkgs.haskell.packages.ghc843.override haskOverrides;
    };
  };
  # Programs
  arxiv-get      = pkgs.haskellPackages.callPackage ./pkgs/arxiv-get.nix {};
}

