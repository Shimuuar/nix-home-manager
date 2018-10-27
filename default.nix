self: pkgs:
let
  #
  khudyakovEnv = pkgs.lib.lowPrio( pkgs.buildEnv {
    name             = "khudyakov-env";
    ignoreCollisions = true;
    paths = [
      # GHC
      self.haskell.compiler.ghc843
      self.haskell.compiler.ghc802
      self.haskell.compiler.ghc822
      self.haskell.compiler.ghc861
      # Other haskell utils
      self.haskellPackages.cabal-install
      self.haskellPackages.stack
      self.haskellPackages.weeder
      self.haskellPackages.graphmod
      self.cabal2nix
      # My programs
      self.arxiv-get
      self.root-plot
      ];
    });

  # Extra haskell packages
  haskOverrides = {
    overrides = hsPkgNew: hsPkgOld: rec {
      spiderment    = hsPkgOld.callPackage ./pkgs/haskell/spiderment.nix {};
      comic-scraper = hsPkgOld.callPackage ./pkgs/haskell/comic-scraper  {};
    };
  };
in
{
  # Additional programs & tools
  fetchhgPrivate = self.callPackage ./pkgs/fetchhgPrivate {};
  root-plot      = self.callPackage ./pkgs/root-plot {};
  arxiv-get      = self.haskellPackages.callPackage ./pkgs/arxiv-get {};
  # Override haskell stuff
  haskellPackages = pkgs.haskellPackages.override haskOverrides;
  haskell         = pkgs.haskell // {
    packages = pkgs.haskell.packages // {
      ghc843 = pkgs.haskell.packages.ghc843.override haskOverrides;
    };
  };
  # Build environment
  inherit khudyakovEnv;
}
