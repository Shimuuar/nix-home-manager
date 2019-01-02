self: previous:
let
  #
  khudyakovEnv = previous.lib.lowPrio( previous.buildEnv {
    name             = "khudyakov-env";
    ignoreCollisions = true;
    paths = [
      # Nix
      self.nix-prefetch-git
      # GHC
      self.haskell.compiler.ghc844
      self.haskell.compiler.ghc802
      self.haskell.compiler.ghc822
      self.haskell.compiler.ghc862
      # Other haskell utils
      self.haskellPackages.cabal-install
      self.haskellPackages.stack
      self.haskellPackages.weeder
      self.haskellPackages.graphmod
      self.hlint
      self.cabal2nix
      # My programs
      self.arxiv-get
      self.plotly-server
      self.root-plot
      ];
    });

  # Extra haskell packages
  haskOverrides = self: super: {
    spiderment    = self.callPackage ./pkgs/haskell/spiderment.nix {};
    comic-scraper = self.callPackage ./pkgs/haskell/comic-scraper  {};
  };
in
{
  # Fetchall package
  inherit khudyakovEnv;
  # Additional programs & tools
  fetchhgPrivate = self.callPackage ./pkgs/fetchhgPrivate {};
  root-plot      = self.callPackage ./pkgs/root-plot {};
  arxiv-get      = self.haskellPackages.callPackage ./pkgs/arxiv-get {};
  plotly-server  = self.haskellPackages.callPackage ./pkgs/plotly-server {};
  # Override haskell stuff
  haskell        = previous.haskell // {
    packageOverrides = self: super:
      previous.haskell.packageOverrides self super //
      haskOverrides self super;
  };
}
