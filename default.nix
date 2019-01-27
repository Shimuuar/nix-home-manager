self: previous:
let
  #
  khudyakovEnv = self.lib.lowPrio( self.buildEnv {
    name             = "khudyakov-env";
    ignoreCollisions = true;
    paths = [
      # Nix
      self.nix-prefetch-git
      # GHC
      self.haskell.compiler.ghc844
      self.haskell.compiler.ghc822
      self.haskell.compiler.ghc863
      # Other haskell utils
      self.cabal-install
      self.haskellPackages.stack
      self.haskellPackages.weeder
      self.haskellPackages.graphmod
      self.haskellPackages.hasktags
      self.hlint
      self.cabal2nix
      self.nbstripout
      # My programs
      self.arxiv-get
      self.gittery
      self.plotly-server
      self.root-plot
      ];
    });
  # Python overrides
  pyOverrides = self: super: {
    pytest-cram = super.pytest-cram.overridePythonAttrs (old: {
      checkPhase = "";
    });
  };
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
  gittery        = self.haskellPackages.callPackage ./pkgs/gittery   {};
  plotly-server  = self.haskellPackages.callPackage ./pkgs/plotly-server {};
  # Override haskell stuff
  python27       = previous.python27.override { packageOverrides = pyOverrides; };
  haskell        = previous.haskell // {
    packageOverrides = self: super:
      previous.haskell.packageOverrides self super //
      haskOverrides self super;
  };
}
