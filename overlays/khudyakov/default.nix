self: previous:
let
  #
  khudyakovEnv = self.lib.lowPrio( self.buildEnv {
    name             = "khudyakov-env";
    ignoreCollisions = true;
    paths = [
      # GCC
      self.gcc
      # GHC
      self.haskell.compiler.ghc864
      self.haskell.compiler.ghc844
      self.haskell.compiler.ghc822
      # Other haskell utils
      self.cabal-install
      self.haskellPackages.stack
      self.haskellPackages.weeder
      self.haskellPackages.graphmod
      self.haskellPackages.hasktags
      self.haskellPackages.ghc-prof-flamegraph
      self.hlint
      self.cabal2nix
      # My programs
      self.arxiv-get
      self.gittery
      self.mdo
      self.colcalc
      ## servant-websockets is marked as brocken
      # self.plotly-server
#      self.root-plot
      self.nixtools.ghc
      self.nixtools.ipython
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
    #
    ghc-prof-flamegraph = self.callPackage ./pkgs/haskell/ghc-prof-flamegraph.nix {};
  };
  # Extra emacs packages
  emacsPkg = {
    polymode  = self.callPackage ./pkgs/emacsPkg/polymode.nix {};
    tla-tools = self.callPackage ./pkgs/emacsPkg/tla-tools.nix {};
  };
in
{
  # Fetchall package
  inherit khudyakovEnv;
  inherit emacsPkg;
  # Additional programs & tools
  drone-cli-110  = self.callPackage ./pkgs/drone-cli {};
  fetchhgPrivate = self.callPackage ./pkgs/fetchhgPrivate {};
  mdo            = self.callPackage ./pkgs/mdo {};
  root-plot      = self.callPackage ./pkgs/root-plot {};
  arxiv-get      = self.haskellPackages.callPackage ./pkgs/arxiv-get {};
  gittery        = self.haskellPackages.callPackage ./pkgs/gittery   {};
  plotly-server  = self.haskellPackages.callPackage ./pkgs/plotly-server {};
  colcalc        = self.callPackage ./pkgs/colcalc {};
  nixtools = {
    ghc     = import ./pkgs/nixtools/ghc.nix     self;
    ipython = import ./pkgs/nixtools/ipython.nix self;
  };
  # Override haskell stuff
  python27       = previous.python27.override { packageOverrides = pyOverrides; };
  haskell        = previous.haskell // {
    packageOverrides = self: super:
      previous.haskell.packageOverrides self super //
      haskOverrides self super;
  };
  # nbstripout fails
  nbstripout     = previous.nbstripout.overridePythonAttrs (_: {
    checkPhase = "echo NO_TESTS"; # For some reason setting empty string doesn't work
  });
  chromium-temp  = import ./pkgs/chromium-temp self;
}
