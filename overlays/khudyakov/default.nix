self: previous:
let
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
  # Additional programs & tools
  fetchhgPrivate = self.callPackage ./pkgs/fetchhgPrivate {};
  mdo            = self.callPackage ./pkgs/mdo {};
  root-plot      = self.callPackage ./pkgs/root-plot {};
  sdist-release  = self.callPackage ./pkgs/sdist-release {};
  arxiv-get      = self.haskellPackages.callPackage ./pkgs/arxiv-get {};
  bibtexer       = self.haskellPackages.callPackage ./pkgs/bibtexer  {};
  gittery        = self.haskellPackages.callPackage ./pkgs/gittery   {};
  plotly-server  = self.haskellPackages.callPackage ./pkgs/plotly-server {};
  # colcalc        = self.callPackage ./pkgs/colcalc {};
  nixtools = {
    ghc     = import ./pkgs/nixtools/ghc.nix     self;
    ipython = import ./pkgs/nixtools/ipython.nix self;
  };
  # Override haskell stuff
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
