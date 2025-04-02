self: previous:
let
  # Python overrides
  pyOverrides = self: super: {
    pytest-cram = super.pytest-cram.overridePythonAttrs (old: {
      checkPhase = "";
    });
  };
  # Extra haskell packages
  haskOverrides = hsself: hssuper: {
    sbv = self.haskell.lib.dontCheck hssuper.sbv;
  };
in
{
  # Python
  python39       = previous.python39.override  { packageOverrides = pyOverrides; };
  python310      = previous.python310.override { packageOverrides = pyOverrides; };
  python311      = previous.python311.override { packageOverrides = pyOverrides; };
  python312      = previous.python312.override { packageOverrides = pyOverrides; };
  # nbstripout depends on pytest-cram which is not buildable with python 3.12
  nbstripout311  = self.callPackage ./nbstripout311.nix {};
  nbstripout     = previous.nbstripout.overridePythonAttrs (_: {
    checkPhase = "echo NO_TESTS"; # For some reason setting empty string doesn't work
  });
  # Additional programs & tools
  fetchhgPrivate = self.callPackage ./pkgs/fetchhgPrivate {};
  mdo            = self.callPackage ./pkgs/mdo {};
  root-plot      = self.callPackage ./pkgs/root-plot {};
  sdist-release  = self.callPackage ./pkgs/sdist-release {};
  arxiv-get      = self.haskellPackages.callPackage ./pkgs/arxiv-get {};
  bibtexer       = self.haskellPackages.callPackage ./pkgs/bibtexer  {};
  gittery        = self.haskellPackages.callPackage ./pkgs/gittery   {};
  # colcalc        = self.callPackage ./pkgs/colcalc {};
  # Override haskell stuff
  haskell = previous.haskell // {
    packageOverrides = self: super:
      previous.haskell.packageOverrides self super //
      haskOverrides self super;
  };
}
