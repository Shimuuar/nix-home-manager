{ lib
, python311
, fetchPypi
, coreutils
, git
, mercurial
}:

python311.pkgs.buildPythonApplication rec {
  version = "0.6.1";
  pname = "nbstripout";

  src = fetchPypi {
    inherit pname version;
    hash = "sha256-kGW83RSIs4bk88CB/8HUj0UTovjYv00NmiggjF2v6dM=";
  };

  # for some reason, darwin uses /bin/sh echo native instead of echo binary, so
  # force using the echo binary
  postPatch = ''
    substituteInPlace tests/test-git.t --replace "echo" "${coreutils}/bin/echo"
  '';

  propagatedBuildInputs = with python311.pkgs; [
    ipython
    nbformat
  ];

  nativeCheckInputs = [
    coreutils
    git
    mercurial
  ];

  # preCheck = ''
  #   export HOME=$(mktemp -d)
  #   export PATH=$out/bin:$PATH
  #   git config --global init.defaultBranch main
  # '';
  checkPhase = "";
  
  meta = {
    description = "Strip output from Jupyter and IPython notebooks";
    homepage = "https://github.com/kynan/nbstripout";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ jluttine ];
    mainProgram = "nbstripout";
  };
}
