{ config, pkgs, ... }:
let
  # Repository with config files
  cfg = ../config;
in
{
  # Generic list of programs
  home.packages = with pkgs; [
    # ----------------
    # CLI utils
    silver-searcher
    crudini
    diffstat
    enca
    fortune
    findutils
    ghostscript
    gnupg
    graphviz
    htop
    imagemagick
    inetutils
    mc
    lz4
    jq
    yq
    mercurial
    p7zip
    pdfgrep
    pdftk
    parallel
    postgresql_12
    python3Packages.ipython
    sdist-release
    rlwrap
    rustup
    sqlite-interactive
    sshfs
    youtube-dl
    zip
    curl
    wget
    # ----------------
    # Devtools
    gcc
    julia-bin
  ] ++
  (if config.extra-param.isMac
   then
     [ coreutils ]
   else
     [
       convmv
       sysstat
       usbutils
       # Devtools
       rr
     ]
  );
  # ----
  programs.aspell = {
    enable = true;
    dicts  = a: with a; [ en ru ];
    # See discussion: https://github.com/NixOS/nixpkgs/issues/1000
    config = "data-dir /home/alexey/.nix-profile/lib/aspell";
  };
  # ----
  programs.bash = {
    enable       = true;
    bashrcExtra  = builtins.readFile "${cfg}/bashrc.sh";
  };
  # ----
  programs.screen = {
    enable   = true;
    settings = builtins.readFile "${cfg}/screenrc";
  };
  # ----
  programs.tmux = {
    enable   = true;
    clock24  = true;
    shortcut = "]";
  };
  # ----
  programs.emacs = {
    enable = true;
    extraPackages = epkg : with epkg; [
      undo-tree
      browse-kill-ring
      # Modes
      go-mode
      rust-mode
      haskell-mode
      hledger-mode
      julia-mode
      kotlin-mode
      markdown-mode
      nix-mode
      scala-mode
      yaml-mode
      toml-mode
      # Tools
      org-roam
      org-roam-bibtex
      org-ref
      helm
      magit
      deft
    ];
  };
  programs.emacsPkg = {
    enable   = true;
    packages = [
    ];
  };
  # ----
  programs.ssh = {
    enable = true;
    matchBlocks = {
      "oka01.ihep.su" = {
        user         = "khudyakov";
        proxyJump    = "sepulcarium.org";
      };
      "oka02.ihep.su" = {
        user         = "khudyakov";
        proxyJump    = "sepulcarium.org";
      };
      "okavme.ihep.su" = {
        user         = "control";
        proxyJump    = "oka01.ihep.su";
      };
      # oka03,oka04 are very old
      "oka03.ihep.su" = {
        user         = "khudyakov";
        proxyJump    = "sepulcarium.org";
        extraOptions = {
          "HostKeyAlgorithms"      = "+ssh-rsa";
          "PubkeyAcceptedKeyTypes" = "+ssh-rsa";
          "KexAlgorithms"          = "+diffie-hellman-group1-sha1";
        };
      };
      "oka04.ihep.su" = {
        user         = "khudyakov";
        proxyJump    = "oka01.ihep.su";
        extraOptions = {
          "HostKeyAlgorithms"      = "+ssh-rsa";
          "PubkeyAcceptedKeyTypes" = "+ssh-rsa";
          "KexAlgorithms"          = "+diffie-hellman-group1-sha1";
        };
      };
      # istra & istrad are no longer accessible from outside
      "istra.ihep.su" = {
        proxyJump = "oka01.ihep.su";
      };
      # istrad is mostly off
#      "istrad.ihep.su" = {
#        proxyJump = "oka01.ihep.su";
#      };
    };
  };
  # ----
  programs.git = {
    enable      = true;
    userName    = "Alexey Khudyakov";
    userEmail   = if config.extra-param.isMac
                  then "khudyakov@sirius.online"
                  else "alexey.skladnoy@gmail.com";
    package     = pkgs.gitAndTools.gitFull;
    extraConfig = {
      safe = { directory = "/etc/nixos"; };
      core = { quotePath = false; };
      pull = { ff = "only"; };
      # Kdiff3 doesn't work on macs
      merge             = {
        tool = if config.extra-param.isMac then "meld" else "kdiff3";
      };
      mergetool =
        if config.extra-param.isMac then
          { meld = { path = "${pkgs.meld}/bin/meld"; };
          }
        else
          { meld   = { path = "${pkgs.meld}/bin/meld";     };
            kdiff3 = { path = "${pkgs.kdiff3}/bin/kdiff3"; };
          };
      init.defaultBranch = "master";
      #
      filter.nbstripout = {
        clean    = "${pkgs.nbstripout}/bin/nbstripout";
        smudge   = "cat";
        required = true;
      };
    };
  };
  # ----
  programs.mercurial = {
    enable      = true;
    userName    = "Alexey Khudyakov";
    userEmail   = "alexey.skladnoy@gmail.com";
    extraConfig = ''
    [extensions]
    record   =
    hgk      =
    hggit    =
    color    =
    pager    =
    '';
  };
  # ----
  programs.haskeline = {
    enable = true;
    config = ''
      historyDuplicates: IgnoreAll
      '';
  };
  # ----
  programs.gdb = {
    enable  = true;
    gdbinit = ''
      set auto-load safe-path /
      set disassembly-flavor intel

      define ghcR1
        printf "R1: 0x%016lx\n", $rbx
        x *$rbx
      end
      define ghcR2
        printf "R2: 0x%016lx\n", $r14
        x *$r14
      end
      define ghcR3
        printf "R3: 0x%016lx\n", $rsi
        x *$rsi
      end
      define ghcR4
        printf "R4: 0x%016lx\n", $rdi
        x *$rdi
      end
      define ghcR5
        printf "R5: 0x%016lx\n", $r8
        x *$r8
      end
      define ghcR6
        printf "R6: 0x%016lx\n", $r9
        x *$r9
      end
      '';
  };
}
