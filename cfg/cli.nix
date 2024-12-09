{ config, pkgs, ... }:
let
  # Repository with config files
  cfg = ../config;
  # We want to package mercurial as python library not as application.
  # This allows hg to import extensions
  #
  # See https://github.com/NixOS/nixpkgs/issues/78509
  py_pkg = pkgs.python3.withPackages (ps: with ps;
    [ ipython
      hg-git
      mercurial
    ]);
  mercurial = pkgs.python3Packages.mercurial;
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
    lzip
    jq
    yq
    p7zip
    pdfgrep
    pdftk
    parallel
    postgresql_14
    sdist-release
    rlwrap
    rustup
    sqlite-interactive
    sshfs
    yt-dlp
    zip
    curl
    wget
    djvu2pdf
    # ----------------
    # Devtools
    gcc
    julia-bin
    py_pkg
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
    package = pkgs.emacs-gtk;
    extraPackages = epkg : with epkg; [
      undo-tree
      browse-kill-ring
      # Modes
      go-mode
      rust-mode
      haskell-mode
      python-mode
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
      openwith
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
    userEmail   = "alexey.skladnoy@gmail.com";
    package     = pkgs.gitAndTools.gitFull;
    extraConfig = {
      safe = { directory = "/etc/nixos"; };
      core = { quotePath = false; };
      pull = { ff = "only"; };
      merge             = {
        tool          = "kdiff3";
        conflictstyle = "zdiff3";
      };
      mergetool = {
        meld   = { path = "${pkgs.meld}/bin/meld";     };
        kdiff3 = { path = "${pkgs.kdiff3}/bin/kdiff3"; };
      };
      init.defaultBranch = "master";
      # Check objects eagerly
      transfer.fsckobjects = true;
      fetch.fsckobjects    = true;
      receive.fsckobjects  = true;
      #
      filter.nbstripout = {
        # Using nbcovert works in principle. In practice it's unacceptably slow
        clean    = "${pkgs.nbstripout311}/bin/nbstripout";
        smudge   = "cat";
        required = true;
      };
    };
  };
  # ----
  programs.mercurial = {
    enable      = true;
    package     = py_pkg;
    userName    = "Alexey Khudyakov";
    userEmail   = "alexey.skladnoy@gmail.com";
    # Note: hggit is broken! Use hg-fast-export
    extraConfig = {
      extensions = {
        record = "";
        hgk    = "";
        color  = "";
        pager  = "";
      };
    };
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
