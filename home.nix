{
  config, lib, pkgs,
  hardware, leisure, ssh, xsession,
  agda, haskell, html, java, javascript, nix, python, rust, shell, tex, xml, yaml,
  ...
}:
with lib; {
  imports = filter (hasSuffix ".nix") (filesystem.listFilesRecursive ./modules);

  programs.home-manager.enable = true;

  home.packages = mkMerge [
    (with pkgs; flatten [
      atool
      bat
      cachix
      clang-tools
      exa
      file
      fd
      gcc
      gnumake
      killall
      htop
      inetutils
      lsof
      openssl
      perl
      pv
      unzip
      wget
      (with xorg; [
        xev
        xkill
      ])
      xterm-24bit
      zip
    ])
    (mkIf leisure (with pkgs; [
      mplayer
      musescore
      neofetch
      yt-dlp
    ]))
    (mkIf hardware (with pkgs; [
      pciutils
    ]))
  ];

  ngpc.shell = "fish";
  programs.bash = {
    enable = true;
    bashrcExtra = ''
      if [[ -f ~/.bashrc.local ]]; then
          source ~/.bashrc.local
      fi
    '';
  };
  programs.zsh = {
    enable = true;
    initExtra = ''
      if [[ -f ~/.zshrc.local ]]; then
          source ~/.zshrc.local
      fi
    '';
  };
  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      set fish_greeting
    '';
    shellInit = ''
      if test -f ~/.config/fish/config.fish.local
          source ~/.config/fish/config.fish.local
      end
    '';
    plugins = [
      {
        name = "replay";
        src = pkgs.fetchFromGitHub {
          owner = "jorgebucaran";
          repo = "replay.fish";
          rev = "a2eef7319adb1c5cd00a4454bb29a9e072d51a53";
          sha256 = "ovxpzHC9coRtm/v3rQlPlX1NEe22+WLA7HpnfqtUJU8=";
        };
      }
    ];
  };

  programs.tmux = {
    enable = true;
    terminal = "tmux-256color";
    extraConfig = ''
      set -ga terminal-overrides ',*256color:Tc,xterm-24bit:Tc'
    '';
  };

  programs.bat.enable = true;
  programs.exa = {
    enable = true;
    enableAliases = true;
  };

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;
    delta.enable = true;
    userEmail = "coltharp@pdx.edu";
    userName = "Nicholas Coltharp";
    aliases = {
      root = "rev-parse --show-toplevel";
    };
    extraConfig = {
      core.commentChar = ";";
      init = {
        defaultBranch = "main";
      };
    };
  };
  programs.starship = {
    enable = true;
    settings = {
      git_branch = {
        always_show_remote = true;
      };
    };
  };

  ngpc.programs.direnv.enable = true;
  ngpc.programs.docker.enable = true;
  ngpc.programs.emacs.enable = true;
  # ngpc.programs.geogebra6.enable = mkIf leisure true;
  ngpc.programs.ripgrep.enable = true;
  ngpc.programs.ssh = mkIf ssh {
    enable = true;
  };

  ngpc.languages.agda = mkIf agda {
    enable = true;
  };
  ngpc.languages.haskell = mkIf haskell {
    enable = true;
  };
  ngpc.languages.html = mkIf html {
    enable = true;
  };
  ngpc.languages.java = mkIf java {
    enable = true;
  };
  ngpc.languages.javascript = mkIf javascript {
    enable = true;
  };
  ngpc.languages.nix = mkIf nix {
    enable = true;
    lsp.enable = true;
  };
  ngpc.languages.python = mkIf python {
    enable = true;
  };
  ngpc.languages.rust = mkIf rust {
    enable = true;
    lsp.enable = true;
  };
  ngpc.languages.shell = mkIf shell {
    enable = true;
  };
  ngpc.languages.tex = mkIf tex {
    enable = true;
  };
  ngpc.languages.xml = mkIf xml {
    enable = true;
  };
  ngpc.languages.yaml = mkIf yaml {
    enable = true;
  };

  ngpc.xsession.enable = mkIf xsession true;
  ngpc.xsession.exwm.enable = mkIf xsession true;
}
