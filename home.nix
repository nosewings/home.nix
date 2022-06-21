{
  config, lib, pkgs,
  hardware, leisure, singleUser, ssh, xsession,
  agda, haskell, html, java, javascript, markdown, nix, python, rust, shell, tex, xml, yaml,
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
      inetutils
      lsof
      nano
      openssl
      perl
      pv
      strace
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

  programs.bash = {
    enable = true;
    bashrcExtra = ''
      ${optionalString singleUser ". ${config.home.profileDirectory}/etc/profile.d/nix.sh"}
      if [[ -f ~/.bashrc.local ]]; then
          source ~/.bashrc.local
      fi
    '';
  };
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableSyntaxHighlighting = true;
    initExtraFirst = optionalString singleUser ". ${config.home.profileDirectory}/etc/profile.d/nix.sh";
    initExtra = ''
      if [[ -f ~/.zshrc.local ]]; then
          source ~/.zshrc.local
      fi
    '';
  };
  programs.fish = {
    enable = true;
    shellInit = ''
      ${optionalString singleUser "replay source ${config.home.profileDirectory}/etc/profile.d/nix.sh"}
      if test -f ~/.config/fish/config.fish.local
          source ~/.config/fish/config.fish.local
      end
    '';
    interactiveShellInit = ''
      set fish_greeting
    '';
    functions = {
      fnf = {
        body = "$argv &>/dev/null & disown";
      };
    };
    plugins = [
      {
        name = "replay";
        src = pkgs.fetchFromGitHub {
          owner = "jorgebucaran";
          repo = "replay.fish";
          rev = "bd8e5b89ec78313538e747f0292fcaf631e87bd2";
          sha256 = "0inniabgdbd7yq71rpmpnzhbk8y23ggvlk4jhaapc7bz0yhbxkkc";
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
  programs.htop = {
    enable = true;
    settings = {
      hide_userland_threads = 1;
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

  ngpc.fonts.enable = true;

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
  ngpc.languages.markdown = mkIf markdown {
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
