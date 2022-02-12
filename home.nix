{ config, lib, pkgs, ... }:
with lib;
{
  imports = [ ./local.nix ] ++ filter (hasSuffix ".nix") (concatMap filesystem.listFilesRecursive [
    ./platforms
    ./modules
  ]);

  programs.home-manager.enable = true;

  home.packages = with pkgs; flatten [
    atool
    bat
    exa
    file
    fd
    killall
    gnumake
    htop
    mgba
    mplayer
    musescore
    neofetch
    nix-prefetch-github
    openssl
    openxcom
    pciutils
    ripgrep
    wget
    (with xorg; [
      xev
      xkill
    ])
    xterm-24bit
    yt-dlp
  ];

  programs.bash.enable = true;
  programs.zsh.enable = true;
  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      set fish_greeting
    '';
  };

  programs.starship.enable = true;

  programs.tmux = {
    enable = true;
    terminal = "tmux-256color";
    extraConfig = ''
      set -ga terminal-overrides ',*256col*:Tc'
    '';
  };

  programs.bat.enable = true;
  programs.exa = {
    enable = true;
    enableAliases = true;
  };

  programs.git = {
    enable = true;
    delta.enable = true;
    userEmail = "coltharp@pdx.edu";
    userName = "Nicholas Coltharp";
    ignores = [
      "*.aux"
      "*.bbl"
      "*.bcf"
      "*.blg"
      "*.log"
      "*.out"
      "*.pdf"
      "*.run.xml"
      ".direnv/"
      "__pycache__/"
      "auto/"
      "dist/"
      "dist-newstyle/"
    ];
    extraConfig = {
      init = {
        defaultBranch = "main";
      };
    };
  };

  ngpc.programs.direnv.enable = true;
  ngpc.programs.emacs.enable = true;
  ngpc.programs.ssh.enable = true;

  ngpc.languages.html.enable = true;
  ngpc.languages.javascript.enable = true;
  ngpc.languages.nix = {
    enable = true;
    lsp.enable = true;
  };
  ngpc.languages.python.enable = true;
  ngpc.languages.rust = {
    enable = true;
    lsp.enable = true;
  };
  ngpc.languages.tex.enable = true;
  ngpc.languages.yaml.enable = true;
}
