{ config, lib, pkgs, ... }:
with lib;
{
  imports = [ ./local.nix ] ++ concatMap filesystem.listFilesRecursive [
    ./platforms
    ./modules
  ];

  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    atool
    bat
    exa
    file
    fd
    killall
    gnumake
    htop
    musescore
    neofetch
    openssl
    pciutils
    ripgrep
    wget
    xorg.xkill
    xterm-24bit
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
      ".direnv/"
      "__pycache__/"
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

  ngpc.languages.nix = {
    enable = true;
    lsp.enable = true;
  };
  ngpc.languages.rust = {
    enable = true;
    lsp.enable = true;
  };
  ngpc.languages.tex.enable = true;
  ngpc.languages.yaml.enable = true;
}
