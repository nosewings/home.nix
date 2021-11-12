with builtins;
{ lib, pkgs, ... }:
with lib;
mkIf (currentSystem == "x86_64-linux") {
  home.sessionVariables = {
    EDITOR = "emacsclient -q emacs";
    VISUAL = "emacsclient -cq emacs";
  };

  xdg.userDirs = {
    enable = true;
    createDirectories = true;
    desktop = "$HOME/desktop";
    documents = "$HOME/documents";
    download = "$HOME/downloads";
    music = "$HOME/music";
    pictures = "$HOME/pictures";
    publicShare = "$HOME/share";
    templates = "$HOME/templates";
    videos = "$HOME/videos";
  };

  home.packages = with pkgs; [
    agda
    openrgb
    usbutils
    vlc
  ];

  programs.beets.enable = true;

  programs.firefox.enable = true;
  home.sessionVariables.MOZ_ENABLE_WAYLAND = "1";

  services.gammastep = {
    enable = true;
    provider = "geoclue2";
    temperature.night = 2500;
  };

  ngpc.fonts.enable = true;
  ngpc.plasma.enable = true;

  ngpc.languages.agda.enable = true;
  ngpc.languages.haskell.enable = true;

  ngpc.programs.discord.enable = true;
  ngpc.programs.lutris.enable = true;
  ngpc.programs.retroarch.enable = true;
  ngpc.programs.slack.enable = true;
  ngpc.programs.steam.enable = true;
  ngpc.programs.zoom.enable = true;

  ngpc.services.fluidsynth.enable = true;
}
