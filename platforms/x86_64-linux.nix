{ lib, pkgs, desktop, leisure, xsession, ... }:
{
  home.sessionVariables = {
    EDITOR = "emacsclient -q emacs";
    VISUAL = "emacsclient -cq emacs";
  };

  xdg.userDirs = lib.mkIf desktop {
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

  home.packages = lib.mkMerge [
    (with pkgs; [
      agda
      libreoffice-fresh
      lm_sensors
      usbutils
    ])
    (lib.mkIf leisure (with pkgs; [
      mgba
      # openmw
      openrgb
      openxcom
      vlc
    ]))
  ];

  programs.beets.enable = lib.mkIf leisure true;

  programs.firefox.enable = lib.mkIf desktop true;
  home.sessionVariables.MOZ_ENABLE_WAYLAND = "1";

  programs.starship.enable = true;

  ngpc.fonts.enable = true;
  ngpc.plasma.enable = lib.mkIf xsession true;

  ngpc.languages.agda.enable = true;
  ngpc.languages.haskell.enable = true;

  ngpc.programs.discord.enable = lib.mkIf leisure true;
  ngpc.programs.lutris.enable = lib.mkIf leisure true;
  ngpc.programs.retroarch.enable = lib.mkIf leisure true;
  ngpc.programs.slack.enable = lib.mkIf leisure true;
  ngpc.programs.steam.enable = lib.mkIf leisure true;
  ngpc.programs.zoom.enable = lib.mkIf leisure true;

  ngpc.services.fluidsynth.enable = lib.mkIf leisure true;
}
