{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.fonts;
in
{
  options.ngpc.fonts = {
    enable = mkEnableOption "Fonts config";
  };

  config = mkIf cfg.enable {
    fonts.fontconfig.enable = true;
    home.packages = with pkgs; [
      dejavu_fonts
      iosevka
      overpass
      roboto
      ubuntu_font_family
    ];
  };
}
