{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.fonts;
in
{
  options.ngpc.fonts = {
    enable = mkEnableOption "Fonts config";
  };
  config = mkIf cfg.enable (mkMerge [
    {
      fonts.fontconfig.enable = true;
      home.packages = with pkgs; [
        dejavu_fonts
        iosevka
        nerdfonts
        overpass
        roboto
        ubuntu_font_family
      ];
    }
    {
      allowUnfreePredicate = pkg: elem (getName pkg) [ "symbola" ];
      home.packages = with pkgs; [ symbola ];
    }
  ]);
}
