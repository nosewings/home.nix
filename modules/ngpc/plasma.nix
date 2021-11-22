{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.plasma;
in
{
  options.ngpc.plasma = {
    enable = mkEnableOption "KDE Plasma config";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs.plasma5Packages; [
      ark
      gwenview
      kate
      okular
      plasma-applet-caffeine-plus
      yakuake
    ];
    services.kdeconnect.enable = true;
  };
}
