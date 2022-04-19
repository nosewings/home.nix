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
    home.packages = with pkgs; with plasma5Packages; [
      ark
      bluedevil
      bluez-qt
      gwenview
      kate
      okular
      yakuake
    ];
    services.kdeconnect.enable = true;
  };
}
