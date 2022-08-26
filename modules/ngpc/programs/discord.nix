{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.programs.discord;
in
{
  options.ngpc.programs.discord = {
    enable = mkEnableOption "Discord config";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ discord ];
    allowUnfreePredicate = pkg: elem (getName pkg) [ "discord" ];
    # Stop Discord from refusing to start when an update is available.
    xdg.configFile."discord/settings.json".text = ''
      {
        "SKIP_HOST_UPDATE": true
      }
    '';
  };
}
