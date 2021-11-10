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
  };
}
