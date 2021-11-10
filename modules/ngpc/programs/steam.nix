{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.programs.steam;
in
{
  options.ngpc.programs.steam = {
    enable = mkEnableOption "Steam config";
  };

  config = mkIf cfg.enable {
    allowUnfreePredicate = pkg: elem (getName pkg) [
      "steam"
      "steam-original"
      "steam-runtime"
    ];
    home.packages = with pkgs; [ steam ];
  };
}
