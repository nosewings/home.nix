{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.programs.teams;
in
{
  options.ngpc.programs.teams = {
    enable = mkEnableOption "Microsoft Teams config";
  };

  config = mkIf cfg.enable {
    allowUnfreePredicate = pkg: elem (getName pkg) [ "teams" ];
    home.packages = with pkgs; [ teams ];
  };
}
