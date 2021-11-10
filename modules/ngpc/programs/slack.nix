{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.programs.slack;
in
{
  options.ngpc.programs.slack = {
    enable = mkEnableOption "Slack config";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ slack ];
    allowUnfreePredicate = pkg: elem (getName pkg) [ "slack" ];
  };
}
