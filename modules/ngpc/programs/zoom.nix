{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.programs.zoom;
in
{
  options.ngpc.programs.zoom = {
    enable = mkEnableOption "Zoom config";
  };

  config = mkIf cfg.enable {
    allowUnfreePredicate = pkg: elem (getName pkg) [ "zoom" ];
    home.packages = with pkgs; [ zoom-us ];
  };
}
