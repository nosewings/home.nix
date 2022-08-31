{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.programs.atool;
in
{
  options.ngpc.programs.atool = {
    enable = mkEnableOption "atool config";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ atool ];
    ngpc.programs.unrar.enable = true;
  };
}
