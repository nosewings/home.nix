{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.programs.retroarch;
in
{
  options.ngpc.programs.retroarch = {
    enable = mkEnableOption "Retroarch config";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ retroarchFull ];
  };
}
