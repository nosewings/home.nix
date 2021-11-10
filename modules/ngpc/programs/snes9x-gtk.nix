with builtins;
{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.programs.snes9x-gtk;
in
{
  options.ngpc.programs.snes9x-gtk = {
    enable = mkEnableOption "Snes9x-GTK config";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ snes9x-gtk ];
    allowUnfreePredicate = pkg: elem (getName pkg) [ "snes9x-gtk" ];
  };
}
