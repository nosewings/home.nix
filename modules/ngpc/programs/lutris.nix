{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.lutris;
in
{
  options.ngpc.programs.lutris = {
    enable = mkEnableOption "Lutris config";
  };

  config = mkIf cfg.enable {
    allowUnfreePredicate = pkg: builtins.elem (getName pkg) [
      "steam"
      "steam-original"
      "steam-runtime"
    ];

    programs.lutris.enable = true;
    programs.lutris.system.gamePath = "${home.homeDirectory}/lutris";
  };
}
