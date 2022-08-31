{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.programs.unrar;
in
{
  options.ngpc.programs.unrar = {
    enable = mkEnableOption "unrar config";
  };

  config = mkIf cfg.enable {
    allowUnfreePredicate = pkg: elem (getName pkg) [
      "unrar"
    ];
    home.packages = with pkgs; [ unrar ];
  };
}
