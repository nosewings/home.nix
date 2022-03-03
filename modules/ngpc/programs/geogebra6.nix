{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.programs.geogebra6;
in
{
  options.ngpc.programs.geogebra6 = {
    enable = mkEnableOption "Geogebra6 config";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ geogebra6 ];
    allowUnfreePredicate = pkg: elem (getName pkg) [ "geogebra" ];
  };
}
