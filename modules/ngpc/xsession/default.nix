{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.xsession;
in
{
  options.ngpc.xsession = {
    enable = mkEnableOption "XSession config";
  };

  config = mkIf cfg.enable {
    xsession = {
      enable = true;
      scriptPath = ".xsession-hm";
    };
  };
}
