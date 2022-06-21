{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.dev.web;
in
{
  options.ngpc.dev.web = {
    enable = mkEnableOption "Web config";
  };
  config = mkIf cfg.enable {
    ngpc.dev.html.enable = true;
    ngpc.dev.javascript.enable = true;
    programs.ripgrep.extraConfig = ''
      --glob=!*.map
    '';
  };
}
