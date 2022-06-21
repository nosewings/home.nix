{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.dev.html;
in
{
  options.ngpc.dev.html = {
    enable = mkEnableOption "HTML config";
  };
  config = mkIf cfg.enable {
    ngpc.programs.emacs.yatemplate.templateFiles.":.*.html" = ./templates/template.html;
  };
}
