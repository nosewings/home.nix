{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.languages.html;
in
{
  options.ngpc.languages.html = {
    enable = mkEnableOption "HTML config";
  };
  config = mkIf cfg.enable {
    ngpc.programs.emacs.yatemplate.templateFiles.":.*.html" = ./templates/template.html;
  };
}
