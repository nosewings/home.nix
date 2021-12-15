{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.languages.javascript;
in
{
  options.ngpc.languages.javascript = {
    enable = mkEnableOption "Javascript config";
  };
  config = mkIf cfg.enable {
    ngpc.programs.emacs.yatemplate.templateFiles.":.*.js" = ./templates/template.js;
  };
}
