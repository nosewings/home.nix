{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.languages.xml;
in
{
  options.ngpc.languages.xml = {
    enable = mkEnableOption "XML config";
  };

  config = mkIf cfg.enable {
    ngpc.programs.emacs.yatemplate.templateFiles = {
      ":.*.xml" = ./templates/template.xml;
    };
  };
}
