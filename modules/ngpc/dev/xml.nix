{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.dev.xml;
in
{
  options.ngpc.dev.xml = {
    enable = mkEnableOption "XML config";
  };

  config = mkIf cfg.enable {
    ngpc.programs.emacs.yatemplate.templateFiles = {
      ":.*.xml" = ./templates/template.xml;
    };
  };
}
