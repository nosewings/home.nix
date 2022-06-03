{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.languages.xml;
in
{
  options.ngpc.languages.nix = {
    enable = mkEnableOption "XML config";
  };

  config = mkIf cfg.enable {
    ngpc.programs.emacs.yatemplate.templateFiles = {
      "00:.*.xml" = ./templates/template.xml;
    };
  };
}
