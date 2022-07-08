{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.dev.xml;
in
{
  options.ngpc.dev.xml = {
    enable = mkEnableOption "XML config";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      libxml2  # for xmllint
    ];
    ngpc.programs.emacs.yatemplate.templateFiles = {
      ":.*.xml" = ./templates/template.xml;
    };
  };
}
