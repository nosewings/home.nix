{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.languages.yaml;
in
{
  options.ngpc.languages.yaml = {
    enable = mkEnableOption "YAML config";
  };

  config = mkIf cfg.enable {
    programs.emacs.init.init.packages = {
      yaml-mode = {
        enable = true;
      };
    };
  };
}
