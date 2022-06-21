{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.dev.yaml;
in
{
  options.ngpc.dev.yaml = {
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
