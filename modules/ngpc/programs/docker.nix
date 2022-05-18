{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.programs.docker;
in
{
  options.ngpc.programs.docker = {
    enable = mkEnableOption "Docker config";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      docker-compose
      docker-edge
      docker-machine
    ];
    programs.emacs.init.init.packages = {
      dockerfile-mode = {
        enable = true;
      };
    };
  };
}
