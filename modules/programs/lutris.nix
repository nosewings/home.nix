with builtins;
{ config, lib, pkgs, ... }:
with lib;
with types;
let
  cfg = config.programs.lutris;
in {
  options.programs.lutris = {
    enable = mkEnableOption "Whether to enable Lutris.";
    system = {
      gamePath = mkOption {
        type = str;
        default = "${config.home.homeDirectory}/Games";
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ lutris ];
    home.file.".config/lutris/system.yml".text = toJSON {
      system.game_path = cfg.system.gamePath;
    };
  };
}
