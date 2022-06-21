{ config, lib, pkgs, ... }:
with lib;
with types;
let
  cfg = config.programs.ripgrep;
  configFile = "${config.xdg.configHome}/ripgrep";
in
{
  options.programs.ripgrep = {
    enable = mkEnableOption "ripgrep config";
    extraConfig = mkOption {
      type = lines;
      description = "Extra settings for ripgrep.";
      default = "";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        ripgrep
      ];
    }
    (mkIf (cfg.extraConfig != "") {
      home.sessionVariables.RIPGREP_CONFIG_PATH = configFile;
      home.file."${configFile}".text = cfg.extraConfig;
    })
  ]);
}
