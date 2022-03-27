{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.services.fluidsynth;
in
{
  options.ngpc.services.fluidsynth = {
    enable = mkEnableOption "Fluidsynth config";
  };

  config = mkIf cfg.enable {
    # home.packages = with pkgs; [ FatBoy ];
    services.fluidsynth = {
      enable = true;
      soundService = "pipewire-pulse";
      # soundFont = "${pkgs.FatBoy}/share/soundfonts/FatBoy.sf2";
    };
  };
}
