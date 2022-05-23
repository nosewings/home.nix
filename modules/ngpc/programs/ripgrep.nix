{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.programs.ripgrep;
in
{
  options.ngpc.programs.ripgrep = {
    enable = mkEnableOption "Ripgrep config";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      ripgrep
    ];
    programs.emacs.init.init.packages.rg = {
      enable = true;
    };
  };
}
