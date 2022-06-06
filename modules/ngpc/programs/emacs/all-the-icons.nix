{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs.all-the-icons;
in
{
  options.ngpc.programs.emacs.all-the-icons = {
    enable = mkEnableOption "Emacs all-the-icons config";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ emacs-all-the-icons-fonts ];
    programs.emacs.init.init.packages.all-the-icons.enable = true;
  };
}
