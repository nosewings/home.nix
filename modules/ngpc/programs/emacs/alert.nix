{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs.alert;
in
{
  options.ngpc.programs.emacs.alert = {
    enable = mkEnableOption "Emacs alert config";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ libnotify ];
    programs.emacs.init.init.packages = {
      alert = {
        enable = true;
        custom = {
          alert-libnotify-command = "\"${pkgs.libnotify}/bin/notify-send\"";
        };
      };
    };
  };
}
