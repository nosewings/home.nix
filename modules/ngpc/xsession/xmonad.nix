{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.xsession.xmonad;
in
{
  options.ngpc.xsession.xmonad = {
    enable = mkEnableOption "XMonad config";
  };

  config = mkIf cfg.enable {
    xsession.windowManager.xmonad = {
      enable = true;
    };
  };
}
