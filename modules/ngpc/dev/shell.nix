{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.dev.shell;
in
{
  options.ngpc.dev.shell = {
    enable = mkEnableOption "Shell config";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ shellcheck ];
  };
}
