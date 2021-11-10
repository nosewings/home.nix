{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.languages.shell;
in
{
  options.ngpc.languages.shell = {
    enable = mkEnableOption "Shell config";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ shellcheck ];
  };
}
