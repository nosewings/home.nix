{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.ripgrep;
in
{
  options.ngpc.programs.ripgrep = {
    enable = mkEnableOption "ripgrep config";
  };

  config = mkIf cfg.enable {
    programs.ripgrep.enable = true;
    programs.emacs.init.init.packages.rg = {
      enable = true;
    };
  };
}
