{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs;
in
{
  config = mkIf (cfg.perspectives == "persp-mode") {
    programs.emacs.init.init.packages.persp-mode = {
      enable = true;
      config = "(persp-mode)";
    };
  };
}
