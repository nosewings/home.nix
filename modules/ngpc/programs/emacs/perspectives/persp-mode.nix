{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs;
in
{
  config = mkIf (cfg.perspectives == "persp-mode") {
    programs.emacs.init.init.packages.persp-mode = {
      enable = true;
      bind = {
        persp-mode-map = {
          "\"C-x b\"" = "persp-switch-to-buffer";
        };
      };
      config = "(persp-mode)";
      custom = {
        persp-keymap-prefix = "(kbd \"C-c v\")";
        persp-set-buffer-predicate = "t";
      };
    };
  };
}
