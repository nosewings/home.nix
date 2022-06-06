{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs.perspective;
in
{
  options.ngpc.programs.emacs.perspective = {
    enable = mkEnableOption "Emacs Perspective config";
  };
  config = mkIf cfg.enable (mkMerge [
    {
      programs.emacs.init.init.packages.perspective = {
        enable = true;
        init = "(persp-mode)";
        custom = {
          persp-mode-prefix-key = "(kbd \"C-c v\")";
        };
      };
    }
    (mkIf (config.ngpc.programs.emacs.completion == "ivy") {
      programs.emacs.init.init.packages.perspective.bind."" = {
        "\"C-x C-b\"" = "persp-counsel-switch-buffer";
      };
    })
    (mkIf (any (x: x == config.ngpc.programs.emacs.completion) [ "ivy" "selectrum" "vertico" ])  {
      programs.emacs.init.init.packages.perspective.bind."" = {
        "\"C-x b\"" = "persp-switch-to-buffer*";
        "\"C-x k\"" = "persp-kill-buffer*";
      };
    })
  ]);
}
