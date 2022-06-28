{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs;
in
{
  config = mkIf (cfg.perspectives == "perspective") (mkMerge [
    {
      programs.emacs.init.init.packages.perspective = {
        enable = true;
        hook = {
          persp-activated = [ "ngpc/persp-activated" ];
          persp-created = [ "ngpc/persp-created-cd" ];
        };
        init = "(persp-mode)";
        custom = {
          persp-mode-prefix-key = "(kbd \"C-c v\")";
          persp-modestring-short = "t";
        };
        preface = ''
          (defun ngpc/persp-activated ()
            (set-frame-parameter nil 'buffer-predicate #'persp-is-current-buffer))
          (defun ngpc/persp-created-cd ()
            (with-current-buffer (window-buffer (selected-window))
              (cd "~")))'';
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
