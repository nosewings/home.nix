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
        init = ''
          (persp-mode)
          (advice-add #'previous-buffer :around #'ngpc/persp-prev-next-buffer)
          (advice-add #'next-buffer :around #'ngpc/persp-prev-next-buffer)'';
        custom = {
          persp-mode-prefix-key = "(kbd \"C-c v\")";
          persp-modestring-short = "t";
        };
        preface = ''
          ;; TODO Should this be marked `pure', `side-effect-free', etc?
          (defun ngpc/persp-prev-next-buffer (orig &rest args)
            (-let [switch-to-prev-buffer-skip
                   (ngpc/switch-to-prev-buffer-skip-all
                    switch-to-prev-buffer-skip
                    (lambda (window buffer bury-or-kill)
                      (not (persp-buffer-list-filter `(,buffer)))))]
              (call-interactively orig args)))'';
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
