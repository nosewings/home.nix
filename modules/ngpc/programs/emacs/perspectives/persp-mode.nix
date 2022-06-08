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
      config = ''
        (persp-mode)
        (advice-add #'previous-buffer :around #'ngpc/persp-prev-next-buffer)
        (advice-add #'next-buffer :around #'ngpc/persp-prev-next-buffer)'';
      custom = {
        persp-keymap-prefix = "(kbd \"C-c v\")";
      };
      preface = ''
        (defun ngpc/persp-prev-next-buffer (orig &rest args)
          (-let [switch-to-prev-buffer-skip
                 (ngpc/switch-to-prev-buffer-skip-all
                  switch-to-prev-buffer-skip
                  (lambda (window buffer bury-or-kill)
                    (not (memq buffer (persp-buffers (get-current-persp))))))])
          (call-interactively orig args))'';
    };
  };
}
