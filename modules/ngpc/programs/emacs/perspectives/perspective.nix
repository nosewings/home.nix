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
          persp-created = [ "ngpc/setup-cd-trap" ];
        };
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
              (call-interactively orig args)))
          (defun ngpc/setup-cd-trap ()
            (let* ((buffer (current-buffer))
                   (func (lambda ()
                           ;; ‘window-configuration-change-hook’ is run when
                           ;; either the buffer or the size of the window has
                           ;; changed.  We only want to act when the buffer
                           ;; changes.
                           (let ((new-buffer (window-buffer window)))
                             (unless (eq buffer new-buffer)
                               (with-current-buffer new-buffer
                                 (cd "~"))
                               t)))))
              (ngpc/add-hook-until 'window-configuration-change-hook func nil 'local)))'';
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
