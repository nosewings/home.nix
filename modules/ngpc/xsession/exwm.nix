{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.xsession.exwm;
in
{
  options.ngpc.xsession.exwm = {
    enable = mkEnableOption "EXWM config";
  };

  config = mkIf cfg.enable {
    programs.emacs.init.init.packages = {
      exwm = {
        enable = true;
        custom = {
          exwm-input-global-keys = ''
            '(([?\s-&] . ngpc/launch))
          '';
          exwm-input-simulation-keys = ''
            '(([?\C-b] . [left])
              ([?\C-f] . [right])
              ([?\C-p] . [up])
              ([?\C-n] . [down])
              ([?\C-a] . [home])
              ([?\C-e] . [end])
              ([?\M-v] . [prior])
              ([?\C-v] . [next])
              ([?\C-d] . [delete])
              ([?\C-k] . [S-end delete]))'';
        };
        hook = {
          exwm-update-title = [ "ngpc/exwm-update-buffer-name" ];
        };
        preface = ''
          (defun ngpc/launch (command)
            (interactive (list (read-shell-command "$ ")))
            (start-process-shell-command command nil command))

          (defun ngpc/exwm-init ()
            "Initialize EXWM."
            (require 'exwm-randr)
            (exwm-randr-enable)
            (exwm-init))

          (defun ngpc/exwm-update-buffer-name ()
            (exwm-workspace-rename-buffer exwm-title))'';
      };
    };
    xsession.windowManager.command = "emacs -f ngpc/exwm-init";
  };
}
