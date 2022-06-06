{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs.projectile;
in
{
  options.ngpc.programs.emacs.projectile = {
    enable = mkEnableOption "Emacs Projectile config";
  };

  config = mkIf cfg.enable {
    programs.emacs.init.init.packages.projectile = {
      enable = true;
      bind-keymap = {
        "" = {
          "\"C-c p\"" = "projectile-command-map";
        };
      };
      config = ''
        (projectile-mode)
        (advice-add #'ngpc/rename-current-buffer-file :after #'ngpc/rename-current-buffer-file/projectile-invalidate-cache)'';
      preface = ''
        (defun ngpc/rename-current-buffer-file/projectile-invalidate-cache ()
          (when (projectile-project-p)
            (call-interactively #'projectile-invalidate-cache)))'';
    };
  };
}
