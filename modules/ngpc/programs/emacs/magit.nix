{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs.magit;
in
{
  options.ngpc.programs.emacs.magit = {
    enable = mkEnableOption "Emacs Magit config";
  };

  config = mkIf cfg.enable {
    programs.emacs.init.init.packages.magit = {
      enable = true;
      bind = {
        "" = {
          "\"C-c g\"" = "magit-file-dispatch";
        };
      };
      custom = {
        git-commit-major-mode = "#'markdown-mode";
        git-commit-summary-max-length = "50";
        # If we try to commit with nothing staged, don't let
        # us.
        magit-commit-ask-to-stage = "nil";
      };
      hook = {
        git-commit-mode = [ "ngpc/git-commit-mode-hook" ];
      };
      preface = ''
        (defun ngpc/git-commit-mode-hook ()
          (setq-local fill-column 72))'';
    };
  };
}
