{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs.magit;
in
{
  options.ngpc.programs.emacs.magit = {
    enable = mkEnableOption "Emacs Magit config";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      programs.emacs.init.init.packages.magit = {
        enable = true;
        bind = {
          "" = {
            "\"C-c g\"" = "magit-file-dispatch";
          };
        };
        custom = {
          git-commit-summary-max-length = "50";
          # If we try to commit with nothing staged, don't let us.
          magit-commit-ask-to-stage = "nil";
          # Show word-level differences in diffs.
          magit-diff-refine-hunk = "'all";
        };
        hook = {
          git-commit-mode = [ "ngpc/git-commit-mode-hook" ];
        };
        preface = ''
          (defun ngpc/git-commit-mode-hook ()
            (setq-local fill-column 72))'';
      };
    }
    (mkIf (cfg.enable && config.ngpc.dev.markdown.enable) {
      programs.emacs.init.init.packages.magit.custom.git-commit-major-mode = "#'poly-markdown-mode";
    })
  ]);
}
