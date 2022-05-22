{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.languages.python;
in
{
  options.ngpc.languages.python = {
    enable = mkEnableOption "Python config";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [ pyright ];
    }
    (mkIf config.ngpc.programs.emacs.lsp.enable {
      home.packages = with pkgs; [
        python310
        python310Packages.ipython
      ];
      programs.emacs.init.init.packages = {
        python = {
          enable = true;
          bind = {
            python-mode-map = {
              "\"C-c c\"" = "ngpc/python-execute-file";
            };
          };
          custom = {
            python-shell-interpreter = "\"ipython\"";
            python-shell-interpreter-args = "\"--simple-prompt -i\"";
          };
          preface = ''
            (defun ngpc/python-execute-file ()
              (interactive)
              (compile (format "%s %s" python-shell-interpreter buffer-file-name) t))'';
        };
        lsp-pyright = {
          enable = true;
          hook = {
            python-mode = [ "ngpc/load-pyright" ];
          };
          preface = ''
            (defun ngpc/load-pyright ()
              (require 'lsp-pyright)
              (lsp))'';
        };
      };
    })
  ]);
}
