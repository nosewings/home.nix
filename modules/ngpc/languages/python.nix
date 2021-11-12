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
      programs.emacs.init.init.packages = {
        lsp-pyright = {
          enable = true;
          hook = {
            python-mode = [ "ngpc/load-pyright" ];
          };
          preface = ''
            (defun ngpc/load-pyright ()
              (require 'lsp-pyright)
              (lsp))
          '';
        };
      };
    })
  ]);
}
