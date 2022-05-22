{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.languages.tex;
in
{
  options.ngpc.languages.tex = {
    enable = mkEnableOption "TeX config";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      texlive.combined.scheme-full
    ];
    programs.emacs.init.init.packages = {
      tex = {
        enable = true;
        package = epkgs: epkgs.auctex;
        custom = {
          TeX-parse-self = "t";
          TeX-auto-save = "t";
        };
        hook = {
          TeX-mode = [ "ngpc/extend-TeX-command-list" ];
        };
        preface = ''
          (defun ngpc/extend-TeX-command-list ()
            (cl-pushnew '("Make" "make" TeX-run-compile nil t) TeX-command-list))'';
      };
    };
  };
}
