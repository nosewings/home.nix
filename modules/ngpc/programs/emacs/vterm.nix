{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs.vterm;
in
{
  options.ngpc.programs.emacs.vterm = {
    enable = mkEnableOption "Emacs vterm config";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      programs.emacs.vterm.enable = true;
      programs.emacs.init.init.packages = {
        vterm = {
          enable = true;
          hook = {
            vterm-mode = [
              "ngpc/disable-hl-line"
            ];
          };
        };
        vterm-toggle = {
          enable = true;
        };
      };
    }
    (mkIf (config.ngpc.programs.emacs.perspectives == "perspective") {
      programs.emacs.init.init.packages.vterm = {
        config = ''
          (when persp-mode
            (ngpc/persp-set-vterm-buffer-name))'';
      };
      programs.emacs.init.init.packages.perspective = {
        preface = ''
          (defun ngpc/persp-set-vterm-buffer-name ()
            (setq vterm-buffer-name (concat "*vterm "
                                            (car persp-modestring-dividers)
                                            (persp-name (persp-curr))
                                            (cadr persp-modestring-dividers)
                                            "*")))'';
        hook = {
          persp-activated = [ "ngpc/persp-set-vterm-buffer-name" ];
        };
      };
    })
  ]);
}
