{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs.treemacs;
in
{
  options.ngpc.programs.emacs.treemacs = {
    enable = mkEnableOption "Emacs Treemacs config";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      programs.emacs.init.init.packages = {
        treemacs = {
          enable = true;
        };
      };
    }
    (mkIf config.ngpc.programs.emacs.magit.enable {
      programs.emacs.init.init.packages.treemacs-magit.enable = true;
    })
    (mkIf config.ngpc.programs.emacs.perspective.enable {
      programs.emacs.init.init.packages.treemacs-perspective.enable = true;
    })
    (mkIf config.ngpc.programs.emacs.projectile.enable {
      programs.emacs.init.init.packages.treemacs-projectile.enable = true;
    })
  ]);
}
