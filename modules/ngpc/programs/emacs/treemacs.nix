{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs.treemacs;
in
{
  options.ngpc.programs.emacs.treemacs = {
    enable = mkEnableOption "Emacs Treemacs config";
  };

  config = mkIf cfg.enable {
    programs.emacs.init.init.packages = {
      treemacs = {
        enable = true;
      };
    };
  };
}
