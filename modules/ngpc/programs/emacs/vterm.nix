{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs.vterm;
in
{
  options.ngpc.programs.emacs.vterm = {
    enable = mkEnableOption "Emacs vterm config";
  };

  config = mkIf cfg.enable {
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
  };
}
