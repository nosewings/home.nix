{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs.which-key;
in
{
  options.ngpc.programs.emacs.which-key = {
    enable = mkEnableOption "Emacs which-key config";
  };

  config = mkIf cfg.enable {
    programs.emacs.init.init.packages.which-key = {
      enable = true;
      config = "(which-key-mode)";
    };
  };
}
