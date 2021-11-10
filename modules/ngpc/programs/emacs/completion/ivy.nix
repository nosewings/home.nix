{ config, lib, ... }:
with lib;
{
  config = mkIf (config.ngpc.programs.emacs.completion == "ivy") {
    programs.emacs.init.init.packages = {
      ivy = {
        enable = true;
        config = "(ivy-mode)";
      };
      counsel = {
        enable = true;
        config = "(counsel-mode)";
      };
    };
  };
}
