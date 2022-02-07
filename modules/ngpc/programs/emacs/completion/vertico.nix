{ config, lib, ... }:
with lib;
{
  config = mkIf (config.ngpc.programs.emacs.completion == "vertico") {
    programs.emacs.init.init.packages = {
      vertico = {
        enable = true;
        config = "(vertico-mode)";
      };
      marginalia = {
        enable = true;
        config = "(marginalia-mode)";
      };
      consult = {
        enable = true;
      };
      orderless = {
        enable = true;
        custom = {
          completion-styles = "'(orderless)";
          completion-category-defaults = "nil";
        };
      };
    };
  };
}
