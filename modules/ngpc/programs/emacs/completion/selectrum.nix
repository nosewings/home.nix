{ config, lib, ... }:
with lib;
{
  config = mkIf (config.ngpc.programs.emacs.completion == "selectrum") {
    programs.emacs.init.init.packages = {
      selectrum = {
        enable = true;
        config = "(selectrum-mode)";
      };
      marginalia = {
        enable = true;
        config = "(marginalia-mode)";
      };
      consult = {
        enable = true;
      };
      selectrum-prescient = {
        enable = true;
        config = ''
          (selectrum-prescient-mode)
          (prescient-persist-mode)'';
      };
    };
  };
}
