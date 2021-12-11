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
        config = ''
          (counsel-mode)
          (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
        '';
      };
      ivy-rich = {
        enable = true;
        config = "(ivy-rich-mode)";
      };
    };
  };
}
