{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs.yasnippet;
in
{
  options.ngpc.programs.emacs.yasnippet = {
    enable = mkEnableOption "Emacs YASnippet config";
  };

  config = mkIf cfg.enable {
    programs.emacs.init.init.packages = {
      yasnippet = {
        enable = true;
        config = "(yas-global-mode)";
      };
      yasnippet-snippets = {
        enable = true;
        loadAtBuild = false;
      };
    };
  };
}
