{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.languages.markdown;
in
{
  options.ngpc.languages.markdown = {
    enable = mkEnableOption "Markdown config";
  };

  config = mkIf cfg.enable {
    programs.emacs.init.init.packages = {
      poly-markdown = {
        enable = true;
      };
    };
  };
}
