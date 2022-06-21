{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.dev.markdown;
in
{
  options.ngpc.dev.markdown = {
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
