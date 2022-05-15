{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs.autoinsert;
in
{
  options.ngpc.programs.emacs.autoinsert = {
    enable = mkEnableOption "Emacs autoinsert config";
  };

  config = mkIf cfg.enable {
    programs.emacs.init.init.packages.autoinsert = {
      enable = true;
      package = null;
      config = "(auto-insert-mode)";
      custom = {
        auto-insert-query = "nil";
      };
    };
  };
}
