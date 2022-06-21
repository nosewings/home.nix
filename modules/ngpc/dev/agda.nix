{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.dev.agda;
in
{
  options.ngpc.dev.agda = {
    enable = mkEnableOption "Agda config";
  };

  config = mkIf cfg.enable {
    programs.emacs.init.init.packages = {
      agda-input = {
        enable = true;
        no-require = false;
      };
      agda2-mode = {
        enable = true;
        mode = {
          "(\\\\.agda)|(\\\\.lagda(\\\\.(tex|rst|md|org))?)\\\\'" = null;
        };
      };
    };
  };
}
