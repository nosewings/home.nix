{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.dev.racket;
in
{
  options.ngpc.dev.racket = {
    enable = mkEnableOption "Racket config";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      racket
    ];
    programs.emacs.init.init.packages = {
      racket-mode = {
        enable = true;
      };
    };
  };
}
