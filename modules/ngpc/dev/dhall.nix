{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.dev.dhall;
in
{
  options.ngpc.dev.dhall = {
    enable = mkEnableOption "Dhall config";
    lsp.enable = mkEnableOption "Dhall LSP config";
  };
  config = mkIf cfg.enable (mkMerge [
    {
    home.packages = with pkgs; [
      dhall
    ];
    programs.emacs.init.init.packages = {
      dhall-mode = {
        enable = true;
      };
    };
    }
    (mkIf cfg.lsp.enable {
      home.packages = with pkgs; [
        dhall-lsp-server
      ];
      programs.emacs.init.init.packages = {
        lsp-mode = {
          hook = {
            purescript-mode = [ "lsp-deferred" ];
          };
        };
      };
    })
  ]);
}
