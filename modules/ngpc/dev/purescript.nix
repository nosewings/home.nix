{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.dev.purescript;
in
{
  options.ngpc.dev.purescript = {
    enable = mkEnableOption "Purescript config";
    lsp = {
      enable = mkEnableOption "Purescript LSP config";
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      ngpc.dev.dhall.enable = true;
      home.packages = with pkgs; [
        purescript
        spago
      ];
      programs.emacs.init.init.packages = {
        purescript-mode = {
          enable = true;
        };
      };
    }
    (mkIf cfg.lsp.enable {
      ngpc.dev.dhall.lsp.enable = true;
      home.packages = with pkgs.nodePackages; [
        purescript-language-server
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
