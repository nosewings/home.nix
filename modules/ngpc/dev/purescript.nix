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
      home.packages = with pkgs; [
        purescript
        spago
      ];
    }
    (mkIf cfg.lsp.enable {
      home.packages = with pkgs.nodePackages; [
        purescript-language-server
      ];
      programs.emacs.init.init.packages = {
        purescript-mode = {
          enable = true;
        };
      };
    })
  ]);
}
