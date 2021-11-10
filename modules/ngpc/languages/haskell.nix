{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.languages.haskell;
in
{
  options.ngpc.languages.haskell = {
    enable = mkEnableOption "Haskell config";
    lsp = {
      enable = mkEnableOption "Haskell LSP config";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [ cabal2nix ];
      programs.emacs.init.init.packages = {
        haskell-mode = {
          enable = true;
        };
      };
    }
    (mkIf config.ngpc.emacs.lsp.enable {
      programs.emacs.init.init.packages = {
        haskell-mode = {
          hook = {
            "" = [ "lsp" ];
          };
        };
        lsp-haskell = {
          enable = true;
        };
      };
    })
  ]);
}
