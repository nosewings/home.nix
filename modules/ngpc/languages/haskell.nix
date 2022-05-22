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
      home.packages = with pkgs; [
        cabal-install
        cabal2nix
        ghc
      ];
      programs.git.ignores = [
        "dist/"
        "dist-newstyle/"
      ];
      programs.emacs.init.init.packages = {
        haskell-mode = {
          enable = true;
        };
      };
    }
    (mkIf config.ngpc.programs.emacs.lsp.enable {
      home.packages = with pkgs; [
        haskell-language-server
      ];
      programs.emacs.init.init.packages = {
        haskell-mode = {
          hook = {
            haskell-mode = [ "lsp" ];
          };
        };
        lsp-haskell = {
          enable = true;
        };
      };
    })
  ]);
}
