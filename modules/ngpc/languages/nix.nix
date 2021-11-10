{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.languages.nix;
in
{
  options.ngpc.languages.nix = {
    enable = mkEnableOption "Nix config";
    lsp = {
      enable = mkEnableOption "Nix LSP config";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      programs.emacs.init.init.packages = {
        nix-mode = {
          enable = true;
        };
      };
    }
    (mkIf cfg.lsp.enable {
      home.packages = with pkgs; [ rnix-lsp ];
    })
    (mkIf (cfg.lsp.enable && config.ngpc.programs.emacs.lsp.enable) {
      programs.emacs.init.init.packages = {
        nix-mode = {
          hook = {
            "" = [ "lsp" ];
          };
        };
      };
    })
  ]);
}
