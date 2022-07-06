{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.dev.julia;
in
{
  options.ngpc.dev.julia = {
    enable = mkEnableOption "Julia config";
    lsp = {
      enable = mkEnableOption "Julia LSP config";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        julia-bin
      ];
      programs.emacs.init.init.packages = {
        julia-mode = {
          enable = true;
        };
      };
    }
    (mkIf cfg.lsp.enable {
      programs.emacs.init.init.packages = {
        lsp-julia = {
          enable = true;
          hook = {
            julia-mode = [ "lsp-deferred" ];
          };
        };
      };
    })
  ]);
}
