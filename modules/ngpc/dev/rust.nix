{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.dev.rust;
in
{
  options.ngpc.dev.rust = {
    enable = mkEnableOption "Rust config";
    lsp = {
      enable = mkEnableOption "Rust LSP config";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        cargo-expand
        rustup
      ];
      programs.emacs.init.init.packages = {
        rustic = {
          enable = true;
        };
      };
    }
    (mkIf cfg.lsp.enable {
      home.packages = with pkgs; [ rust-analyzer ];
    })
    (mkIf (cfg.lsp.enable && config.ngpc.programs.emacs.lsp.enable) {
      programs.emacs.init.init.packages = {
        lsp-mode = {
          hook = {
            rustic-mode = [ "lsp-deferred" ];
          };
        };
      };
    })
  ]);
}
