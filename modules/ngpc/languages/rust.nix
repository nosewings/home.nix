{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.languages.rust;
in
{
  options.ngpc.languages.rust = {
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
        rustic = {
          hook = {
            "rustic-mode" = [ "lsp" ];
          };
        };
      };
    })
  ]);
}
