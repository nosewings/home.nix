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
      home.packages = with pkgs; [
        nix-prefetch-git
      ];
      programs.emacs.init.init.packages = {
        nix-mode = {
          enable = true;
        };
        # `nix-update` doesn't have autoloads.
        nix-update = {
          enable = true;
          no-require = false;
        };
      };
    }
    (mkIf config.ngpc.programs.direnv.enable {
      programs.fish.shellAbbrs = {
        nix-direnv-flake-init = "nix flake new -t github:nix-community/nix-direnv";
      };
    })
    (mkIf cfg.lsp.enable {
      home.packages = with pkgs; [ rnix-lsp ];
    })
    (mkIf (cfg.lsp.enable && config.ngpc.programs.emacs.lsp.enable) {
      programs.emacs.init.init.packages = {
        lsp-mode = {
          hook = {
            "nix-mode" = [ "lsp" ];
          };
        };
      };
    })
  ]);
}
