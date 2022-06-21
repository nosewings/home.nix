{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.dev.nix;
in
{
  options.ngpc.dev.nix = {
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
      programs.emacs.overrides = self: super: {
        nix-update = self.trivialBuild {
          pname = "nix-update";
          version = "1.0";
          src = pkgs.fetchFromGitHub {
            owner = "nosewings";
            repo = "nix-update-el";
            rev = "055cb9a5f887d63299020159fee73df9a231b2e4";
            sha256 = "057kcwh1c3wd12jk42r1bnn652cm2h8ml1zc1gsn58liwf82qky1";
          };
        };
      };
      programs.emacs.init.init.packages = {
        nix-mode = {
          enable = true;
        };
        nix-update = {
          enable = true;
          commands = [ "nix-update-fetch" ];
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
            "nix-mode" = [ "lsp-deferred" ];
          };
        };
      };
    })
    (mkIf config.ngpc.programs.direnv.enable {
      programs.fish.shellAbbrs = {
        nix-direnv-flake-init = "nix flake new -t github:nix-community/nix-direnv";
      };
    })
  ]);
}
