{ config, lib, pkgs, system, ... }:
with lib;
let
  cfg = config.ngpc.dev.haskell;
  allHlsGhcVersions = [ "884" "8107" "902" "923" ];
  systemHlsGhcVersions =
    if system == "aarch64-darwin" then
      filter (x: x != "884") allHlsGhcVersions
    else
      allHlsGhcVersions;
in
{
  options.ngpc.dev.haskell = {
    enable = mkEnableOption "Haskell config";
    lsp = {
      enable = mkEnableOption "Haskell LSP config";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        cabal-install
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
      ngpc.programs.emacs.normalModes = mkOptionDefault {
        haskell-cabal-mode = "haskell-mode";
      };
    }
    (mkIf config.ngpc.dev.nix.enable {
      home.packages = with pkgs; [
        cabal2nix
      ];
    })
    (mkIf config.ngpc.programs.emacs.lsp.enable {
      home.packages = with pkgs; [
        (haskell-language-server.override { supportedGhcVersions = systemHlsGhcVersions; })
      ];
      programs.emacs.init.init.packages = {
        haskell-mode = {
          hook = {
            haskell-mode = [ "lsp-deferred" ];
          };
        };
        lsp-haskell = {
          enable = true;
          custom = {
            # Default is "haskell-language-server-wrapper", but that's
            # not what the Nix binary is called.  We don't give an
            # exact path, becaue it's common for Haskell project to
            # come with a Nix shell that provides its own hls.
            lsp-haskell-server-path = "\"haskell-language-server\"";
          };
        };
      };
    }
    )
  ]);
}
