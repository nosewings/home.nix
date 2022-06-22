{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.dev.scala;
in
{
  options.ngpc.dev.scala = {
    enable = mkEnableOption "Scala config";
    lsp.enable = mkEnableOption "Scala LSP config";
  };
  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        scala
      ];
      programs.emacs.init.init.packages = {
        scala-mode = {
          enable = true;
        };
      };
    }
    (mkIf cfg.lsp.enable {
      home.packages = with pkgs; [
        metals
      ];
      programs.emacs.init.init.packages = {
        lsp-mode = {
          hook = {
            scala-mode = [ "lsp-deferred" ];
          };
        };
      };
    })]);
}
