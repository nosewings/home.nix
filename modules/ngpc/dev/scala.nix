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
      ngpc.dev.jdk.enable = true;
      home.packages = with pkgs; [
        scala
        sbt
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
        lsp-metals = {
          enable = true;
          hook = {
            scala-mode = [ "lsp-deferred" ];
          };
        };
      };
    })]);
}
