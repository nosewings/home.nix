{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.languages.java;
in
{
  options.ngpc.languages.java = {
    enable = mkEnableOption "Java config";
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      openjdk11
      maven
    ];
    programs.git.ignores = [
      "*.class"
      "*.jar"
    ];
    programs.emacs.init.init.packages = {
      lsp-java = {
        enable = true;
        hook = {
          java-mode = [ "lsp" ];
        };
      };
    };
    ngpc.programs.emacs.yatemplate.templateFiles = {
      ":.*.java" = ./templates/template.java;
    };
  };
}
