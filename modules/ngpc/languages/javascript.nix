{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.languages.javascript;
in
{
  options.ngpc.languages.javascript = {
    enable = mkEnableOption "Javascript config";
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      nodejs
    ];
    programs.git.ignores = [
      "node_modules/"
    ];
    programs.emacs.init.init.packages = {
      js2-mode = {
        enable = true;
      };
      typescript-mode = {
        enable = true;
      };
      lsp-mode = {
        hook = {
          js2-mode = [ "lsp" ];
          typescript-mode = [ "lsp" ];
        };
      };
    };
    ngpc.programs.emacs = {
      yatemplate.templateFiles = {
        ":.*.js" = ./templates/template.js;
        ":.*.ts" = ./templates/template.ts;
      };
    };
  };
}
