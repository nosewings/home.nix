{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.dev.javascript;
in
{
  options.ngpc.dev.javascript = {
    enable = mkEnableOption "Javascript config";
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; flatten [
      nodejs
      (with nodePackages; [
        typescript
        typescript-language-server
      ])
    ];
    programs.git.ignores = [
      "node_modules/"
    ];
    programs.emacs.init.init.packages = {
      js2-mode = {
        enable = true;
        mode = {
          "\\\\.js\\\\'" = null;
        };
      };
      typescript-mode = {
        enable = true;
      };
      lsp-mode = {
        hook = {
          js2-mode = [ "lsp-deferred" ];
          typescript-mode = [ "lsp-deferred" ];
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
