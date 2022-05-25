{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs.lsp;
in
{
  options.ngpc.programs.emacs.lsp = {
    enable = mkEnableOption "Emacs LSP config";
  };

  config = mkMerge [
    {
      programs.emacs.init.init.packages = {
        lsp-mode = {
          enable = true;
          custom = {
            lsp-keymap-prefix = "\"C-c l\"";
          };
        };
        lsp-ui = {
          enable = true;
        };
      };
    }
    (mkIf (config.ngpc.programs.emacs.completion == "ivy") {
      programs.emacs.init.init.packages.lsp-ivy = {
        enable = true;
      };
    })
    (mkIf config.ngpc.programs.emacs.treemacs.enable {
      programs.emacs.init.init.packages = {
        lsp-treemacs = {
          enable = true;
        };
      };
    })
    (mkIf config.ngpc.programs.emacs.which-key.enable {
      programs.emacs.init.init.packages.lsp-mode.hook = {
        lsp-mode = [ "lsp-enable-which-key-integration" ];
      };
    })
  ];
}
