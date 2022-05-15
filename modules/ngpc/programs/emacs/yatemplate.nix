{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs.yatemplate;
in
{
  options.ngpc.programs.emacs.yatemplate = {
    enable = mkEnableOption "Emacs YATemplate config";
    templateFiles = mkOption {
      type = with types; attrsOf path;
      default = {};
    };
  };

  config = mkIf cfg.enable {
    ngpc.programs.emacs.autoinsert.enable = true;
    programs.emacs.init.init.packages.yatemplate = {
      enable = true;
      config = ''
        (yas-global-mode)
        (yatemplate-fill-alist)'';
    };
    home.file = flip mapAttrs' cfg.templateFiles (file: source:
       nameValuePair ".config/emacs/templates/${file}" { inherit source; }
    );
  };
}
