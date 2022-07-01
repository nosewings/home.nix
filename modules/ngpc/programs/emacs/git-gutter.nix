{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs.git-gutter;
in
{
  options.ngpc.programs.emacs.git-gutter = {
    enable = mkEnableOption "Emacs git-gutter config";
  };
  config = mkIf cfg.enable (mkMerge [
    {
      programs.emacs.init.init.packages.git-gutter = {
        enable = true;
        config = "(global-git-gutter-mode)";
      };
    }
    (mkIf config.ngpc.programs.emacs.magit.enable {
      programs.emacs.init.init.packages.git-gutter = {
        hook = {
          magit-post-refresh = [ "git-gutter:update-all-windows" ];
        };
      };
    })
  ]);
}
