{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.direnv;
in
{
  options.ngpc.programs.direnv = {
    enable = mkEnableOption "Direnv config";
  };

  config = {
    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    programs.emacs.init.init.packages = {
      direnv = {
        enable = true;
        config = "(direnv-mode)";
        hook = {
          find-file = [ "ngpc/direnv-update-environment-if-not-remote" ];
        };
        preface = ''
          (defun ngpc/direnv-update-environment-if-not-remote ()
            (unless (file-remote-p (buffer-file-name (current-buffer)))
              (direnv-update-environment)))'';
      };
    };
  };
}
