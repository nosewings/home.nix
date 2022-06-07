{ config, lib, ... }:
with lib;
with types;
{
  options.ngpc.programs.emacs.perspectives = mkOption {
    type = enum [ null "persp-mode" "perspective" ];
    default = null;
  };
}
