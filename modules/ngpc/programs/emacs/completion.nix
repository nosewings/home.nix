{ config, lib, ... }:
with lib;
with types;
{
  options.ngpc.programs.emacs = {
    completion = mkOption {
      type = enum [ null "ivy" "selectrum" "vertico" ];
      default = null;
    };
  };
}
