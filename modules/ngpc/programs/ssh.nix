with builtins;
{ config, lib, ... }:
with lib;
let
  cfg = config.ngpc.programs.ssh;
in
{
  options.ngpc.programs.ssh = {
    enable = mkEnableOption "SSH config";
  };

  config = mkIf cfg.enable {
    programs.ssh.enable = true;
    home.activation = {
      "ssh.authorized_keys" = hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD eval "curl $VERBOSE_ARG 'https://github.com/nosewings.keys' > '${config.home.homeDirectory}/.ssh/authorized_keys'"
      '';
    };
  };
}
