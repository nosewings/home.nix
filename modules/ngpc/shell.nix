{ config, lib, pkgs, ... }:
with lib;
with types;
let
  cfg = config.ngpc.shell;
  sentinel = "__NGPC_INTERACTIVE_INIT";
  notBash = sh: mkBefore ''
    if [[ $- == *i* ]] && test -z "''$${sentinel}"; then
        exec ${sh}
    fi
  '';
  notZsh = sh: mkBefore ''
    if test -z "''$${sentinel}" ; then
        exec ${sh}
    fi
  '';
  notFish = sh: mkBefore ''
    if status --is-interactive; and test -z "''$${sentinel}"
        exec ${sh}
    end
  '';
  yesBashOrZsh = "export ${sentinel}=1";
  yesFish = "set -x ${sentinel} 1";
in
{
  options.ngpc.shell = mkOption {
    type = enum [ "bash" "zsh" "fish" ];
  };
  config = mkMerge [
    (mkIf (cfg == "bash") {
      programs.bash.initExtra = yesBashOrZsh;
      programs.zsh.initExtraFirst = notZsh "bash";
      programs.fish.shellInit = notFish "bash";
    })
    (mkIf (cfg == "zsh") {
      programs.bash.bashrcExtra = notBash "zsh";
      programs.zsh.initExtra = yesBashOrZsh;
      programs.fish.shellInit = notFish "zsh";
    })
    (mkIf (cfg == "fish") {
      programs.bash.bashrcExtra = notBash "fish";
      programs.zsh.initExtra = notZsh "fish";
      programs.fish.shellInit = yesFish;
    })
  ];
}
