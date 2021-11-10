with builtins;
{ lib, pkgs, ... }:
with lib;
mkIf (currentSystem == "aarch64-darwin") {
  home.sessionVariables = {
    EDITOR = "emacs -nw";
    VISUAL = "emacs";
  };
}
