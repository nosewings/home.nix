{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.dev.jdk;
in
{
  options.ngpc.dev.jdk = {
    enable = mkEnableOption "JVM config";
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      jdk
    ];
    programs.git.ignores = [
      "*.class"
      "*.jar"
    ];
  };
}
