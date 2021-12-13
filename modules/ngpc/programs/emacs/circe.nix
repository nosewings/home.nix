{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs.circe;
in
{
  options.ngpc.programs.emacs.circe = {
    enable = mkEnableOption "Emacs Circe config";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ gnutls ];
    programs.emacs.init.init.packages = {
      circe = {
        enable = true;
        custom = {
          circe-network-options = ''
            '(("Libera Chat"
               :nick "nosewings"
               :tls-keylist (("${config.home.homeDirectory}/.ssh/libera.pem" "${config.home.homeDirectory}/.ssh/libera.pem"))
               :channels ("#agda" "#haskell" "#nixos")))
            '';
        };
      circe-notifications = {
        enable = true;
        hook = {
          circe-connected-hook = [ "enable-circe-notifications" ];
        };
      };
    };
  };
}
