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
    ngpc.programs.emacs.alert.enable = true;
    programs.emacs.init.init.packages = {
      circe = {
        enable = true;
        custom = {
          circe-network-options = ''
            '(("Libera Chat"
               :nick "nosewings"
               :tls-keylist (("${config.home.homeDirectory}/.ssh/libera.pem" "${config.home.homeDirectory}/.ssh/libera.pem"))
               :channels ("#agda" "#haskell" "#nixos" "#rust")))
            '';
        };
        hook = {
          circe-mode = [ "ngpc/disable-show-trailing-whitespace" ];
        };
      };
      circe-notifications = mkMerge [
        {
          enable = true;
          hook = {
            circe-server-connected = [ "enable-circe-notifications" ];
          };
        }
        (mkIf pkgs.stdenv.isDarwin {
          custom = {
            circe-notifications-alert-style = "'osx-notify";
          };
        })
      ];
    };
  };
}
