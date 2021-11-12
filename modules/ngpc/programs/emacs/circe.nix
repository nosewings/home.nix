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
               :tls-keylist (("${home.homeDirectory}/.ssh/libera.pem" "${home.homeDirectory}/.ssh/libera.pem"))
               :channels ("#agda" "#haskell" "#nixos")))
            '';
        };
      };
    };
  };
}
