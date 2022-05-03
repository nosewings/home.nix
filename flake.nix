{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { nixpkgs, home-manager, emacs-overlay, ... }:
    let
      overlays = [
        (super: self: {
          xterm-24bit = self.stdenv.mkDerivation {
            pname = "xterm-24bit";
            version = "0.1";
            src = self.fetchFromGitHub {
              owner = "nosewings";
              repo = "xterm-24bit";
              rev = "7de6c0f85f35bc67bcb97f4926c8ebee1efece4d";
              sha256 = "115x65gwh5yqka7blcdljf5dk8n62wz69nx7qjxm7sm4r8np1lkb";
            };
            buildInputs = [ self.ncurses ];
            phases = [ "unpackPhase" "installPhase" ];
            installPhase = ''
              mkdir -p $out/share/terminfo
              tic -o $out/share/terminfo -x xterm-24bit.terminfo
            '';
          };
          FatBoy = self.stdenv.mkDerivation {
            pname = "FatBoy";
            version = "0.790";
            src = self.fetchurl {
              url = "https://dl.fatboy.site/FatBoy-latest.7z";
              sha256 = "330aad903459a8363f822ee5ac1c24beb01321196baca2bbee0c5fbfda0c6870";
            };
            buildInputs = [ self.p7zip ];
            phases = [ "unpackPhase" "installPhase" ];
            unpackPhase = "7z e $src";
            installPhase = ''
              mkdir -p $out/share/soundfonts
              mv FatBoy-v0.790.sf2 $out/share/soundfonts/FatBoy.sf2
            '';
            meta = {
              description = "A free GM/GS SoundFont for classic video game MIDI, emulation, and general usage.";
              homepage = "https://fatboy.site/";
            };
          };
        })
        emacs-overlay.overlay
      ];
      mkConfiguration = args: home-manager.lib.homeManagerConfiguration rec {
        inherit (args) system username homeDirectory stateVersion;
        configuration = {
          imports = [ ./home.nix ./platforms/${system}.nix ];
          nixpkgs.overlays = overlays;
        } // (args.configuration or {});
      };
    in {
      homeConfigurations.blackstar = mkConfiguration rec {
        system = "x86_64-linux";
        username = "ngpc";
        homeDirectory = "/home/${username}";
        stateVersion = "22.05";
        configuration = {
          xresources.properties = {
            "Xft.dpi" = 144;
          };
        };
      };

      homeConfigurations.no-surprises = mkConfiguration rec {
        system = "aarch64-darwin";
        username = "ngpc";
        homeDirectory = "/Users/${username}";
        stateVersion = "22.05";
      };
    };
}
