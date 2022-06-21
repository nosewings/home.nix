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
      mkConfiguration =
        { system
        , username
        , homeDirectory
        , stateVersion
        , desktop ? false
        , hardware ? false
        , leisure ? false
        , singleUser
        , ssh ? false
        , xsession ? false
        , agda ? false
        , haskell ? false
        # HTML support costs us essentially nothing.
        , html ? true
        , java ? false
        , javascript ? false
        # Markdown support costs us essentially nothing.
        , markdown ? true
        # We'll always want the ability to configure our system.
        , nix ? true
        , python ? false
        , rust ? false
        # We'll always want the ability to write scripts, so we
        # include shell by default.
        , shell ? true
        , tex ? false
        , web ? false
        # YAML support costs us essentially nothing.
        , yaml ? true
        # XML support costs us essentially nothing.
        , xml ? true
        , config ? { }
        }: home-manager.lib.homeManagerConfiguration {
          inherit system username homeDirectory stateVersion;
          configuration = {
            imports = [ ./home.nix ./platforms/${system}.nix ];
            config = config // {
              nixpkgs.overlays = overlays;
            };
          };
          extraSpecialArgs = {
            inherit system
              desktop hardware leisure singleUser ssh xsession
              agda haskell html java javascript markdown nix python rust shell tex web xml yaml;
          };
        };
    in {
      homeConfigurations.blackstar = mkConfiguration rec {
        system = "x86_64-linux";
        username = "ngpc";
        homeDirectory = "/home/${username}";
        stateVersion = "22.05";
        desktop = true;
        hardware = true;
        ssh = true;
        leisure = true;
        singleUser = false;
        xsession = true;
        agda = true;
        haskell = true;
        java = true;
        python = true;
        rust = true;
        tex = true;
        web = true;
        config = {
          xresources.properties = {
            "Xft.dpi" = 144;
          };
        };
      };

      homeConfigurations.fake-plastic-trees = mkConfiguration rec {
        system = "x86_64-linux";
        username = "ngpc";
        homeDirectory = "/home/${username}";
        stateVersion = "22.05";
        desktop = true;
        hardware = true;
        ssh = true;
        leisure = true;
        singleUser = true;
        xsession = true;
        agda = true;
        haskell = true;
        java = true;
        python = true;
        rust = true;
        tex = true;
        web = true;
      };

      homeConfigurations.no-surprises = mkConfiguration rec {
        system = "aarch64-darwin";
        username = "ngpc";
        homeDirectory = "/Users/${username}";
        stateVersion = "22.05";
        hardware = true;
        ssh = true;
        singleUser = false;
        haskell = true;
        java = true;
        python = true;
        rust = true;
        tex = true;
        web = true;
      };

      homeConfigurations.ec2 = mkConfiguration rec {
        system = "x86_64-linux";
        username = "ec2-user";
        homeDirectory = "/home/${username}";
        stateVersion = "22.05";
        singleUser = false;
        java = true;
        web = true;
      };
    };
}
