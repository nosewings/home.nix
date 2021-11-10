self: super:
with super;
{
  FatBoy = stdenv.mkDerivation {
    pname = "FatBoy";
    version = "0.790";
    src = fetchurl {
      url = "https://dl.fatboy.site/FatBoy-latest.7z";
      sha256 = "330aad903459a8363f822ee5ac1c24beb01321196baca2bbee0c5fbfda0c6870";
    };
    buildInputs = [ p7zip ];
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
}
