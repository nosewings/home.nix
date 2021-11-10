self: super:
with super;
{
  xterm-24bit = stdenv.mkDerivation {
    pname = "xterm-24bit";
    version = "0.1";
    src = fetchFromGitHub {
      owner = "nosewings";
      repo = "xterm-24bit";
      rev = "7de6c0f85f35bc67bcb97f4926c8ebee1efece4d";
      sha256 = "115x65gwh5yqka7blcdljf5dk8n62wz69nx7qjxm7sm4r8np1lkb";
    };
    buildInputs = [ ncurses ];
    phases = [ "unpackPhase" "installPhase" ];
    installPhase = ''
      mkdir -p $out/share/terminfo
      tic -o $out/share/terminfo -x xterm-24bit.terminfo
    '';
  };
}
