{ config, lib, ... }:
with lib;
let
  cfg = config.programs.emacs.vterm;

  bashPrintf = {
    initExtra = ''
      vterm_printf() {
          if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ] ); then
              # Tell tmux to pass the escape sequences through
              printf "\ePtmux;\e\e]%s\007\e\\" "$1"
          elif [ "''${TERM%%-*}" = "screen" ]; then
              # GNU screen (screen, screen-256color, screen-256color-bce)
              printf "\eP\e]%s\007\e\\" "$1"
          else
              printf "\e]%s\e\\" "$1"
          fi
      }
    '';
  };

  zshPrintf = bashPrintf;

  fishPrintf = {
    functions.vterm_printf.body = ''
      if begin; [ -n "$TMUX" ]; and string match -q -r "screen|tmux" "$TERM"; end
          # tell tmux to pass the escape sequences through
          printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
      else if string match -q -- "screen*" "$TERM"
          # GNU screen (screen, screen-256color, screen-256color-bce)
          printf "\eP\e]%s\007\e\\" "$argv"
      else
          printf "\e]%s\e\\" "$argv"
      end
    '';
  };

  bashClearScrollback = {
    initExtra = ''
      if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
          function clear() {
              vterm_printf "51;Evterm-clear-scrollback";
              tput clear;
          }
      fi
    '';
  };

  zshClearScrollback = {
    initExtra = ''
      if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
          alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
      fi
    '';
  };

  fishClearScrollback = {
    interactiveShellInit = ''
      if [ "$INSIDE_EMACS" = 'vterm' ]
          function clear
              vterm_printf "51;Evterm-clear-scrollback";
              tput clear;
          end
      end
    '';
  };

  bashTracking = {
    initExtra = mkAfter ''
      vterm_prompt_end() {
          vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
      }
      vterm_set_prompt_end() {
          if ! [[ "$PS1" =~ vterm_prompt_end ]]; then
              PS1=$PS1'\[$(vterm_prompt_end)\]'
          fi
      }
      PROMPT_COMMAND="''${PROMPT_COMMAND:+$PROMPT_COMMAND;}vterm_set_prompt_end"
    '';
  };

  zshTracking = {
    initExtra = mkAfter ''
      vterm_prompt_end() {
          vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
      }
      setopt PROMPT_SUBST
      PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
    '';
  };

  fishTracking = {
    functions.vterm_prompt_end.body = "vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)";
    interactiveShellInit = mkAfter ''
      functions --copy fish_prompt vterm_old_fish_prompt
      function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
          # Remove the trailing newline from the original prompt. This is done
          # using the string builtin from fish, but to make sure any escape codes
          # are correctly interpreted, use %b for printf.
          printf "%b" (string join "\n" (vterm_old_fish_prompt))
          vterm_prompt_end
      end
    '';
  };

  bashMessagePassing = {
    initExtra = mkAfter ''
      vterm_cmd() {
        local vterm_elisp
        vterm_elisp=""
        while [ $# -gt 0 ]; do
            vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
            shift
        done
        vterm_printf "51;E$vterm_elisp"
    }
    '';
  };

  zshMessagePassing = bashMessagePassing;

  fishMessagePassing = {
    functions.vterm_cmd = {
      description = "Run an Emacs command among the ones been defined in vterm-eval-cmds.";
      body = ''
        set -l vterm_elisp ()
        for arg in $argv
            set -a vterm_elisp (printf '"%s" ' (string replace -a -r '([\\\\"])' '\\\\\\\\$1' $arg))
        end
        vterm_printf '51;E'(string join ''\'''\' $vterm_elisp)
      '';
    };
  };
in {
  options.programs.emacs.vterm = {
    enable = mkEnableOption "Emacs vterm integration";

    enableBashIntegration = mkOption {
      default = true;
      type = types.bool;
      description = ''
        Whether to enable Bash integration.
      '';
    };

    enableZshIntegration = mkOption {
      default = true;
      type = types.bool;
      description = ''
        Whether to enable Zsh integration.
      '';
    };

    enableFishIntegration = mkOption {
      default = true;
      type = types.bool;
      description = ''
        Whether to enable Fish integration.
      '';
    };

    enableClearScrollback = mkOption {
      default = true;
      type = types.bool;
    };

    enableTracking = mkOption {
      default = true;
      type = types.bool;
    };

    enableMessagePassing = mkOption {
      default = true;
      type = types.bool;
    };
  };

  config = mkIf cfg.enable {
    programs.bash = mkIf cfg.enableBashIntegration (mkMerge [
      bashPrintf
      (mkIf cfg.enableClearScrollback bashClearScrollback)
      (mkIf cfg.enableTracking bashTracking)
      (mkIf cfg.enableMessagePassing bashMessagePassing)
    ]);
    programs.zsh = mkIf cfg.enableZshIntegration (mkMerge [
      zshPrintf
      (mkIf cfg.enableClearScrollback zshClearScrollback)
      (mkIf cfg.enableTracking zshTracking)
      (mkIf cfg.enableMessagePassing zshMessagePassing)
    ]);
    programs.fish = mkIf cfg.enableFishIntegration (mkMerge [
      fishPrintf
      (mkIf cfg.enableClearScrollback fishClearScrollback)
      (mkIf cfg.enableTracking fishTracking)
      (mkIf cfg.enableMessagePassing fishMessagePassing)
    ]);
  };
}
